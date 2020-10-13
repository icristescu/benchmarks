open Lwt.Infix

let ( ++ ) = Int64.add

type t = {
  fd : Unix.file_descr;
  mutable cursor : int64;
  lwt_fd : Lwt_unix.file_descr;
}

let v fd =
  let lwt_fd = Lwt_unix.of_unix_file_descr ~blocking:false fd in
  { fd; cursor = 0L; lwt_fd }

module type IO = sig
  val write :
    fd:Unix.file_descr ->
    fd_offset:int64 ->
    buffer:bytes ->
    buffer_offset:int ->
    length:int ->
    int

  val read :
    fd:Unix.file_descr ->
    fd_offset:int64 ->
    buffer:bytes ->
    buffer_offset:int ->
    length:int ->
    int

  val lseek : bool
end

module IO_Unix : IO = struct
  let write ~fd ~fd_offset:_ ~buffer ~buffer_offset ~length =
    Unix.write fd buffer buffer_offset length

  let read ~fd ~fd_offset:_ ~buffer ~buffer_offset ~length =
    Unix.read fd buffer buffer_offset length

  let lseek = true
end

module IO_Index : IO = struct
  open Index_unix.Syscalls

  let write = pwrite

  let read = pread

  let lseek = false
end

module Raw (I : IO) = struct
  let really_write fd buffer =
    let rec aux buffer_offset length =
      let w = I.write ~fd ~fd_offset:0L ~buffer ~buffer_offset ~length in
      if w = 0 || w = length then ()
      else (aux [@tailcall]) (buffer_offset + w) (length - w)
    in
    (aux [@tailcall]) 0 (Bytes.length buffer)

  let really_read fd length buffer =
    let rec aux buffer_offset length =
      let r = I.read ~fd ~fd_offset:0L ~buffer ~buffer_offset ~length in
      if r = 0 then buffer_offset (* end of file *)
      else if r = length then buffer_offset + r
      else (aux [@tailcall]) (buffer_offset + r) (length - r)
    in
    (aux [@tailcall]) 0 length

  let lseek t off =
    if off = t.cursor then ()
    else
      let _ = Unix.LargeFile.lseek t.fd off Unix.SEEK_SET in
      t.cursor <- off

  let unsafe_write t ~off buf =
    if I.lseek then lseek t off;
    let buf = Bytes.unsafe_of_string buf in
    really_write t.fd buf;
    t.cursor <- off ++ Int64.of_int (Bytes.length buf);
    t.cursor

  let unsafe_read t ~off ~len buf =
    if I.lseek then lseek t off;
    let n = really_read t.fd len buf in
    t.cursor <- off ++ Int64.of_int n;
    n
end

module type Lwt_IO = sig
  val write :
    lwt_fd:Lwt_unix.file_descr ->
    fd:Unix.file_descr ->
    fd_offset:int64 ->
    buffer:bytes ->
    buffer_offset:int ->
    length:int ->
    int Lwt.t

  val read :
    lwt_fd:Lwt_unix.file_descr ->
    fd:Unix.file_descr ->
    fd_offset:int64 ->
    buffer:bytes ->
    buffer_offset:int ->
    length:int ->
    int Lwt.t

  val lseek : bool
end

module Lwt_Unix : Lwt_IO = struct
  let write ~lwt_fd ~fd:_ ~fd_offset:_ ~buffer ~buffer_offset ~length =
    Lwt_unix.write lwt_fd buffer buffer_offset length

  let read ~lwt_fd ~fd:_ ~fd_offset:_ ~buffer ~buffer_offset ~length =
    Lwt_unix.read lwt_fd buffer buffer_offset length

  let lseek = true
end

module Wrap_Index_IO : Lwt_IO = struct
  open Index_unix.Syscalls

  let write ~lwt_fd ~fd ~fd_offset ~buffer ~buffer_offset ~length =
    Lwt_unix.wrap_syscall Lwt_unix.Write lwt_fd (fun () ->
        pwrite ~fd ~fd_offset ~buffer ~buffer_offset ~length)

  let read ~lwt_fd ~fd ~fd_offset ~buffer ~buffer_offset ~length =
    Lwt_unix.wrap_syscall Lwt_unix.Read lwt_fd (fun () ->
        pread ~fd ~fd_offset ~buffer ~buffer_offset ~length)

  let lseek = false
end

module Lwt_raw (I : Lwt_IO) = struct
  let really_write lwt_fd fd buffer =
    let rec aux buffer_offset length =
      I.write ~lwt_fd ~fd ~fd_offset:0L ~buffer ~buffer_offset ~length
      >>= fun w ->
      if w = 0 || w = length then Lwt.return_unit
      else (aux [@tailcall]) (buffer_offset + w) (length - w)
    in
    (aux [@tailcall]) 0 (Bytes.length buffer)

  let really_read lwt_fd fd length buffer =
    let rec aux buffer_offset length =
      I.read ~lwt_fd ~fd ~fd_offset:0L ~buffer ~buffer_offset ~length
      >>= fun r ->
      if r = 0 then Lwt.return buffer_offset (* end of file *)
      else if r = length then Lwt.return (buffer_offset + r)
      else (aux [@tailcall]) (buffer_offset + r) (length - r)
    in
    (aux [@tailcall]) 0 length

  let lseek t off =
    if off = t.cursor then ()
    else
      let _ = Unix.LargeFile.lseek t.fd off Unix.SEEK_SET in
      t.cursor <- off

  let unsafe_write t ~off buf =
    if I.lseek then lseek t off;
    let buf = Bytes.unsafe_of_string buf in
    really_write t.lwt_fd t.fd buf >|= fun () ->
    t.cursor <- off ++ Int64.of_int (Bytes.length buf);
    t.cursor

  let unsafe_read t ~off ~len buf =
    if I.lseek then lseek t off;
    really_read t.lwt_fd t.fd len buf >|= fun n ->
    t.cursor <- off ++ Int64.of_int n;
    n
end

type s = { file : string; mutable raw : t }

let protect_unix_exn = function
  | Unix.Unix_error _ as e -> failwith (Printexc.to_string e)
  | e -> raise e

let ignore_enoent = function
  | Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | e -> raise e

let protect f x = try f x with e -> protect_unix_exn e

let safe f x = try f x with e -> ignore_enoent e

let mkdir dirname =
  let rec aux dir k =
    if Sys.file_exists dir && Sys.is_directory dir then k ()
    else (
      if Sys.file_exists dir then safe Unix.unlink dir;
      (aux [@tailcall]) (Filename.dirname dir) @@ fun () ->
      protect (Unix.mkdir dir) 0o755;
      k () )
  in
  aux dirname (fun () -> ())

let openfile file =
  let mode = Unix.O_RDWR in
  mkdir (Filename.dirname file);
  match Sys.file_exists file with
  | false ->
      let x = Unix.openfile file Unix.[ O_CREAT; mode; O_CLOEXEC ] 0o644 in
      let raw = v x in
      { file; raw }
  | true ->
      let x = Unix.openfile file Unix.[ O_EXCL; mode; O_CLOEXEC ] 0o644 in
      let raw = v x in
      { file; raw }

let close t = Unix.close t.raw.fd

let nb_elements = 10_000_000

let element_size = 1_000

let start_off = 200_000_000L

let random_char () = char_of_int (Random.int 256)

let random_string () = String.init element_size (fun _i -> random_char ())

module type Bench_IO = sig
  val read : s -> off:int64 -> bytes -> int

  val write : s -> off:int64 -> string -> int64

  val name : string
end

module Bench_unix : Bench_IO = struct
  open Raw (IO_Unix)

  let read t ~off buf = unsafe_read t.raw ~off ~len:(Bytes.length buf) buf

  let write t ~off buf = unsafe_write t.raw ~off buf

  let name = "io_unix"
end

module Bench_index : Bench_IO = struct
  open Raw (IO_Index)

  let read t ~off buf = unsafe_read t.raw ~off ~len:(Bytes.length buf) buf

  let write t ~off buf = unsafe_write t.raw ~off buf

  let name = "io_index"
end

module Bench (B : Bench_IO) = struct
  let write t =
    let rec aux off i =
      if i = nb_elements then ()
      else
        let random = random_string () in
        let off = B.write t ~off random in
        aux off (i + 1)
    in
    aux start_off 0

  let read t =
    let rec aux off i =
      if i = nb_elements then ()
      else
        let buf = Bytes.create element_size in
        let n = B.read t ~off buf in
        aux (off ++ Int64.of_int n) (i + 1)
    in
    aux start_off 0

  let with_timer f =
    let t0 = Sys.time () in
    let _ = f () in
    Sys.time () -. t0

  let bench () =
    let t = openfile "store.pack" in
    let write_time = with_timer (fun () -> write t) in
    let read_time = with_timer (fun () -> read t) in
    Fmt.epr "[%s] write = %f, read = %f\n" B.name write_time read_time;
    close t
end

module type Bench_Lwt = sig
  val read : s -> off:int64 -> bytes -> int Lwt.t

  val write : s -> off:int64 -> string -> int64 Lwt.t

  val name : string
end

module Bench_lwt_unix : Bench_Lwt = struct
  open Lwt_raw (Lwt_Unix)

  let read t ~off buf = unsafe_read t.raw ~off ~len:(Bytes.length buf) buf

  let write t ~off buf = unsafe_write t.raw ~off buf

  let name = "lwt_io_unix"
end

module Bench_wrap_index : Bench_Lwt = struct
  open Lwt_raw (Wrap_Index_IO)

  let read t ~off buf = unsafe_read t.raw ~off ~len:(Bytes.length buf) buf

  let write t ~off buf = unsafe_write t.raw ~off buf

  let name = "lwt_io_index"
end

module Lwt_bench (B : Bench_Lwt) = struct
  let write t =
    let rec aux off i =
      if i = nb_elements then Lwt.return_unit
      else
        let random = random_string () in
        B.write t ~off random >>= fun off -> aux off (i + 1)
    in
    aux start_off 0

  let read t =
    let rec aux off i =
      if i = nb_elements then Lwt.return_unit
      else
        let buf = Bytes.create element_size in
        B.read t ~off buf >>= fun n -> aux (off ++ Int64.of_int n) (i + 1)
    in
    aux start_off 0

  let with_timer f =
    let t0 = Sys.time () in
    f () >|= fun () -> Sys.time () -. t0

  let bench () =
    let t = openfile "store.pack" in
    with_timer (fun () -> write t) >>= fun write_time ->
    with_timer (fun () -> read t) >|= fun read_time ->
    Fmt.epr "[%s] write = %f, read = %f\n" B.name write_time read_time;
    close t
end

module B1 = Bench (Bench_unix)
module B2 = Bench (Bench_index)
module B3 = Lwt_bench (Bench_lwt_unix)
module B4 = Lwt_bench (Bench_wrap_index)

let () =
  Fmt.epr "nb_elements = %d; element_size = %d\n" nb_elements element_size;
  B1.bench ();
  B2.bench ();
  Lwt_main.run (B3.bench () >>= B4.bench)
