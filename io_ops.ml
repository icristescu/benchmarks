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

module Raw = struct
  let really_write fd buf =
    let rec aux off len =
      let w = Unix.write fd buf off len in
      if w = 0 || w = len then () else (aux [@tailcall]) (off + w) (len - w)
    in
    (aux [@tailcall]) 0 (Bytes.length buf)

  let really_read fd len buf =
    let rec aux off len =
      let r = Unix.read fd buf off len in
      if r = 0 then off (* end of file *)
      else if r = len then off + r
      else (aux [@tailcall]) (off + r) (len - r)
    in
    (aux [@tailcall]) 0 len

  let lseek t off =
    if off = t.cursor then ()
    else
      let _ = Unix.LargeFile.lseek t.fd off Unix.SEEK_SET in
      t.cursor <- off

  let unsafe_write t ~off buf =
    lseek t off;
    let buf = Bytes.unsafe_of_string buf in
    really_write t.fd buf;
    t.cursor <- off ++ Int64.of_int (Bytes.length buf);
    t.cursor

  let unsafe_read t ~off ~len buf =
    lseek t off;
    let n = really_read t.fd len buf in
    t.cursor <- off ++ Int64.of_int n;
    n
end

module Wrap_lwt_raw = struct
  let really_write lwt_fd fd buf =
    let rec aux off len =
      Lwt_unix.wrap_syscall Lwt_unix.Write lwt_fd (fun () ->
          Unix.write fd buf off len)
      >>= fun w ->
      if w = 0 || w = len then Lwt.return_unit
      else (aux [@tailcall]) (off + w) (len - w)
    in
    (aux [@tailcall]) 0 (Bytes.length buf)

  let really_read lwt_fd fd len buf =
    let rec aux off len =
      Lwt_unix.wrap_syscall Lwt_unix.Write lwt_fd (fun () ->
          Unix.read fd buf off len)
      >>= fun r ->
      if r = 0 then Lwt.return off (* end of file *)
      else if r = len then Lwt.return (off + r)
      else (aux [@tailcall]) (off + r) (len - r)
    in
    (aux [@tailcall]) 0 len

  let lseek t off =
    if off = t.cursor then ()
    else
      let _ = Unix.LargeFile.lseek t.fd off Unix.SEEK_SET in
      t.cursor <- off

  let unsafe_write t ~off buf =
    lseek t off;
    let buf = Bytes.unsafe_of_string buf in
    really_write t.lwt_fd t.fd buf >|= fun () ->
    t.cursor <- off ++ Int64.of_int (Bytes.length buf);
    t.cursor

  let unsafe_read t ~off ~len buf =
    lseek t off;
    really_read t.lwt_fd t.fd len buf >|= fun n ->
    t.cursor <- off ++ Int64.of_int n;
    n
end

module Lwt_raw = struct
  let really_write fd buf =
    let rec aux off len =
      Lwt_unix.write fd buf off len >>= fun w ->
      if w = 0 || w = len then Lwt.return_unit
      else (aux [@tailcall]) (off + w) (len - w)
    in
    (aux [@tailcall]) 0 (Bytes.length buf)

  let really_read fd len buf =
    let rec aux off len =
      Lwt_unix.read fd buf off len >>= fun r ->
      if r = 0 then Lwt.return off (* end of file *)
      else if r = len then Lwt.return (off + r)
      else (aux [@tailcall]) (off + r) (len - r)
    in
    (aux [@tailcall]) 0 len

  let lseek t off =
    if off = t.cursor then ()
    else
      let _ = Unix.LargeFile.lseek t.fd off Unix.SEEK_SET in
      t.cursor <- off

  let unsafe_write t ~off buf =
    lseek t off;
    let buf = Bytes.unsafe_of_string buf in
    really_write t.lwt_fd buf >|= fun () ->
    t.cursor <- off ++ Int64.of_int (Bytes.length buf);
    t.cursor

  let unsafe_read t ~off ~len buf =
    lseek t off;
    really_read t.lwt_fd len buf >|= fun n ->
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

(* let arr = ref [||] *)

module Unix_bench = struct
  open Raw

  let read t ~off buf = unsafe_read t.raw ~off ~len:(Bytes.length buf) buf

  let write t ~off buf = unsafe_write t.raw ~off buf

  let write t =
    let rec aux off i =
      if i = nb_elements then ()
      else
        let random = random_string () in
        let off = write t ~off random in
        aux off (i + 1)
    in
    aux start_off 0

  let read t =
    let rec aux off i =
      if i = nb_elements then ()
      else
        let buf = Bytes.create element_size in
        let n = read t ~off buf in
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
    Fmt.epr
      "unix sys call nb_elements = %d; element_size = %d\n\
      \ write = %f, read = %f\n"
      nb_elements element_size write_time read_time;
    close t
end

module Lwt_unix_bench = struct
  let read ~wrap t ~off buf =
    if wrap then Wrap_lwt_raw.unsafe_read t.raw ~off ~len:(Bytes.length buf) buf
    else Lwt_raw.unsafe_read t.raw ~off ~len:(Bytes.length buf) buf

  let write ~wrap t ~off buf =
    if wrap then Wrap_lwt_raw.unsafe_write t.raw ~off buf
    else Lwt_raw.unsafe_write t.raw ~off buf

  let write ~wrap t =
    let rec aux off i =
      if i = nb_elements then Lwt.return_unit
      else
        let random = random_string () in
        write ~wrap t ~off random >>= fun off -> aux off (i + 1)
    in
    aux start_off 0

  let read ~wrap t =
    let rec aux off i =
      if i = nb_elements then Lwt.return_unit
      else
        let buf = Bytes.create element_size in
        read ~wrap t ~off buf >>= fun n -> aux (off ++ Int64.of_int n) (i + 1)
    in
    aux start_off 0

  let with_timer f =
    let t0 = Sys.time () in
    f () >|= fun () -> Sys.time () -. t0

  let bench_wrap () =
    let t = openfile "store.pack" in
    with_timer (fun () -> write ~wrap:true t) >>= fun write_time ->
    with_timer (fun () -> read ~wrap:true t) >|= fun read_time ->
    Fmt.epr
      "wrap_syscall unix sys call nb_elements = %d; element_size = %d\n\
      \ write = %f, read = %f\n"
      nb_elements element_size write_time read_time;
    close t

  let bench () =
    let t = openfile "store.pack" in
    with_timer (fun () -> write ~wrap:false t) >>= fun write_time ->
    with_timer (fun () -> read ~wrap:false t) >|= fun read_time ->
    Fmt.epr
      "lwt_unix sys call nb_elements = %d; element_size = %d\n\
      \ write = %f, read = %f\n"
      nb_elements element_size write_time read_time;
    close t
end

(* let count = ref 0 *)

let () =
  (* let r = random_string () in *)
  (* arr := Array.make nb_elements r; *)
  (* Fmt.epr "with array in memory\n"; *)
  Unix_bench.bench ();
  Lwt_main.run (Lwt_unix_bench.bench_wrap ());
  Lwt_main.run (Lwt_unix_bench.bench ())

(* Array.iter (fun i -> count := String.length i + !count) !arr *)
