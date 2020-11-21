open Memtrace.Trace

module Byte_units : sig
  type t

  val zero : t

  val ( + ) : t -> t -> t

  val ( - ) : t -> t -> t

  val of_bytes : t -> float

  val to_bytes : float -> t
end = struct
  type t = float

  let zero = 0.0

  let ( + ) x y = x +. y

  let ( - ) x y = x -. y

  let to_bytes n = n

  let of_bytes n = n
end

type obj_info = { size : Byte_units.t }

let bytes_of_nsamples ~trace nsamples =
  let { Info.sample_rate; word_size; _ } = Reader.info trace in
  let nwords = Float.of_int nsamples /. sample_rate in
  Float.of_int word_size *. nwords |> Byte_units.to_bytes

let full_graph_and_max_time ~trace : (Timedelta.t * Byte_units.t) list =
  let objects : obj_info Obj_id.Tbl.t = Obj_id.Tbl.create 1000 in
  let total_size = ref Byte_units.zero in
  let points : (Timedelta.t * Byte_units.t) list ref = ref [] in
  Reader.iter trace (fun time event ->
      ( match event with
      | Alloc { obj_id; nsamples; _ } ->
          let size = nsamples |> bytes_of_nsamples ~trace in
          Obj_id.Tbl.add objects obj_id { size };
          total_size := Byte_units.(!total_size + size)
      | Promote _ -> ()
      | Collect obj_id ->
          let obj_info = Obj_id.Tbl.find objects obj_id in
          Obj_id.Tbl.remove objects obj_id;
          total_size := Byte_units.(!total_size - obj_info.size) );
      points := (time, !total_size) :: !points);
  List.rev !points

let read_trace filename =
  let trace = Reader.open_ ~filename in
  let oc = open_out (filename ^ ".out") in
  let info = Reader.info trace in
  Format.printf "word size = %d\n" info.word_size;
  let points = full_graph_and_max_time ~trace in
  List.iter
    (fun (time, total_size) ->
      let t = Timedelta.to_int64 time in
      let s = Byte_units.of_bytes total_size |> Float.to_int in
      Printf.fprintf oc "%Ld,%d\n" t s)
    points;
  close_out oc;
  Reader.close trace

let main srcs =
  List.iter
    (fun tr ->
      Format.printf "reading %s\n" tr;
      read_trace tr)
    srcs

open Cmdliner

let srcs =
  let doc = "Trace file(s) to compare." in
  Arg.(non_empty & pos_all file [] & info [] ~docv:"TRACES" ~doc)

let cmd =
  let doc = "Read and plot trace files" in
  let exits = Term.default_exits in
  ( Term.(const main $ srcs),
    Term.info "read_trace" ~version:"v1.0.4" ~doc ~exits )

let () = Term.(exit @@ eval cmd)
