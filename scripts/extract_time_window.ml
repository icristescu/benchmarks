open Re.Str

let start = ref 0.0

let stop = ref 0.0

let header_line s =
  try
    let _pos = search_forward (regexp "cycles") s 0 in
    true
  with Not_found -> false

let in_interval s =
  try
    let _pos = search_forward (regexp "cycles") s 0 in
    let t =
      let ls = split (regexp "[ \t,:]+") s in
      let time = List.nth ls 2 in
      float_of_string time
    in
    t >= !start && t <= !stop
  with Not_found -> assert false

let read_line ic oc cp =
  let rec aux cp i =
    try
      let line = input_line ic in
      match header_line line with
      | false ->
          if cp then Printf.fprintf oc "%s\n" line;
          aux cp (i + 1)
      | true ->
          if in_interval line then (
            Printf.fprintf oc "%s\n" line;
            aux true 0 )
          else aux false 0
    with End_of_file -> ()
  in
  aux cp 0

let read_file input output =
  let ic = open_in input in
  let oc = open_out output in
  read_line ic oc true;
  close_in ic;
  close_out oc

let main in_file out_file start' stop' =
  start := start';
  stop := stop';
  read_file in_file out_file

open Cmdliner

let in_file =
  let doc =
    Arg.info ~doc:"File from the output of perf script" [ "i"; "input" ]
  in
  Arg.(value @@ opt string "out.perf" doc)

let out_file =
  let doc =
    Arg.info ~doc:"Output file, containing only the time interval"
      [ "o"; "output" ]
  in
  Arg.(value @@ opt string "out_filtered.perf" doc)

let start =
  let doc = Arg.info ~doc:"Start timestamp." [ "s"; "start" ] in
  Arg.(value @@ opt float 0.0 doc)

let stop =
  let doc = Arg.info ~doc:"End timestamp." [ "e"; "end" ] in
  Arg.(value @@ opt float 1.0 doc)

let main_term = Term.(const main $ in_file $ out_file $ start $ stop)

let () =
  let info = Term.info "Extract from file" in
  Term.exit @@ Term.eval (main_term, info)
