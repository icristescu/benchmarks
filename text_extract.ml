open Cmdliner
open Re.Str

let file =
  let doc = Arg.info ~doc:"File." [ "f"; "file" ] in
  Arg.(value @@ opt string "dune" doc)

let read_file filename extract =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      let line = input_line chan in
      match extract line with None -> () | Some l -> lines := l :: !lines
    done;
    !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

let write_file file lines =
  let oc = open_out file in
  List.iter (fun (t, l) -> Printf.fprintf oc "%s, %f\n" t l) lines;
  close_out oc

let convert_to_seconds s =
  let rec convert_minutes a b =
    let pos = search_forward (regexp "[0-9]") b 0 in
    let c = string_before b pos in
    match c with
    | "min" ->
        let d = convert (string_after b pos) in
        (float_of_string a *. 60.0) +. d
    | _ -> Fmt.failwith "not implemented %s" s
  and convert s =
    let pos = search_forward (regexp "[a-z]") s 0 in
    let a = string_before s pos in
    let b = string_after s pos in
    match b with
    | "s" -> float_of_string a
    | "ms" -> float_of_string a *. 0.001
    | "min" -> float_of_string a *. 60.0
    | _ -> (
        try convert_minutes a b
        with Not_found -> Fmt.failwith "not implemented %s" s )
  in
  convert s

let extract_timestamp s =
  let time =
    let ls = split (regexp "[ \t,]+") s in
    List.nth ls 2
  in
  let ls = split (regexp "[:]") time in
  List.fold_right (fun a acc -> a ^ acc) ls ""

let extract_from_line s =
  let completed = "completed in" in
  try
    let pos = search_forward (regexp completed) s 0 in
    let time = extract_timestamp s in
    let completed =
      let ls = string_after s pos |> split (regexp "[ \t,]+") in
      List.nth ls 2
    in
    let seconds = convert_to_seconds completed in
    Some (time, seconds)
  with Not_found -> None

let counter = ref 0

let extract2 s =
  let validated = "successfully validated" in
  try
    let _ = search_forward (regexp validated) s 0 in
    counter := succ !counter;
    if !counter = 50 then (
      counter := 0;
      let time = extract_timestamp s in
      Some (time, 1.0) )
    else None
  with Not_found -> None

let main file =
  let lines = read_file file extract_from_line in
  write_file "completed_times" lines;
  let lines = read_file file extract2 in
  write_file "block_validated" lines

let main_term = Term.(const main $ file)

let () =
  let info = Term.info "Extract from file" in
  Term.exit @@ Term.eval (main_term, info)
