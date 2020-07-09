open Cmdliner
open Re.Str

let file =
  let doc = Arg.info ~doc:"File." [ "f"; "file" ] in
  Arg.(value @@ opt string "logs" doc)

let separate_logs =
  let doc =
    Arg.info
      ~doc:"Separate files for validator.block and prevalidator/validator.chain"
      [ "s"; "separate" ]
  in
  Arg.(value @@ flag doc)

let baker =
  let doc = Arg.info ~doc:"Baker logs" [ "b"; "baker" ] in
  Arg.(value @@ flag doc)

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
  List.iter (fun (_t, l) -> Printf.fprintf oc "%f\n" l) lines;
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
    | "us" -> float_of_string a *. 0.000001
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

(** returns true if [s] contains at least one string in ls *)
let rec contains s ls =
  match ls with
  | hd :: tl -> (
      try
        let _ = search_forward (regexp hd) s 0 in
        true
      with Not_found -> contains s tl )
  | [] -> false

let extract_from_line_block_validator s =
  let completed = "completed in" in
  try
    let pos = search_forward (regexp completed) s 0 in
    if not (contains s [ "validator.block" ]) then raise Not_found;
    let time = extract_timestamp s in
    let completed =
      let ls = string_after s pos |> split (regexp "[ \t,]+") in
      List.nth ls 2
    in
    let seconds = convert_to_seconds completed in
    Some (time, seconds)
  with Not_found -> None

let extract_from_line_chain_validator s =
  let completed = "completed in" in
  try
    let pos = search_forward (regexp completed) s 0 in
    if not (contains s [ "prevalidator"; "validator.chain" ]) then
      raise Not_found;
    let time = extract_timestamp s in
    let completed =
      let ls = string_after s pos |> split (regexp "[ \t,]+") in
      List.nth ls 2
    in
    let seconds = convert_to_seconds completed in
    Some (time, seconds)
  with Not_found -> None

let extract_from_baker s =
  let validated = "validated block in" in
  try
    let pos = search_forward (regexp validated) s 0 in
    let validated_in =
      let ls = string_after s pos |> split (regexp "[ \t,]+") in
      List.nth ls 3
    in
    let seconds = convert_to_seconds validated_in in
    Some (0.0, seconds)
  with Not_found -> None

let main file separate baker =
  if baker then
    let lines = read_file file extract_from_baker in
    write_file "baker_validated" lines
  else if separate then (
    let lines = read_file file extract_from_line_block_validator in
    write_file "block_validator" lines;
    let lines = read_file file extract_from_line_chain_validator in
    write_file "chain_validator" lines )
  else
    let lines = read_file file extract_from_line in
    write_file "completed_times" lines

let main_term = Term.(const main $ file $ separate_logs $ baker)

let () =
  let info = Term.info "Extract from file" in
  Term.exit @@ Term.eval (main_term, info)
