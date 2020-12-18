open Cmdliner
open Re.Str

let file =
  let doc = Arg.info ~doc:"File." [ "f"; "file" ] in
  Arg.(value @@ opt string "logs" doc)

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
  List.iter (fun (_, l) -> Printf.fprintf oc "%f\n" l) lines;
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
        with Not_found -> Fmt.failwith "not implemented %s" s)
  in
  convert s

let extract oc s =
  let ls = split (regexp "[ \t,]+") s in
  if List.length ls <> 2 then ()
  else
    try ignore (search_forward (regexp "objects.") s 0)
    with Not_found -> (
      try ignore (search_forward (regexp "completed.") s 0)
      with Not_found -> (
        try ignore (search_forward (regexp "waiting.") s 0)
        with Not_found ->
          let time = List.nth ls 1 in
          let seconds = convert_to_seconds time in
          Printf.fprintf oc "%f\n" seconds))

let main file =
  let oc = open_out "replace" in
  let ic = open_in file in
  let extract = extract oc in
  try
    while true do
      let line = input_line ic in
      extract line
    done
  with End_of_file ->
    close_in ic;
    close_out oc

let _main _file =
  let s = "[] objects_added_in_upper_since_last_freeze=5049261" in
  let pos = search_forward (regexp "objects.") s 0 in
  Fmt.epr "pos %d" pos

let main_term = Term.(const main $ file)

let () =
  let info = Term.info "Extract from the logs of a tezos node" in
  Term.exit @@ Term.eval (main_term, info)
