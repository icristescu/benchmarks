open Cmdliner
open Re.Str

let file =
  let doc = Arg.info ~doc:"File." [ "f"; "file" ] in
  Arg.(value @@ opt string "logs" doc)

let read_file filename extract =
  let chan = open_in filename in
  let oc = open_out "visit_level" in
  try
    while true do
      let line = input_line chan in
      match extract line with
      | None -> ()
      | Some (x, y) -> Printf.fprintf oc "%s,%s\n" x y
    done
  with End_of_file ->
    close_in chan;
    close_out oc

let extract_time s =
  let pos = search_forward (regexp "us") s 0 in
  let s = string_before s pos in
  string_after s 1

let lines s =
  let ls = split (regexp "[ ]+") s in
  let time = extract_time (List.hd ls) in
  let level = List.nth ls 5 in
  Some (time, level)

let main file = read_file file lines

let main_term = Term.(const main $ file)

let () =
  let info = Term.info "Extract from the logs of a tezos node" in
  Term.exit @@ Term.eval (main_term, info)
