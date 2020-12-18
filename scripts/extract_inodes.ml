open Cmdliner
open Re.Str

let file =
  let doc = Arg.info ~doc:"File." [ "f"; "file" ] in
  Arg.(value @@ opt string "logs" doc)

let context_tag =
  let doc = Arg.info ~doc:"Extract context lines" [ "context" ] in
  Arg.(value @@ flag doc)

let keys_tag =
  let doc = Arg.info ~doc:"Extract keys" [ "keys" ] in
  Arg.(value @@ flag doc)

let run_id =
  let doc = Arg.info ~doc:"Run id." [ "i"; "id" ] in
  Arg.(value @@ opt int 1 doc)

let read_file filename extract run_id =
  let chan = open_in filename in
  let oc = open_out (run_id ^ "-context") in
  try
    while true do
      let line = input_line chan in
      match extract line with None -> () | Some l -> Printf.fprintf oc "%s\n" l
    done
  with End_of_file ->
    close_in chan;
    close_out oc

let context_lines s =
  let validated = "context" in
  try
    let pos = search_forward (regexp validated) s 0 in
    let line = string_after s (pos+9) in
    Some line
  with Not_found -> None

let main file run_id context =
  let run_id = Printf.sprintf "%d" run_id in
  if context then
    read_file file context_lines run_id

let main_term = Term.(const main $ file $ run_id $ context_tag )

let () =
  let info = Term.info "Extract from the logs of a tezos node" in
  Term.exit @@ Term.eval (main_term, info)
