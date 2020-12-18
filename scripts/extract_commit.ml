open Cmdliner
open Re.Str

let file =
  let doc = Arg.info ~doc:"File." [ "f"; "file" ] in
  Arg.(value @@ opt string "logs" doc)

let read_file filename extract =
  let chan = open_in filename in
  let oc = open_out "commits" in
  try
    while true do
      let line = input_line chan in
      match extract line with
      | None -> ()
      | Some l -> Printf.fprintf oc "%s\n" l
    done
  with End_of_file ->
    close_in chan;
    close_out oc

let context_lines s =
  let validated = "\\[commit\\]" in
  try
    let pos = search_forward (regexp validated) s 0 in
    let line = string_after s (pos + 9) in
    Some line
  with Not_found -> None

let main file = read_file file context_lines

let main_term = Term.(const main $ file)

let () =
  let info = Term.info "Extract from the logs of a tezos node" in
  Term.exit @@ Term.eval (main_term, info)
