open Cmdliner
open Re.Str

let file =
  let doc = Arg.info ~doc:"File." [ "f"; "file" ] in
  Arg.(value @@ opt string "logs" doc)

let run_id =
  let doc = Arg.info ~doc:"Run id." [ "i"; "id" ] in
  Arg.(value @@ opt int 1 doc)

let read_file filename extract run_id =
  let chan = open_in filename in
  let oc = open_out (run_id ^ "-index.json") in
  try
    while true do
      let line = input_line chan in
      match extract line with None -> () | Some l -> Printf.fprintf oc "%s\n" l
    done
  with End_of_file ->
    close_in chan;
    close_out oc

let context_lines s =
  let validated = "\\[index\\]" in
  try
    let pos = search_forward (regexp validated) s 0 in
    let ls = string_after s pos |> split (regexp "[ ]+") in
    (* Fmt.epr "found at pos %d in line %s ls = %a\n" pos s Fmt.(list string) ls; *)
    Some (try List.nth ls 1 with e -> Fmt.epr "Failure in %s" s; raise e)
  with Not_found -> None

let main file run_id =
  let run_id = Printf.sprintf "%d" run_id in
  read_file file context_lines run_id

let main_term = Term.(const main $ file $ run_id )

let () =
  let info = Term.info "Extract from the logs of a tezos node" in
  Term.exit @@ Term.eval (main_term, info)
