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

let convert_to_us s =
  let pos = search_forward (regexp "us") s 0 in
  String.sub s 1 (pos - 1)

let extract oc s =
  let ls = split (regexp "[ \t,]+") s in
  if List.length ls <> 6 then ()
  else
    let time = List.nth ls 0 in
    let seconds = convert_to_us time in
    let contents = List.nth ls 5 in
    Printf.fprintf oc "%s, %s\n" seconds contents

let main file =
  let oc = open_out "nodes" in
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

let main_term = Term.(const main $ file)

let () =
  let info = Term.info "Extract from the logs of a tezos node" in
  Term.exit @@ Term.eval (main_term, info)
