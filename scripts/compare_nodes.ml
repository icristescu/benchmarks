open Cmdliner
open Re.Str

let file1 =
  let doc = Arg.info ~doc:"File." [ "f1"; "file1" ] in
  Arg.(value @@ opt string "file" doc)

let read_write_file filename extract =
  let chan = open_in filename in
  let oc = open_out (filename ^ "_only_off") in
  try
    while true do
      let line = input_line chan in
      match extract line with
      | Some l -> Printf.fprintf oc "%s\n" l
      | None -> ()
    done
  with End_of_file ->
    close_in chan;
    close_out oc

let remove_offset s =
  let off = "at off" in
  try
    let pos = search_forward (regexp off) s 0 in
    let str_before = string_before s pos in
    let str_after =
      string_after s pos
      |> split (regexp "[ ]+")
      |> (function
           | x :: y :: _ :: rs -> x :: y :: rs
           | _ -> Fmt.failwith "unexpected %s" s)
      |> String.concat " "
    in
    String.concat "" [ str_before; str_after ]
  with Not_found -> Fmt.failwith "unexpected %s" s

let extract_offset s =
  let off = "at off" in
  try
    let pos = search_forward (regexp off) s 0 in
    let offset =
      string_after s pos |> split (regexp "[ ]+") |> function
      | _ :: _ :: offset :: _ -> offset
      | _ -> Fmt.failwith "unexpected %s" s
    in
    Some offset
  with Not_found -> Fmt.failwith "unexpected %s" s

let extract_hashes_diff s =
  let found = "found \\[" in
  let co = "-Co" in
  let off = "at off" in
  try
    let pos1 = search_forward (regexp found) s 0 in
    let _ = search_forward (regexp co) s pos1 in
    let pos2 = search_forward (regexp off) s pos1 in
    let offset =
      string_after s pos2 |> split (regexp "[ ]+") |> function
      | _ :: _ :: offset :: _ -> offset
      | _ -> Fmt.failwith "unexpected %s" s
    in
    Some offset
  with Not_found -> None

(* let main file1 = read_write_file file1 remove_hash *)
let main file1 = read_write_file file1 extract_hashes_diff

let main_term = Term.(const main $ file1)

let () =
  let info = Term.info "Extract from the logs of a tezos node" in
  Term.exit @@ Term.eval (main_term, info)
