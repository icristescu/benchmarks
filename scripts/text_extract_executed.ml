open Cmdliner
open Re.Str

let file =
  let doc = Arg.info ~doc:"File." [ "f"; "file" ] in
  Arg.(value @@ opt string "logs" doc)

let init = ref []

let checkout = ref []

let get_protocol = ref []

let commit = ref []

let sync = ref []

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

(** returns true if [s] contains at least one string in ls *)
let rec contains s ls =
  match ls with
  | hd :: tl -> (
      try
        let _ = search_forward (regexp hd) s 0 in
        true
      with Not_found -> contains s tl )
  | [] -> false

let extract s =
  let executed = "executed in" in
  try
    let pos = search_forward (regexp executed) s 0 in
    let time =
      let ls = string_after s pos |> split (regexp "[ \t,]+") in
      List.nth ls 2
    in
    let seconds = convert_to_seconds time in
    if contains s [ "Context.init" ] then init := seconds :: !init
    else if contains s [ "Context.checkout" ] then
      checkout := seconds :: !checkout
    else if contains s [ "Context.get_protocol" ] then
      get_protocol := seconds :: !get_protocol
    else if contains s [ "Context.commit" ] then commit := seconds :: !commit
    (* else  *)
    (* if contains s [ "RO sync" ] then sync := seconds :: !sync *)
    (* else Fmt.failwith "line %s" s *)
  with Not_found -> ()

let read_file filename =
  let chan = open_in filename in
  try
    while true do
      let line = input_line chan in
      extract line
    done
  with End_of_file -> close_in chan

let write_file file lines =
  let oc = open_out file in
  List.iter (fun l -> Printf.fprintf oc "%f\n" l) lines;
  close_out oc

let main file =
  let () = read_file file in
  write_file "init" (List.rev !init);
  write_file "checkout" (List.rev !checkout);
  write_file "protocol" (List.rev !get_protocol);
  write_file "commit" (List.rev !commit);
  write_file "ro_syncs" (List.rev !sync)

let main_term = Term.(const main $ file)

let () =
  let info = Term.info "Extract from file" in
  Term.exit @@ Term.eval (main_term, info)
