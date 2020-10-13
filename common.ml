open Re.Str

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
  List.iter (fun l -> Printf.fprintf oc "%s\n" l) lines;
  close_out oc

(** returns true if [s] contains at least one string in ls *)
let rec contains s ls =
  match ls with
  | hd :: tl -> (
      try
        let _ = search_forward (regexp hd) s 0 in
        true
      with Not_found -> contains s tl )
  | [] -> false

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
