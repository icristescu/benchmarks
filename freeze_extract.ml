open Cmdliner
open Re.Str
open Common

let stats_timestamp : Unix.tm option ref = ref None

let completed_in : (float * float) list ref = ref []

let line_nb = ref 0

let block_nbs : int list ref = ref []

let log_file =
  let doc = Arg.info ~doc:"Log file." [ "l"; "log-file" ] in
  Arg.(value @@ opt string "logs" doc)

let csv_file =
  let doc = Arg.info ~doc:"csv file." [ "c"; "csv-file" ] in
  Arg.(value @@ opt string "csv" doc)

type freeze_stats = {
  timestamp_start : Unix.tm;
  timestamp_end : Unix.tm;
  nb : int;
  waiting_freeze : float;
  completed_freeze : float;
  copied_objects : int;
  block_end : int;
}

let pp_timestamp ppf (t : Unix.tm) =
  Fmt.pf ppf "%d:%d:%d" t.tm_hour t.tm_min t.tm_sec

let pp_stats s =
  Fmt.str
    "%d started = %a ended = %a block_ended = %d waiting =%f completed =%f" s.nb
    pp_timestamp s.timestamp_start pp_timestamp s.timestamp_end s.block_end
    s.waiting_freeze s.completed_freeze

let extract_timestamp_in_unix_tm s =
  let time =
    let ls = split (regexp "[ \t,]+") s in
    List.nth ls 2
  in
  let time = string_before time 8 in
  let tm = time |> split (regexp "[:]") |> List.map int_of_string in
  match tm with
  | [ tm_hour; tm_min; tm_sec ] ->
      let t =
        ( {
            tm_hour;
            tm_min;
            tm_sec;
            tm_mday = 19;
            tm_mon = 9;
            tm_year = 2020;
            tm_wday = 6;
            tm_yday = 254;
            tm_isdst = false;
          }
          : Unix.tm )
      in
      Fmt.epr "extracted %a \n%!" pp_timestamp t;
      t
  | _ -> failwith "unexpected timestramp"

let extract_irmin_stats s =
  try
    let pos = search_forward (regexp "Irmin stats:") s 0 in
    let time = extract_timestamp_in_unix_tm s in
    let s = string_after s pos in
    let nb =
      let pos = search_forward (regexp "nb_freeze") s 0 in
      let ls = string_after s pos |> split (regexp "[ \t,]+") in
      List.nth ls 2 |> int_of_string
    in
    if nb = 0 then (
      stats_timestamp := Some time;
      raise Not_found )
    else
      let timestamp_start =
        match !stats_timestamp with
        | Some t -> t
        | None -> Fmt.failwith "expected start timestamp at second freeze"
      in
      stats_timestamp := Some time;
      let copied_objects =
        let pos = search_forward (regexp "copied_objects") s pos in
        let ls = string_after s pos |> split (regexp "[ \t,]+") in
        List.nth ls 2 |> int_of_string
      in
      let waiting_freeze =
        let pos = search_forward (regexp "waiting_freeze") s pos in
        let ls = string_after s pos |> split (regexp "[ \t,]+") in
        List.nth ls 2 |> float_of_string
      in
      let completed_freeze =
        let pos = search_forward (regexp "completed_freeze") s pos in
        let ls = string_after s pos |> split (regexp "[ \t,]+") in
        List.nth ls 2 |> float_of_string
      in
      let timestamp_end =
        let started, _ = Unix.mktime timestamp_start in
        let seconds = completed_freeze *. 0.000001 in
        Unix.localtime (started +. seconds)
      in
      ( Some
          {
            timestamp_start;
            nb;
            copied_objects;
            waiting_freeze;
            completed_freeze;
            timestamp_end;
            block_end = 0;
          }
        : freeze_stats option )
  with Not_found -> None

let extract_completed_in s =
  let completed = "completed in" in
  try
    let pos = search_forward (regexp completed) s 0 in
    let completed =
      let ls = string_after s pos |> split (regexp "[ \t,]+") in
      List.nth ls 2
    in
    let seconds = convert_to_seconds completed in
    seconds
  with Not_found -> Fmt.failwith "expected completed in .. in line %s" s

let read_next_line_until chan word =
  let rec aux () =
    let line = input_line chan in
    line_nb := succ !line_nb;
    if contains line [ word ] then line else aux ()
  in
  aux ()

let get_completed_after_timestamp filename timestamps =
  Fmt.epr "get_completed_after_timestamp \n%!";
  let chan = open_in filename in
  let rec aux = function
    | [] -> close_in chan
    | ts :: ls -> (
        try
          let _ = read_next_line_until chan ts in
          let line = read_next_line_until chan "validator.block: Request pushed" in
          let completed_in_1 = extract_completed_in line in
          let line = read_next_line_until chan "validator.block: Request pushed " in
          let completed_in_2 = extract_completed_in line in
          Fmt.epr "after %s, got %f, %f \n%!" ts completed_in_1 completed_in_2;
          completed_in := (completed_in_1, completed_in_2) :: !completed_in;
          aux ls
        with End_of_file -> close_in chan )
  in
  aux timestamps

let get_block_nbs filename =
  Fmt.epr "get_block_nbs\n%!";
  let chan = open_in filename in
  line_nb := 0;
  let rec aux block_nbs = function
    | [] ->
        close_in chan;
        block_nbs
    | (ts1, ts2) :: ls -> (
        try
          let _ = string_of_float ts1 |> read_next_line_until chan in
          let _ = string_of_float ts2 |> read_next_line_until chan in
          Fmt.epr "line= %d\n%!" !line_nb;
          aux (!line_nb :: block_nbs) ls
        with End_of_file ->
          close_in chan;
          block_nbs )
  in
  aux [] (List.rev !completed_in)

let main log_file csv_file =
  let lines = read_file log_file extract_irmin_stats in
  let wht =
    List.map (fun f -> Fmt.str "%a" pp_timestamp f.timestamp_end) lines
  in
  List.iter (fun l -> Fmt.epr "linesss %s\n%!" l) wht;
  get_completed_after_timestamp log_file wht;
  get_block_nbs csv_file
  |> List.map2 (fun f block -> { f with block_end = block }) lines
  |> List.map pp_stats |> write_file "freezes"

let main_term = Term.(const main $ log_file $ csv_file)

let () =
  Printexc.record_backtrace true;
  let info = Term.info "Extract from file" in
  Term.exit @@ Term.eval (main_term, info)
