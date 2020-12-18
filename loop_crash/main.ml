open Bos
open Rresult

type config = {
  data_dirs : Fpath.t array;
  initial_dir : Fpath.t;
  results_dir : Fpath.t;
  logs : Fpath.t array;
  bootstrap : int;
  continue : int;
  tezos_node : string;
  logs_level : Logs.level option;
}

open struct
  let cp src dst = Cmd.(v "cp" % "-R" % p src % p dst)

  let mv src dst = Cmd.(v "mv" % p src % p dst)

  let run_node node data_dir = Cmd.(v node % "run" % "--data-dir" % p data_dir)

  let reconstruct node data_dir =
    Cmd.(v node % "storage" % "reconstruct-index" % "--data-dir" % p data_dir)

  let free data_dir log =
    OS.Dir.delete ~must_exist:true ~recurse:true data_dir >>= fun () ->
    OS.File.delete log

  let mv data_dir log dst =
    OS.Cmd.run (mv data_dir dst) >>= fun () -> OS.Cmd.run (mv log dst)

  let ulimit ?mem () =
    (* machine dependent - in our case ulimit is set in kbytes.*)
    let gb = 1024 * 1024 in
    let mem =
      match mem with None -> "unlimited" | Some m -> string_of_int (m * gb)
    in
    OS.Env.current () >>= fun env ->
    OS.Cmd.(run ~env Cmd.(v "ulimit" % "-v" % mem)) >>= fun () ->
    OS.Cmd.(run_out ~env ~err:err_run_out Cmd.(v "ulimit" % "-v") |> to_string)
    >>= fun m ->
    Format.printf "read ulimit -v %s\n%!" m;
    Ok ()

  let grep msg logs =
    match Cmd.(v "grep" % msg % p logs) |> OS.Cmd.run_status with
    | Ok (`Exited 1) ->
        Format.printf "grep exit 1 - should be catched\n%!";
        false
    | Ok (`Exited 0) ->
        Format.printf "grep exit 0 - should be catched\n%!";
        true
    | Ok status ->
        Fmt.failwith "grep unexpected status %a" OS.Cmd.pp_status status
    | Error (`Msg m) -> Fmt.failwith "grep failed with %s" m

  let grep_validator_failed logs =
    grep "TangoAdviseSlimCanvas" logs |> function
    | false ->
        Format.printf "node did not reached the ulimit\n%!";
        false
    | true ->
        Format.printf "node reached the ulimit\n%!";
        true

  let grep_to_file msg logs =
    match OS.Cmd.(run_out Cmd.(v "grep" % msg % p logs) |> to_lines) with
    | Ok l -> Ok l
    | Error (`Msg m) ->
        Format.printf "grep to file - exit 1 - %s\n%!" m;
        Ok []

  let tezos_processes () =
    let tmp = OS.Dir.default_tmp () in
    let tmp_file = Fpath.(tmp / "pids_tezos") in
    Format.printf "using temporary file %a\n%!" Fpath.pp tmp_file;
    OS.Cmd.(run_out Cmd.(v "ps" % "aux") |> to_file tmp_file) >>= fun () ->
    grep_to_file "tezos" tmp_file >>= fun l ->
    match l with
    | [] ->
        Format.printf "no tezos node running\n%!";
        Ok []
    | ls ->
        List.fold_left
          (fun acc line ->
            let line' = Re.Str.(split (regexp "[ \t,]+")) line in
            let pid = List.nth line' 1 in
            Format.printf "grep tezos pid %s\n%!" pid;
            let pid = int_of_string pid in
            pid :: acc)
          [] ls
        |> List.sort Int.compare
        |> fun acc -> Ok acc
end

let _wait pid =
  let pid', status = Unix.waitpid [ Unix.WUNTRACED ] pid in
  if pid <> pid' then
    Fmt.failwith "I'm %d, expecting child %d, but got %d instead"
      (Unix.getpid ()) pid pid';
  match status with
  | Unix.WEXITED code -> Format.printf "child %d exits with %d\n%!" pid code
  | Unix.WSIGNALED code ->
      Format.printf "child %d signaled with %d\n%!" pid code
  | Unix.WSTOPPED code -> Format.printf "child %d stopped with %d\n%!" pid code

let separate_process_runs_node ?(logs_level = Logs.Info) node data_dir logs mem
    =
  ulimit ?mem () >>= fun () ->
  (match logs_level with
  | Debug -> OS.Env.set_var "TEZOS_CONTEXT" (Some "vv")
  | Info -> OS.Env.set_var "TEZOS_CONTEXT" (Some "v")
  | _ -> OS.Env.set_var "TEZOS_CONTEXT" None)
  >>= fun () ->
  let run_node = run_node node data_dir in
  Format.printf "process %d: %a\n%!" (Unix.getpid ()) Cmd.pp run_node;
  OS.Cmd.(run_out ~err:err_run_out run_node |> to_file logs) >>= fun _ -> exit 0

let kill_all pids =
  List.iter
    (fun pid ->
      Format.printf "killed %d\n%!" pid;
      Unix.kill pid 9)
    pids;
  Unix.sleep 10;
  Ok ()

let kill_random = function
  | [ p1; p2 ] ->
      let p = if Random.int 2 = 1 then p1 else p2 in
      Format.printf "killed %d\n%!" p;
      Unix.kill p 9
  | _ -> failwith "unexpected tezos processes"

let run_node node data_dir logs ?logs_level mode sleep =
  let mem = if mode = `Ulimit then Some (Random.int 4 + 1) else None in
  Format.printf "Setting ulimit to %s\n%!"
    (match mem with None -> "unlimited" | Some x -> string_of_int x);
  match Unix.fork () with
  | 0 -> separate_process_runs_node ?logs_level node data_dir logs mem
  | pid -> (
      let me = Unix.getpid () in
      let kill_all pids = List.filter (fun p -> p <> me) pids |> kill_all in
      Format.printf "I'm process %d, child process is %d\n%!" me pid;
      match mode with
      | `Ulimit ->
          let sleep = Option.get mem * 60 * 10 in
          Unix.sleep sleep;
          tezos_processes () >>= fun pids -> kill_all pids
      | `Kill_all ->
          Unix.sleep sleep;
          tezos_processes () >>= fun pids -> kill_all pids
      | `Randomly ->
          Unix.sleep sleep;
          tezos_processes () >>= fun pids ->
          List.filter (fun p -> p <> pid && p <> me) pids |> fun pids ->
          kill_random pids;
          Format.printf "the run continues with remaining process ...\n%!";
          Unix.sleep 120;
          tezos_processes () >>= fun pids -> kill_all pids)

let relaunch node data_dir logs ?logs_level sleep =
  match Unix.fork () with
  | 0 -> separate_process_runs_node ?logs_level node data_dir logs None
  | pid ->
      let me = Unix.getpid () in
      let kill_all pids = List.filter (fun p -> p <> me) pids |> kill_all in
      Format.printf "I'm process %d, child process is %d\n%!" me pid;
      Unix.sleep sleep;
      tezos_processes () >>= fun pids -> kill_all pids

let run_reconstruct node data_dir logs =
  let run_node = reconstruct node data_dir in
  Format.printf "%a\n%!" Cmd.pp run_node;
  OS.Cmd.(run_out ~err:err_run_out run_node |> to_file logs)

let s conf id =
  OS.Dir.create Fpath.(conf.results_dir / "s" / id) >>= fun _ ->
  free conf.data_dirs.(0) conf.logs.(0) >>= fun () ->
  free conf.data_dirs.(1) conf.logs.(1)

let f conf id next =
  let dir = if next then "fs" else "ff" in
  let res = Fpath.(conf.results_dir / dir / id) in
  OS.Dir.create res >>= function
  | false -> Fmt.failwith "dir %a already exists" Fpath.pp res
  | true ->
      mv conf.data_dirs.(0) conf.logs.(0) res >>= fun () ->
      mv conf.data_dirs.(1) conf.logs.(1) res >>= fun () ->
      if next then
        free conf.data_dirs.(2) conf.logs.(2) >>= fun () ->
        free conf.data_dirs.(3) conf.logs.(3)
      else
        mv conf.data_dirs.(2) conf.logs.(2) res >>= fun () ->
        mv conf.data_dirs.(3) conf.logs.(3) res

let fs conf id = f conf id true

let ff conf id = f conf id false

let run mode conf id =
  let run_node = run_node conf.tezos_node in
  let relaunch = relaunch conf.tezos_node in
  let run_reconstruct = run_reconstruct conf.tezos_node in
  let data0 = conf.data_dirs.(0) in
  let logs0 = conf.logs.(0) in
  cp conf.initial_dir data0 |> OS.Cmd.run >>= fun () ->
  run_node ~logs_level:Logs.Debug data0 logs0 mode conf.bootstrap >>= fun () ->
  let continue =
    mode <> `Ulimit || (mode = `Ulimit && grep_validator_failed logs0)
  in
  match continue with
  | false -> Ok ()
  | true -> (
      let data1 = conf.data_dirs.(1) in
      let logs1 = conf.logs.(1) in
      cp data0 data1 |> OS.Cmd.run >>= fun () ->
      OS.File.delete Fpath.(data1 / "lock") >>= fun () ->
      relaunch data1 logs1 conf.continue >>= fun () ->
      grep "failed" logs1 |> function
      | false ->
          Format.printf "node works after crash\n%!";
          s conf id
      | true -> (
          Format.printf "node fails after crash\n%!";
          let data2 = conf.data_dirs.(2) in
          let logs2 = conf.logs.(2) in
          OS.Cmd.run (cp data1 data2) >>= fun () ->
          OS.Dir.delete ~must_exist:true ~recurse:true
            Fpath.(data2 / "context" / "index")
          >>= fun () ->
          run_reconstruct data2 logs2 >>= fun () ->
          let data3 = conf.data_dirs.(3) in
          let logs3 = conf.logs.(3) in
          OS.Cmd.run (cp data2 data3) >>= fun () ->
          OS.File.delete Fpath.(data3 / "lock") >>= fun () ->
          relaunch data3 logs3 conf.continue >>= fun () ->
          grep "failed" logs3 |> function
          | false ->
              Format.printf "node works after reconstruct\n%!";
              fs conf id
          | true ->
              Format.printf "node fails after reconstruct\n%!";
              ff conf id))

let main logs_level data_dir tezos_node bootstrap continue loop =
  Printexc.record_backtrace true;
  OS.Dir.current () >>= fun current ->
  let initial_dir = Fpath.v data_dir in
  let current =
    let pid = Unix.getpid () |> string_of_int in
    Fpath.(current / pid)
  in
  let results_dir = Fpath.(current / "res") in
  OS.Dir.create results_dir >>= fun _ ->
  let data_dirs =
    Array.init 4 (fun i ->
        let data = "data" ^ string_of_int i in
        Fpath.(current / "res" / data))
  in
  let logs =
    Array.init 4 (fun i ->
        let logs = "logs" ^ string_of_int i in
        Fpath.(current / logs))
  in
  let bootstrap = bootstrap * 60 in
  let continue = continue * 60 in
  let conf =
    {
      initial_dir;
      tezos_node;
      data_dirs;
      logs;
      results_dir;
      bootstrap;
      continue;
      logs_level;
    }
  in
  Format.printf
    "run with: node = %s\n\
     initial_dir = %a\n\
     data = %a\n\
     results =%a\n\
     logs = %a\n\
     %!"
    tezos_node Fpath.pp initial_dir Fpath.pp data_dirs.(0) Fpath.pp results_dir
    Fpath.pp logs.(0);
  List.fold_left
    (fun acc i ->
      acc >>= fun () ->
      Format.printf "Run id = %d\n%!" i;
      match run `Ulimit conf (string_of_int i) with
      | Ok () -> Ok ()
      | Error (`Msg m) as e ->
          Format.printf "Erorr -- %s\n%!" m;
          e)
    (Ok ()) (List.init loop Fun.id)

open Cmdliner

let bootstrap =
  let doc =
    Arg.info
      ~doc:"Time (in minutes) of bootstrapping the node before the crash."
      [ "bootstrap" ]
  in
  Arg.(value @@ opt int 2 doc)

let continue =
  let doc =
    Arg.info ~doc:"Time (in minutes) of running the node after the crash."
      [ "continue" ]
  in
  Arg.(value @@ opt int 2 doc)

let data_dir =
  let doc = Arg.info ~doc:"Initial store." [ "data-dir" ] in
  Arg.(value @@ opt string "/Users/icristes/Documents/tezos/data" doc)

let tezos_node =
  let doc = Arg.info ~doc:"Tezos node executable." [ "tezos-node" ] in
  Arg.(value @@ opt string "/Users/icristes/Documents/tezos/tezos-node" doc)

let loop =
  let doc = Arg.info ~doc:"Number of loops." [ "loop" ] in
  Arg.(value @@ opt int 2 doc)

let log_level = Logs_cli.level ()

let main_term =
  Term.(
    const main $ log_level $ data_dir $ tezos_node $ bootstrap $ continue $ loop)

let () =
  let info = Term.info ~doc:"Run tezos nodes" "main" in
  Term.exit @@ Term.eval (main_term, info)
