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

  let grep logs = Cmd.(v "grep" % "failed" % p logs)

  let free data_dir log =
    OS.Dir.delete ~must_exist:true ~recurse:true data_dir >>= fun () ->
    OS.File.delete log

  let mv data_dir log dst =
    OS.Cmd.run (mv data_dir dst) >>= fun () -> OS.Cmd.run (mv log dst)

  let tezos_processes () =
    let tmp = OS.Dir.default_tmp () in
    let tmp_file = Fpath.(tmp / "pids_tezos") in
    Format.printf "using temporary file %a\n%!" Fpath.pp tmp_file;
    OS.Cmd.(run_out Cmd.(v "ps" % "aux") |> to_file tmp_file) >>= fun () ->
    OS.Cmd.(run_out Cmd.(v "grep" % "tezos" % p tmp_file) |> to_lines)
    >>= fun l ->
    match l with
    | [] -> failwith "no tezos node running?"
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

let run_node node data_dir logs ?(logs_level = Logs.Info) sleep =
  match Unix.fork () with
  | 0 ->
      (match logs_level with
      | Debug -> OS.Env.set_var "TEZOS_CONTEXT" (Some "vv")
      | Info -> OS.Env.set_var "TEZOS_CONTEXT" (Some "v")
      | _ -> OS.Env.set_var "TEZOS_CONTEXT" None)
      >>= fun () ->
      let run_node = run_node node data_dir in
      Format.printf "child %d: %a\n%!" (Unix.getpid ()) Cmd.pp run_node;
      OS.Cmd.(run_out ~err:err_run_out run_node |> to_file logs) >>= fun _ ->
      exit 0
  | pid -> (
      let me = Unix.getpid () in
      Format.printf "I'm process %d, child process is %d\n%!" me pid;
      Unix.sleep sleep;
      (* the child process launches the tezos node in a separate process - we
         only kill one pid, the second one. *)
      tezos_processes () >>= fun pids ->
      List.filter (fun p -> p <> pid && p <> me) pids |> function
      | [ _; child ] ->
          Format.printf "killed %d\n%!" pid;
          Unix.kill child 9;
          Unix.sleep 120;
          Format.printf "the run continues...\n%!";
          tezos_processes () >>= fun pids ->
          List.filter (fun p -> p <> me) pids
          |> List.iter (fun pid ->
                 Format.printf "killed %d\n%!" pid;
                 Unix.kill pid 9);
          Unix.sleep 10;
          Ok ()
      | _ -> failwith "unexpected tezos processes")

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

let run conf id =
  let run_node = run_node conf.tezos_node in
  let run_reconstruct = run_reconstruct conf.tezos_node in
  let data0 = conf.data_dirs.(0) in
  let logs0 = conf.logs.(0) in
  cp conf.initial_dir data0 |> OS.Cmd.run >>= fun () ->
  run_node ~logs_level:Logs.Debug data0 logs0 conf.bootstrap >>= fun () ->
  let data1 = conf.data_dirs.(1) in
  let logs1 = conf.logs.(1) in
  cp data0 data1 |> OS.Cmd.run >>= fun () ->
  OS.File.delete Fpath.(data1 / "lock") >>= fun () ->
  run_node data1 logs1 conf.continue >>= fun () ->
  (grep logs1 |> OS.Cmd.(run_status ~err:err_run_out)) >>= function
  | `Exited 1 ->
      Format.printf "node works after crash\n%!";
      s conf id
  | `Exited 0 -> (
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
      run_node data3 logs3 conf.continue >>= fun () ->
      (grep logs3 |> OS.Cmd.(run_status ~err:err_run_out)) >>= function
      | `Exited 1 ->
          Format.printf "node works after reconstruct\n%!";
          fs conf id
      | `Exited 0 ->
          Format.printf "node fails after reconstruct\n%!";
          ff conf id
      | status ->
          Fmt.failwith "unexpected grep status %a" OS.Cmd.pp_status status)
  | status -> Fmt.failwith "unexpected grep status %a" OS.Cmd.pp_status status

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
      string_of_int i |> run conf)
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
