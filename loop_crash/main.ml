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
}

open struct
  let cp src dst = Cmd.(v "cp" % "-R" % p src % p dst)

  let mv src dst = Cmd.(v "mv" % p src % p dst)

  let run_node node data_dir = Cmd.(v node % "run" % "--data-dir" % p data_dir)

  let reconstruct node data_dir =
    Cmd.(v node % "run" % "--data-dir" % p data_dir)

  let grep logs = Cmd.(v "grep" % "failed" % p logs)

  let free data_dir log =
    OS.Dir.delete ~must_exist:true ~recurse:true data_dir >>= fun () ->
    OS.File.delete log

  let mv data_dir log dst =
    OS.Cmd.run (mv data_dir dst) >>= fun () -> OS.Cmd.run (mv log dst)
end

let wait pid =
  let pid', status = Unix.waitpid [ Unix.WUNTRACED ] pid in
  if pid <> pid' then
    Fmt.failwith "I'm %d, expecting child %d, but got %d instead"
      (Unix.getpid ()) pid pid';
  match status with
  | Unix.WEXITED code -> Format.printf "child %d exits with %d\n%!" pid code
  | Unix.WSIGNALED code ->
      Format.printf "child %d signaled with %d\n%!" pid code
  | Unix.WSTOPPED code -> Format.printf "child %d stopped with %d\n%!" pid code

let run_node node data_dir logs sleep =
  match Unix.fork () with
  | 0 ->
      let run_node = run_node node data_dir in
      Format.printf "child %d: %a\n%!" (Unix.getpid ()) Cmd.pp run_node;
      OS.Cmd.(run_out ~err:err_run_out run_node |> to_file logs) >>= fun _ ->
      exit 0
  | pid ->
      Unix.sleep sleep;
      Unix.kill pid 9;
      wait pid;
      Unix.sleep 60;
      (* wait for process to completely shut down *)
      Ok ()

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
  run_node data0 logs0 conf.bootstrap >>= fun () ->
  let data1 = conf.data_dirs.(1) in
  let logs1 = conf.logs.(1) in
  cp data0 data1 |> OS.Cmd.run >>= fun () ->
  Format.printf "delete %a\n%!" Fpath.pp Fpath.(data1 / "lock");
  OS.File.delete Fpath.(data1 / "lock") >>= fun () ->
  OS.File.exists Fpath.(data1 / "lock") >>= function
  | true -> failwith "should remove file"
  | false -> (
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
          OS.Dir.delete Fpath.(data2 / "context" / "index") >>= fun () ->
          run_reconstruct data2 logs2 >>= fun () ->
          let data3 = conf.data_dirs.(2) in
          let logs3 = conf.logs.(3) in
          OS.Cmd.run (cp data2 data3) >>= fun () ->
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
      | status ->
          Fmt.failwith "unexpected grep status %a" OS.Cmd.pp_status status)

let main data_dir tezos_node bootstrap continue _loop =
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
    }
  in
  Format.printf "run with data = %a\nresults =%a\nlogs = %a\n%!" Fpath.pp
    data_dirs.(0) Fpath.pp results_dir Fpath.pp logs.(0);
  string_of_int 0 |> run conf

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
  Arg.(value @@ opt int 10 doc)

let main_term =
  Term.(const main $ data_dir $ tezos_node $ bootstrap $ continue $ loop)

let () =
  let info = Term.info ~doc:"Run tezos nodes" "main" in
  Term.exit @@ Term.eval (main_term, info)

(* TODO add debug logs*)
