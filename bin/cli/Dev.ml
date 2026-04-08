let default_port = 8080
let restart_grace_seconds = 2.0

let build_env config port =
  let base = [| "PORT=" ^ string_of_int port; "HOST=" ^ config.Flags.host |] in
  let extras = if config.Flags.verbose then [||] else [| "NO_LOG=1" |] in
  Process.child_env ~extra:(Array.concat [ base; extras ]) ()

let parse_requested_port port_value =
  match Process.parse_port port_value with
  | Ok port -> port
  | Error message ->
      Terminal.print_warn (message ^ ", defaulting to 8080");
      default_port

let select_available_port ~host desired_port =
  match Process.first_available_port ~host desired_port with
  | Ok selected_port ->
      if selected_port <> desired_port then
        Terminal.print_warn
          (Printf.sprintf "Port %d is already in use on %s; using %d instead"
             desired_port host selected_port);
      selected_port
  | Error message ->
      Terminal.print_err message;
      exit 1

let print_ready host port =
  Printf.printf "\n  %s %s\n\n%!" (Terminal.cyan "Ready at")
    (Terminal.bold (Printf.sprintf "http://%s:%d" host port))

let wait_for_exit_with_timeout pid timeout_seconds =
  let open Lwt.Syntax in
  let wait_for_exit =
    Lwt.catch
      (fun () ->
        let* _pid, _status = Lwt_unix.waitpid [] pid in
        Lwt.return true)
      (fun _exn -> Lwt.return true)
  in
  let timeout =
    let* () = Lwt_unix.sleep timeout_seconds in
    Lwt.return false
  in
  Lwt.pick [ wait_for_exit; timeout ]

let run args =
  let config = Flags.parse_dev args in
  Printf.printf "\n%s\n\n" (Terminal.bold "utopia dev");

  Terminal.print_step "Running initial build bootstrap";
  if not (Artifacts.has_source_routes_directory ()) then (
    Terminal.print_err
      "Missing route source directory. Create 'app/' (preferred) or legacy \
       'pages/'.";
    exit 1);

  if not (Npm_preflight.ensure ~command_name:"utopia dev" ()) then exit 1;

  let compiler = Binaries.resolve_bin "utopia.compiler" in
  let code = Process.run_command compiler [ "--mode"; "development" ] in
  if code <> 0 then (
    Terminal.print_err "Initial compilation failed (see errors above)";
    exit code);
  Terminal.print_done "Initial compilation complete";

  let dune = Binaries.resolve_bin "dune" in
  Terminal.print_step "Building project";
  let code =
    Process.run_command dune
      (Artifacts.dune_build_args (Artifacts.generated_build_targets ()))
  in
  if code <> 0 then (
    Terminal.print_err "Initial dune build failed";
    exit code);
  Terminal.print_done "Project built";

  let generated_server = Artifacts.generated_server_exe_ref () in
  if not (Artifacts.artifact_exists generated_server) then (
    Terminal.print_err
      (Printf.sprintf
         "Generated server executable missing at %s. Re-run `utopia build` and \
          check dune errors."
         (Artifacts.artifact_display generated_server));
    exit 1);

  let selected_port = ref (parse_requested_port config.port) in
  selected_port := select_available_port ~host:config.host !selected_port;
  let watch_env = build_env config !selected_port in
  let spawn_generated_server () =
    selected_port := select_available_port ~host:config.host !selected_port;
    let server_path = Artifacts.artifact_path generated_server in
    let env = build_env config !selected_port in
    try Ok (Process.spawn server_path [ "--dev" ] env)
    with Unix.Unix_error (error, _, _) -> Error (Unix.error_message error)
  in

  let watch_pid =
    if config.no_watch then None
    else (
      Terminal.print_step "Starting dune watch (with RPC)";
      let pid =
        Process.spawn_silent dune
          ([ "build"; "-w" ] @ Artifacts.dune_root_args () @ [ "." ])
          watch_env
      in
      Some pid)
  in

  Terminal.print_step
    (Printf.sprintf "Starting dev server on %s:%d" config.host !selected_port);
  let server_pid =
    match spawn_generated_server () with
    | Ok pid -> ref pid
    | Error message ->
        Terminal.print_err
          (Printf.sprintf "Could not start generated dev server: %s" message);
        (match watch_pid with
        | Some pid -> Process.kill_if_alive pid
        | None -> ());
        exit 1
  in
  let server_mtime =
    ref (Process.file_mtime (Artifacts.artifact_path generated_server))
  in

  print_ready config.host !selected_port;

  let teardown () =
    Process.kill_if_alive !server_pid;
    (match watch_pid with Some pid -> Process.kill_if_alive pid | None -> ());
    try
      while true do
        ignore (Unix.waitpid [ Unix.WNOHANG ] (-1))
      done
    with Unix.Unix_error (Unix.ECHILD, _, _) -> ()
  in
  let handle_signal _ =
    Printf.printf "\n%s\n%!" (Terminal.dim "  Shutting down...");
    teardown ();
    exit 0
  in
  Sys.set_signal Sys.sigint (Sys.Signal_handle handle_signal);
  Sys.set_signal Sys.sigterm (Sys.Signal_handle handle_signal);

  let exit_code =
    if config.no_watch then
      let _, status = Unix.waitpid [] !server_pid in
      Process.unix_status_code status
    else
      Lwt_main.run
        (let open Lwt.Syntax in
         let rpc_task =
           Lwt.catch
             (fun () ->
               Build_rpc.run_loop
                 ~build_dir:
                   (Filename.concat
                      (Artifacts.workspace_root_string ())
                      (Fpath.to_string Artifacts.build_directory))
                 ~verbose:config.verbose ())
             (fun exn ->
               if config.verbose then
                 Terminal.print_warn
                   (Printf.sprintf "RPC error: %s" (Printexc.to_string exn));
               Lwt.return_unit)
         in
         let watch_monitor =
           match watch_pid with
           | Some pid ->
               let* _, status = Lwt_unix.waitpid [] pid in
               let code = Process.unix_status_code status in
               Terminal.print_err
                 (Printf.sprintf "Watch process exited with code %d" code);
               Lwt.return code
           | None ->
               let waiter, _wakener = Lwt.wait () in
               waiter
         in
         let server_monitor =
           let rec loop last_mtime =
             let* pid, status = Lwt_unix.waitpid [ Unix.WNOHANG ] !server_pid in
             if pid = !server_pid then (
               let code = Process.unix_status_code status in
               Terminal.print_err
                 (Printf.sprintf "Server exited with code %d" code);
               Lwt.return code)
             else
               let next_mtime =
                 Process.file_mtime (Artifacts.artifact_path generated_server)
               in
               match (last_mtime, next_mtime) with
               | Some previous, Some current
                 when not (Float.equal previous current) -> (
                   Terminal.print_step "Restarting generated dev server";
                   let previous_port = !selected_port in
                   Process.kill_if_alive !server_pid;
                   let* exited_after_term =
                     wait_for_exit_with_timeout !server_pid
                       restart_grace_seconds
                   in
                   let* () =
                     if exited_after_term then Lwt.return_unit
                     else (
                       Terminal.print_warn
                         "Generated dev server did not exit in time; forcing \
                          kill";
                       Process.force_kill_if_alive !server_pid;
                       let* _ =
                         Lwt.catch
                           (fun () -> Lwt_unix.waitpid [] !server_pid)
                           (fun _exn -> Lwt.return (-1, Unix.WEXITED 0))
                       in
                       Lwt.return_unit)
                   in
                   match spawn_generated_server () with
                   | Ok pid ->
                       server_pid := pid;
                       server_mtime := Some current;
                       if !selected_port = previous_port then
                         Terminal.print_done "Generated dev server restarted"
                       else (
                         Terminal.print_done
                           (Printf.sprintf
                              "Generated dev server restarted on %s:%d"
                              config.host !selected_port);
                         print_ready config.host !selected_port);
                       let* () = Lwt_unix.sleep 0.5 in
                       loop (Some current)
                   | Error message ->
                       Terminal.print_err
                         (Printf.sprintf
                            "Could not restart generated dev server: %s" message);
                       Lwt.return 1)
               | Some _, None ->
                   Terminal.print_err
                     (Printf.sprintf
                        "Generated server executable disappeared at %s"
                        (Artifacts.artifact_display generated_server));
                   Lwt.return 1
               | _, Some current ->
                   server_mtime := Some current;
                   let* () = Lwt_unix.sleep 0.5 in
                   loop (Some current)
               | _ ->
                   let* () = Lwt_unix.sleep 0.5 in
                   loop last_mtime
           in
           loop !server_mtime
         in
         let* code =
           Lwt.pick
             [
               server_monitor;
               watch_monitor;
               (let* () = rpc_task in
                let waiter, _wakener = Lwt.wait () in
                waiter);
             ]
         in
         Lwt.return code)
  in
  teardown ();
  exit_code
