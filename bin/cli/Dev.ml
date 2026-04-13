let default_port = 8080
let restart_grace_seconds = 2.0

let generate_dev_token () =
  let buf = Buffer.create 64 in
  Random.self_init ();
  for _ = 1 to 32 do
    Buffer.add_string buf (Printf.sprintf "%02x" (Random.int 256))
  done;
  Buffer.contents buf

let json_escape s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string buf {|\"|}
      | '\\' -> Buffer.add_string buf {|\\|}
      | '\n' -> Buffer.add_string buf {|\n|}
      | '\r' -> Buffer.add_string buf {|\r|}
      | '\t' -> Buffer.add_string buf {|\t|}
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let format_diagnostic_json (d : Build_rpc.structured_diagnostic) =
  let loc =
    match d.location with
    | Some l -> Printf.sprintf {|,"location":"%s"|} (json_escape l)
    | None -> ""
  in
  let targets =
    d.targets
    |> List.map (fun t -> Printf.sprintf {|"%s"|} (json_escape t))
    |> String.concat ","
  in
  Printf.sprintf {|{"severity":"%s","message":"%s"%s,"targets":[%s]}|}
    (json_escape d.severity) (json_escape d.message) loc targets

let format_build_event ~build_id ~status ~rebuilding ?(errors = [])
    ?(warnings = []) () =
  let errors_json =
    errors |> List.map format_diagnostic_json |> String.concat ","
  in
  let warnings_json =
    warnings |> List.map format_diagnostic_json |> String.concat ","
  in
  Printf.sprintf
    {|{"kind":"build_state","build_id":%d,"status":"%s","rebuilding":%s,"errors":[%s],"warnings":[%s]}|}
    build_id status
    (if rebuilding then "true" else "false")
    errors_json warnings_json

let post_dev_event ~host ~port ~token body =
  let open Lwt.Syntax in
  Lwt.catch
    (fun () ->
      let addr = Unix.ADDR_INET (Unix.inet_addr_of_string host, port) in
      let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Lwt.finalize
        (fun () ->
          let* () = Lwt_unix.connect fd addr in
          let request =
            Printf.sprintf
              "POST /_utopia/dev-events HTTP/1.1\r\n\
               Host: %s:%d\r\n\
               Authorization: Bearer %s\r\n\
               Content-Type: application/json\r\n\
               Content-Length: %d\r\n\
               Connection: close\r\n\
               \r\n\
               %s"
              host port token (String.length body) body
          in
          let bytes = Bytes.of_string request in
          let* _n = Lwt_unix.write fd bytes 0 (Bytes.length bytes) in
          Lwt.return_unit)
        (fun () -> Lwt_unix.close fd))
    (fun _exn -> Lwt.return_unit)

let build_env ~dev_token config port =
  let base =
    [|
      "PORT=" ^ string_of_int port;
      "HOST=" ^ config.Flags.host;
      "UTOPIA_DEV_TOKEN=" ^ dev_token;
    |]
  in
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
    Terminal.print_err "Missing route source directory. Create 'app/'.";
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
  (* Build server executable — required for the dev server to start. *)
  let code =
    Process.run_command dune
      (Artifacts.dune_build_args [ Artifacts.generated_server_build_target () ])
  in
  if code <> 0 then (
    Terminal.print_err "Initial dune build failed";
    exit code);
  Terminal.print_done "Project built";

  (* Build client-side bundles (melange + esbuild) — best-effort.
     This produces the dev overlay JS; if it fails (e.g. melange
     library compat issues) the server still works without it. *)
  let esbuild_code =
    Process.run_command dune
      (Artifacts.dune_build_args
         [ Artifacts.generated_esbuild_build_target () ])
  in
  if esbuild_code = 0 then Terminal.print_done "Client bundles built"
  else
    Terminal.print_warn
      "Client bundle build failed (dev overlay will not be available)";

  let generated_server = Artifacts.generated_server_exe_ref () in
  if not (Artifacts.artifact_exists generated_server) then (
    Terminal.print_err
      (Printf.sprintf
         "Generated server executable missing at %s. Re-run `utopia build` and \
          check dune errors."
         (Artifacts.artifact_display generated_server));
    exit 1);

  let dev_token = generate_dev_token () in
  let selected_port = ref (parse_requested_port config.port) in
  selected_port := select_available_port ~host:config.host !selected_port;
  let watch_env = build_env ~dev_token config !selected_port in
  let spawn_generated_server () =
    selected_port := select_available_port ~host:config.host !selected_port;
    let server_path = Artifacts.artifact_path generated_server in
    let env = build_env ~dev_token config !selected_port in
    try Ok (Process.spawn server_path [ "--dev" ] env)
    with Unix.Unix_error (error, _, _) -> Error (Unix.error_message error)
  in

  let watch_pid =
    if config.no_watch then None
    else (
      Terminal.print_step "Starting dune watch (with RPC)";
      (* Build the default alias (.), the server executable, and the
         esbuild alias explicitly.  _utopia/ is data_only_dirs so
         (alias_rec all) from the parent does not recurse into it.
         The explicit targets ensure dune watch rebuilds both the
         server and client bundles when source files change. *)
      let pid =
        Process.spawn_silent dune
          ([ "build"; "-w" ]
          @ Artifacts.dune_root_args ()
          @ [
              ".";
              Artifacts.generated_server_build_target ();
              Artifacts.generated_esbuild_build_target ();
            ])
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
         let build_id = ref 0 in
         let dev_hooks : Build_rpc.lifecycle_hooks =
           {
             build_started =
               (fun () ->
                 incr build_id;
                 let body =
                   format_build_event ~build_id:!build_id ~status:"building"
                     ~rebuilding:true ()
                 in
                 Lwt.async (fun () ->
                     post_dev_event ~host:config.host ~port:!selected_port
                       ~token:dev_token body));
             build_failed =
               (fun diagnostics ->
                 let structured =
                   List.map Build_rpc.structured_of_diagnostic diagnostics
                 in
                 let errors =
                   List.filter
                     (fun (d : Build_rpc.structured_diagnostic) ->
                       d.severity = "error")
                     structured
                 in
                 let warnings =
                   List.filter
                     (fun (d : Build_rpc.structured_diagnostic) ->
                       d.severity = "warning")
                     structured
                 in
                 let body =
                   format_build_event ~build_id:!build_id ~status:"failed"
                     ~rebuilding:false ~errors ~warnings ()
                 in
                 Lwt.async (fun () ->
                     post_dev_event ~host:config.host ~port:!selected_port
                       ~token:dev_token body));
             build_succeeded =
               (fun () ->
                 (* Server restart handles reload via SSE disconnect/reconnect.
                    Post a healthy state so the browser knows the build succeeded. *)
                 let body =
                   format_build_event ~build_id:!build_id ~status:"healthy"
                     ~rebuilding:false ()
                 in
                 Lwt.async (fun () ->
                     post_dev_event ~host:config.host ~port:!selected_port
                       ~token:dev_token body));
           }
         in
         let rpc_task =
           let build_dir =
             Filename.concat
               (Artifacts.workspace_root_string ())
               (Fpath.to_string Artifacts.build_directory)
           in
           let rec rpc_loop () =
             Build_rpc.clear_active_diagnostics ();
             let* () =
               Lwt.catch
                 (fun () ->
                   Build_rpc.run_loop ~hooks:dev_hooks ~build_dir
                     ~verbose:config.verbose ())
                 (fun exn ->
                   if config.verbose then
                     Terminal.print_warn
                       (Printf.sprintf "RPC error: %s" (Printexc.to_string exn));
                   Lwt.return_unit)
             in
             if config.verbose then
               Terminal.print_warn "Dune RPC disconnected, reconnecting…";
             let* () = Lwt_unix.sleep 1.0 in
             rpc_loop ()
           in
           rpc_loop ()
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
                   (* The executable is temporarily missing — dune removes
                       stale artifacts when a rebuild fails. Keep polling;
                       the file will reappear once the build succeeds. *)
                   let* () = Lwt_unix.sleep 0.5 in
                   loop last_mtime
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
                (* rpc_loop reconnects indefinitely; this is only
                   reachable if Lwt cancels the promise. *)
                let waiter, _wakener = Lwt.wait () in
                waiter);
             ]
         in
         Lwt.return code)
  in
  teardown ();
  exit_code
