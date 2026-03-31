let default_port = 8080

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

let run args =
  Printf.printf "\n%s\n\n" (Terminal.bold "utopia prod");

  Terminal.print_step "Verifying build artifacts";
  let missing =
    Artifacts.missing_artifacts (Artifacts.required_server_artifacts ())
  in
  if missing <> [] then (
    Terminal.print_err
      "Missing required build artifacts. Run 'utopia build' first.";
    List.iter
      (fun artifact ->
        Printf.eprintf "    missing: %s\n%!"
          (Artifacts.artifact_display artifact))
      missing;
    exit 1);
  Terminal.print_done "Build artifacts verified";

  let port =
    match Sys.getenv_opt "PORT" with Some value -> value | None -> "8080"
  in
  let host =
    match Sys.getenv_opt "HOST" with Some value -> value | None -> "0.0.0.0"
  in
  let selected_port =
    parse_requested_port port |> select_available_port ~host
  in

  Terminal.print_step
    (Printf.sprintf "Starting production server on %s:%d" host selected_port);
  Printf.printf "\n";

  let server = Artifacts.generated_server_exe_ref () in
  let env =
    Process.child_env
      ~extra:[| "PORT=" ^ string_of_int selected_port; "HOST=" ^ host |]
      ()
  in
  let server_pid = ref None in
  let handle_signal _ =
    Printf.printf "\n%s\n%!" (Terminal.dim "  Shutting down...");
    (match !server_pid with
    | Some pid -> Process.kill_if_alive pid
    | None -> ());
    exit 0
  in
  Sys.set_signal Sys.sigint (Sys.Signal_handle handle_signal);
  Sys.set_signal Sys.sigterm (Sys.Signal_handle handle_signal);
  let pid =
    let server_path = Artifacts.artifact_path server in
    Unix.create_process_env server_path
      (Array.of_list (server_path :: args))
      env Unix.stdin Unix.stdout Unix.stderr
  in
  server_pid := Some pid;
  let _, status = Unix.waitpid [] pid in
  Process.unix_status_code status
