let version = "0.1.0"

let is_tty = Unix.isatty Unix.stdout

let bold text = if is_tty then "\027[1m" ^ text ^ "\027[0m" else text
let dim text = if is_tty then "\027[2m" ^ text ^ "\027[0m" else text
let green text = if is_tty then "\027[32m" ^ text ^ "\027[0m" else text
let red text = if is_tty then "\027[31m" ^ text ^ "\027[0m" else text
let cyan text = if is_tty then "\027[36m" ^ text ^ "\027[0m" else text
let yellow text = if is_tty then "\027[33m" ^ text ^ "\027[0m" else text

let print_step label = Printf.printf "  %s %s\n%!" (green "▸") label
let print_done label = Printf.printf "  %s %s\n%!" (green "✓") label
let print_warn label = Printf.printf "  %s %s\n%!" (yellow "⚠") label
let print_err label = Printf.eprintf "  %s %s\n%!" (red "✗") label

let run_command program args =
  let pid =
    Unix.create_process program (Array.of_list (program :: args)) Unix.stdin
      Unix.stdout Unix.stderr
  in
  let _, status = Unix.waitpid [] pid in
  match status with
  | Unix.WEXITED code -> code
  | Unix.WSIGNALED signal ->
      Printf.eprintf "Process %s killed by signal %d\n%!" program signal;
      128 + signal
  | Unix.WSTOPPED signal ->
      Printf.eprintf "Process %s stopped by signal %d\n%!" program signal;
      128 + signal

let run_command_capture program args =
  try
    let read_end, write_end = Unix.pipe () in
    let pid =
      Unix.create_process program (Array.of_list (program :: args)) Unix.stdin
        write_end write_end
    in
    Unix.close write_end;
    let channel = Unix.in_channel_of_descr read_end in
    let buffer = Buffer.create 256 in
    (try
       while true do
         Buffer.add_char buffer (input_char channel)
       done
     with End_of_file -> ());
    close_in channel;
    let _, status = Unix.waitpid [] pid in
    let output = Buffer.contents buffer |> String.trim in
    match status with
    | Unix.WEXITED 0 -> Some output
    | _ -> None
  with Unix.Unix_error _ -> None

(* Map public names to the dune build-directory executable names.
   During development (dune exec / dune build), binaries live next to
   cli.exe under _build/default/bin/ with their dune (name ...) plus
   ".exe" suffix. After `dune install`, they appear on PATH under
   their (public_name ...). *)
let dune_exe_name = function
  | "utopia.compiler" -> Some "compiler.exe"
  | "utopia.server" -> Some "Server.exe"
  | "dune" -> None (* always on PATH *)
  | _ -> None

(* Resolve a sibling binary. Order:
   1. Sibling in the same directory as our own executable (dune build layout).
   2. `which` lookup (installed or on PATH).
   3. Bare name (let create_process search PATH). *)
let resolve_bin name =
  let self_dir =
    try Filename.dirname Sys.executable_name
    with _ -> ""
  in
  (* Try dune-name sibling first *)
  let sibling_by_dune =
    match dune_exe_name name with
    | Some exe_name ->
        let path = Filename.concat self_dir exe_name in
        if self_dir <> "" && Sys.file_exists path then Some path else None
    | None -> None
  in
  match sibling_by_dune with
  | Some path -> path
  | None ->
      (* Try public-name sibling *)
      let sibling = Filename.concat self_dir name in
      if self_dir <> "" && Sys.file_exists sibling then sibling
      else
        match run_command_capture "which" [ name ] with
        | Some path when path <> "" -> path
        | _ -> name

let file_exists path = Sys.file_exists path
let is_directory path = Sys.file_exists path && Sys.is_directory path

let rec remove_recursive path =
  if Sys.is_directory path then (
    Sys.readdir path
    |> Array.iter (fun entry -> remove_recursive (Filename.concat path entry));
    Unix.rmdir path)
  else Sys.remove path

let remove_if_exists path =
  if file_exists path then (
    (try remove_recursive path
     with exn ->
       print_warn
         (Printf.sprintf "Could not fully remove %s: %s" path
            (Printexc.to_string exn)));
    true)
  else false

let routes_manifest_path = "_utopia/routes.manifest"

let read_file path =
  In_channel.with_open_bin path (fun ch -> In_channel.input_all ch)

let count_routes () =
  if not (file_exists routes_manifest_path) then 0
  else
    let content = read_file routes_manifest_path in
    content |> String.split_on_char '\n'
    |> List.filter (fun line -> String.trim line <> "")
    |> List.length

let print_build_report () =
  let num_routes = count_routes () in
  Printf.printf "\n%s\n" (bold "  Build report");
  Printf.printf "  Routes:     %d\n" num_routes;
  if file_exists "_utopia/dune" then
    Printf.printf "  Generated:  _utopia/dune\n";
  if file_exists routes_manifest_path then
    Printf.printf "  Manifest:   %s\n" routes_manifest_path;
  if file_exists "_utopia/scripts.manifest" then
    Printf.printf "  Scripts:    _utopia/scripts.manifest\n";
  Printf.printf "  Output:     _build/\n%!";
  print_newline ()

let cmd_build _args =
  Printf.printf "\n%s\n\n" (bold "utopia build");

  (* 1. Validate project shape *)
  print_step "Validating project structure";
  if not (is_directory "pages") then (
    print_err "Missing 'pages' directory. Create it and add page files.";
    exit 1);
  print_done "Project structure valid";

  (* 2. Run the compiler to generate route manifest + dune rules *)
  print_step "Generating route manifest and dune rules";
  let compiler = resolve_bin "utopia.compiler" in
  let code = run_command compiler [] in
  if code <> 0 then (
    print_err "Compiler failed (see errors above)";
    exit code);
  print_done "Route manifest and dune rules generated";

  (* 3. Build server + melange client outputs *)
  print_step "Building server and client outputs";
  let dune = resolve_bin "dune" in
  let code = run_command dune [ "build"; "." ] in
  if code <> 0 then (
    print_err "dune build failed";
    exit code);
  print_done "Build complete";

  (* 4. Emit build report *)
  print_build_report ();
  0

let cmd_prod args =
  Printf.printf "\n%s\n\n" (bold "utopia prod");

  (* 1. Verify required build artifacts exist *)
  print_step "Verifying build artifacts";
  let missing = ref [] in
  if not (file_exists routes_manifest_path) then
    missing := routes_manifest_path :: !missing;
  if not (file_exists "_utopia/dune") then
    missing := "_utopia/dune" :: !missing;
  if !missing <> [] then (
    print_err "Missing required build artifacts. Run 'utopia build' first.";
    List.iter (fun path -> Printf.eprintf "    missing: %s\n%!" path) !missing;
    exit 1);
  print_done "Build artifacts verified";

  (* 2. Resolve PORT and HOST from env/flags *)
  let port =
    match Sys.getenv_opt "PORT" with
    | Some p -> p
    | None -> "8080"
  in
  let host =
    match Sys.getenv_opt "HOST" with
    | Some h -> h
    | None -> "0.0.0.0"
  in

  (* 3. Start production server *)
  print_step
    (Printf.sprintf "Starting production server on %s:%s" host port);
  Printf.printf "\n";

  (* Start the server as a subprocess instead of exec, so we can report
     the exit code back to the caller. *)
  let server = resolve_bin "utopia.server" in
  let env =
    Array.append (Unix.environment ())
      [| "PORT=" ^ port; "HOST=" ^ host |]
  in
  let pid =
    Unix.create_process_env server (Array.of_list (server :: args)) env
      Unix.stdin Unix.stdout Unix.stderr
  in
  let _, status = Unix.waitpid [] pid in
  match status with
  | Unix.WEXITED code -> code
  | Unix.WSIGNALED s -> 128 + s
  | Unix.WSTOPPED s -> 128 + s

(* Simple prefix-based arg parser for dev flags *)
type dev_config = {
  port : string;
  host : string;
  no_watch : bool;
  verbose : bool;
}

let default_dev_config =
  {
    port =
      (match Sys.getenv_opt "PORT" with
      | Some p -> p
      | None -> "8080");
    host =
      (match Sys.getenv_opt "HOST" with
      | Some h -> h
      | None -> "127.0.0.1");
    no_watch = false;
    verbose = false;
  }

let parse_dev_args args =
  let rec loop config = function
    | [] -> config
    | "--port" :: value :: rest -> loop { config with port = value } rest
    | "--host" :: value :: rest -> loop { config with host = value } rest
    | "--no-watch" :: rest -> loop { config with no_watch = true } rest
    | "--verbose" :: rest -> loop { config with verbose = true } rest
    | unknown :: _ ->
        Printf.eprintf "Unknown dev flag: %s\n%!" unknown;
        exit 1
  in
  loop default_dev_config args

(* Spawn a subprocess, return its pid *)
let spawn program args env =
  Unix.create_process_env program (Array.of_list (program :: args)) env
    Unix.stdin Unix.stdout Unix.stderr

(* Spawn a subprocess with stdout/stderr sent to /dev/null *)
let spawn_silent program args env =
  let dev_null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  let pid =
    Unix.create_process_env program (Array.of_list (program :: args)) env
      Unix.stdin dev_null dev_null
  in
  Unix.close dev_null;
  pid

let kill_if_alive pid =
  try Unix.kill pid Sys.sigterm
  with Unix.Unix_error (Unix.ESRCH, _, _) -> ()

module Rpc = Dune_rpc_lwt.V1
module V1 = Dune_rpc.V1

(* Render a Pp.t diagnostic message to a plain string *)
let pp_to_string pp_doc =
  let buffer = Buffer.create 256 in
  let fmt = Format.formatter_of_buffer buffer in
  Pp.to_fmt fmt (Pp.map_tags pp_doc ~f:(fun _ -> ()));
  Format.pp_print_flush fmt ();
  Buffer.contents buffer

(* Format a diagnostic location as "file:line:col" *)
let format_loc loc =
  let start = V1.Loc.start loc in
  Printf.sprintf "%s:%d:%d" start.pos_fname start.pos_lnum
    (start.pos_cnum - start.pos_bol)

(* Pretty-print a single diagnostic *)
let format_diagnostic diag =
  let severity_str =
    match V1.Diagnostic.severity diag with
    | Some V1.Diagnostic.Error -> red "error"
    | Some V1.Diagnostic.Warning -> yellow "warning"
    | None -> dim "note"
  in
  let loc_str =
    match V1.Diagnostic.loc diag with
    | Some loc -> format_loc loc ^ ": "
    | None -> ""
  in
  let message = V1.Diagnostic.message diag |> pp_to_string in
  let targets =
    V1.Diagnostic.targets diag
    |> List.filter_map (fun target ->
           match target with
           | V1.Target.Path p -> Some p
           | V1.Target.Alias a -> Some (Printf.sprintf "(alias %s)" a)
           | V1.Target.Library l -> Some (Printf.sprintf "(library %s)" l)
           | V1.Target.Executables es ->
               Some (Printf.sprintf "(executables %s)" (String.concat " " es))
           | V1.Target.Preprocess ps ->
               Some (Printf.sprintf "(preprocess %s)" (String.concat " " ps))
           | V1.Target.Loc loc -> Some (format_loc loc))
  in
  let target_str =
    match targets with
    | [] -> ""
    | ts -> dim (Printf.sprintf " [%s]" (String.concat " -> " ts))
  in
  Printf.sprintf "    %s%s: %s%s" loc_str severity_str
    (String.trim message) target_str

(* Format a progress update *)
let format_progress = function
  | V1.Progress.Waiting -> dim "waiting"
  | V1.Progress.In_progress { complete; remaining; failed } ->
      let failed_str =
        if failed > 0 then Printf.sprintf ", %s failed" (red (string_of_int failed))
        else ""
      in
      Printf.sprintf "%d/%d%s" complete (complete + remaining) failed_str
  | V1.Progress.Failed -> red "failed"
  | V1.Progress.Interrupted -> yellow "interrupted"
  | V1.Progress.Success -> green "done"

(* Wait for the dune RPC socket to become available, with retries *)
let wait_for_rpc_socket ~build_dir ~max_retries ~delay_ms =
  let open Lwt.Syntax in
  let rec loop n =
    if n >= max_retries then Lwt.return_none
    else
      let* result =
        Lwt.catch
          (fun () ->
            let where = Rpc.Where.default ~build_dir () in
            let* chan = Rpc.connect_chan where in
            Lwt.return_some chan)
          (fun _exn ->
            let* () = Lwt_unix.sleep (Float.of_int delay_ms /. 1000.0) in
            loop (n + 1))
      in
      Lwt.return result
  in
  loop 0

(* Active diagnostics set, keyed by id *)
let active_diagnostics : (int, V1.Diagnostic.t) Hashtbl.t = Hashtbl.create 64

let diagnostic_id_to_int diag =
  V1.Diagnostic.Id.hash (V1.Diagnostic.id diag)

let print_active_diagnostics () =
  let diags = Hashtbl.fold (fun _ d acc -> d :: acc) active_diagnostics [] in
  if diags <> [] then (
    Printf.printf "\n";
    diags
    |> List.iter (fun diag ->
           Printf.printf "%s\n%!" (format_diagnostic diag));
    Printf.printf "\n%!")

(* Run the RPC event loop: subscribe to progress and diagnostics *)
let run_rpc_loop ~build_dir ~verbose =
  let open Lwt.Syntax in
  let init =
    V1.Initialize.create ~id:(V1.Id.make (Csexp.Atom "utopia-dev"))
  in
  let* chan_opt = wait_for_rpc_socket ~build_dir ~max_retries:50 ~delay_ms:200 in
  match chan_opt with
  | None ->
      print_warn "Could not connect to dune RPC (build watch may not support RPC)";
      (* Fall through — the watch process still runs, we just don't get
         structured progress. Block until cancelled. *)
      let waiter, _wakener = Lwt.wait () in
      waiter
  | Some chan ->
      if verbose then print_done "Connected to dune RPC";
      let handler =
        Rpc.Client.Handler.create
          ~log:(fun msg ->
            if verbose then
              Printf.printf "  %s %s\n%!" (dim "rpc")
                (V1.Message.message msg);
            Lwt.return_unit)
          ()
      in
      Rpc.Client.connect ~handler chan init ~f:(fun client ->
          (* Subscribe to progress *)
          let* progress_result =
            Rpc.Client.poll client V1.Sub.progress
          in
          let progress_stream =
            match progress_result with
            | Ok stream -> Some stream
            | Error _err ->
                print_warn "Could not subscribe to build progress";
                None
          in
          (* Subscribe to diagnostics *)
          let* diag_result =
            Rpc.Client.poll client V1.Sub.diagnostic
          in
          let diag_stream =
            match diag_result with
            | Ok stream -> Some stream
            | Error _err ->
                print_warn "Could not subscribe to diagnostics";
                None
          in
          (* Progress polling loop *)
          let progress_loop =
            match progress_stream with
            | None ->
                let waiter, _wakener = Lwt.wait () in
                waiter
            | Some stream ->
                let last_status = ref "" in
                let rec loop () =
                  let* event = Rpc.Client.Stream.next stream in
                  match event with
                  | None -> Lwt.return_unit
                  | Some progress ->
                      let status = format_progress progress in
                      if status <> !last_status then (
                        last_status := status;
                        (match progress with
                        | V1.Progress.Success ->
                            Printf.printf "  %s Build %s\n%!" (green "✓") status
                        | V1.Progress.Failed ->
                            Printf.printf "  %s Build %s\n%!" (red "✗") status;
                            print_active_diagnostics ()
                        | V1.Progress.Interrupted ->
                            Printf.printf "  %s Build %s\n%!" (yellow "⚠") status
                        | V1.Progress.In_progress _ ->
                            Printf.printf "  %s Build %s\n%!" (cyan "▸") status
                        | V1.Progress.Waiting ->
                            Printf.printf "  %s Build %s\n%!" (dim "◌") status));
                      loop ()
                in
                loop ()
          in
          (* Diagnostic polling loop *)
          let diagnostic_loop =
            match diag_stream with
            | None ->
                let waiter, _wakener = Lwt.wait () in
                waiter
            | Some stream ->
                let rec loop () =
                  let* event = Rpc.Client.Stream.next stream in
                  match event with
                  | None -> Lwt.return_unit
                  | Some events ->
                      events |> List.iter (fun event ->
                        match event with
                        | V1.Diagnostic.Event.Add diag ->
                            let key = diagnostic_id_to_int diag in
                            Hashtbl.replace active_diagnostics key diag;
                            if verbose then
                              Printf.printf "%s\n%!" (format_diagnostic diag)
                        | V1.Diagnostic.Event.Remove diag ->
                            let key = diagnostic_id_to_int diag in
                            Hashtbl.remove active_diagnostics key);
                      loop ()
                in
                loop ()
          in
          (* Disconnection sentinel *)
          let disconnected =
            let* () = Rpc.Client.disconnected client in
            if verbose then
              print_warn "Dune RPC disconnected";
            Lwt.return_unit
          in
          (* Race all three — whichever finishes first *)
          Lwt.pick [ progress_loop; diagnostic_loop; disconnected ])

let cmd_dev args =
  let config = parse_dev_args args in
  Printf.printf "\n%s\n\n" (bold "utopia dev");

  (* 1. Initial compile/build bootstrap *)
  print_step "Running initial build bootstrap";
  if not (is_directory "pages") then (
    print_err "Missing 'pages' directory. Create it and add page files.";
    exit 1);

  let compiler = resolve_bin "utopia.compiler" in
  let code = run_command compiler [] in
  if code <> 0 then (
    print_err "Initial compilation failed (see errors above)";
    exit code);
  print_done "Initial compilation complete";

  (* Run a quick dune build to ensure artifacts are ready *)
  let dune = resolve_bin "dune" in
  print_step "Building project";
  let code = run_command dune [ "build"; "." ] in
  if code <> 0 then (
    print_err "Initial dune build failed";
    exit code);
  print_done "Project built";

  (* 2. Build environment for subprocesses *)
  let env =
    let base =
      [| "PORT=" ^ config.port; "HOST=" ^ config.host |]
    in
    (* In dev mode, show request logs only when --verbose is passed *)
    let extras =
      if config.verbose then [||]
      else [| "NO_LOG=1" |]
    in
    Array.concat [ Unix.environment (); base; extras ]
  in

  (* 3. Start dune watch (enables RPC server) *)
  let watch_pid =
    if config.no_watch then None
    else (
      print_step "Starting dune watch (with RPC)";
      (* Silence dune's own stdout/stderr — we get structured output via RPC *)
      let pid = spawn_silent dune [ "build"; "-w"; "." ] env in
      Some pid)
  in

  (* 4. Start local server *)
  print_step
    (Printf.sprintf "Starting dev server on %s:%s" config.host config.port);
  let server = resolve_bin "utopia.server" in
  let server_pid = spawn server [] env in

  Printf.printf "\n  %s %s\n\n%!"
    (cyan "Ready at")
    (bold (Printf.sprintf "http://%s:%s" config.host config.port));

  (* 5. Setup teardown *)
  let teardown () =
    kill_if_alive server_pid;
    (match watch_pid with Some pid -> kill_if_alive pid | None -> ());
    (* Reap children *)
    (try
       while true do
         ignore (Unix.waitpid [ Unix.WNOHANG ] (-1))
       done
     with Unix.Unix_error (Unix.ECHILD, _, _) -> ())
  in
  let handle_signal _ =
    Printf.printf "\n%s\n%!" (dim "  Shutting down...");
    teardown ();
    exit 0
  in
  Sys.set_signal Sys.sigint (Sys.Signal_handle handle_signal);
  Sys.set_signal Sys.sigterm (Sys.Signal_handle handle_signal);

  (* 6. Run the RPC event loop + wait for subprocess exit in parallel *)
  let exit_code =
    if config.no_watch then (
      (* Without watch, just wait for server to exit *)
      let _, status = Unix.waitpid [] server_pid in
      match status with
      | Unix.WEXITED code -> code
      | Unix.WSIGNALED s -> 128 + s
      | Unix.WSTOPPED s -> 128 + s)
    else
      (* With watch: run the Lwt RPC loop alongside process monitoring *)
      Lwt_main.run
        (let open Lwt.Syntax in
         (* RPC event loop (best-effort — if RPC fails, we keep running) *)
         let rpc_task =
           Lwt.catch
             (fun () -> run_rpc_loop ~build_dir:"_build" ~verbose:config.verbose)
             (fun exn ->
               if config.verbose then
                 print_warn
                   (Printf.sprintf "RPC error: %s" (Printexc.to_string exn));
               Lwt.return_unit)
         in
         (* Monitor child processes via Lwt *)
         let process_monitor =
           let rec wait_loop () =
             let* result =
               Lwt.catch
                 (fun () ->
                   let* pid, status = Lwt_unix.waitpid [] (-1) in
                   Lwt.return_some (pid, status))
                 (fun _exn -> Lwt.return_none)
             in
             match result with
             | None -> Lwt.return 1
             | Some (pid, status) ->
                 let code =
                   match status with
                   | Unix.WEXITED c -> c
                   | Unix.WSIGNALED s -> 128 + s
                   | Unix.WSTOPPED s -> 128 + s
                 in
                 if pid = server_pid then (
                   print_err
                     (Printf.sprintf "Server exited with code %d" code);
                   Lwt.return code)
                 else
                   let is_watch =
                     match watch_pid with
                     | Some wp -> wp = pid
                     | None -> false
                   in
                   if is_watch then (
                     print_err
                       (Printf.sprintf "Watch process exited with code %d" code);
                     Lwt.return code)
                   else wait_loop ()
           in
           wait_loop ()
         in
         (* Pick: whichever resolves first determines exit *)
         let* code =
           Lwt.pick
             [
               process_monitor;
               (let* () = rpc_task in
                (* RPC loop ended but processes still running — keep waiting *)
                let waiter, _wakener = Lwt.wait () in
                waiter);
             ]
         in
         Lwt.return code)
  in
  teardown ();
  exit_code

let cmd_clean _args =
  Printf.printf "\n%s\n\n" (bold "utopia clean");
  let removed = ref [] in

  (* Remove _build *)
  if remove_if_exists "_build" then
    removed := "_build" :: !removed;

  (* Remove _utopia generated artifacts *)
  if remove_if_exists "_utopia" then
    removed := "_utopia" :: !removed;

  (* Run dune clean as well to handle anything we missed *)
  let dune = resolve_bin "dune" in
  let code = run_command dune [ "clean" ] in
  if code <> 0 then
    print_warn "dune clean returned non-zero (this is usually harmless)";

  (* Report *)
  if !removed = [] then
    print_done "Nothing to clean"
  else (
    List.iter
      (fun path -> print_done (Printf.sprintf "Removed %s" path))
      (List.rev !removed);
    print_done "Clean complete");
  Printf.printf "\n";
  0

let version_of program args =
  try
    match run_command_capture program args with
    | Some output -> output
    | None -> dim "not found"
  with Unix.Unix_error (Unix.ENOENT, _, _) -> dim "not found"

let cmd_info _args =
  Printf.printf "\n%s\n\n" (bold "utopia info");

  (* Tool versions *)
  Printf.printf "  %s\n" (bold "Versions");
  Printf.printf "    utopia:     %s\n" version;
  Printf.printf "    ocaml:      %s\n"
    (version_of "ocaml" [ "-vnum" ]);
  Printf.printf "    dune:       %s\n"
    (version_of "dune" [ "--version" ]);
  Printf.printf "    melange:    %s\n"
    (version_of "melange" [ "--version" ]);
  Printf.printf "    reason:     %s\n"
    (version_of "refmt" [ "--version" ]);

  (* Project paths *)
  Printf.printf "\n  %s\n" (bold "Project");
  let cwd = Sys.getcwd () in
  Printf.printf "    root:       %s\n" cwd;
  Printf.printf "    pages:      %s\n"
    (if is_directory "pages" then green "found" else red "missing");
  Printf.printf "    _utopia:    %s\n"
    (if is_directory "_utopia" then green "generated" else dim "not generated");
  Printf.printf "    _build:     %s\n"
    (if is_directory "_build" then green "present" else dim "not present");

  (* Route stats *)
  let num_routes = count_routes () in
  if num_routes > 0 then
    Printf.printf "    routes:     %d\n" num_routes;

  (* Command status *)
  Printf.printf "\n  %s\n" (bold "Commands");
  let commands_status =
    [
      ("build", true);
      ("prod", true);
      ("dev", true);
      ("clean", true);
      ("info", true);
    ]
  in
  List.iter
    (fun (name, implemented) ->
      let status =
        if implemented then green "implemented" else yellow "scaffolded"
      in
      Printf.printf "    %-10s  %s\n" name status)
    commands_status;

  Printf.printf "\n";
  0

type command = {
  name : string;
  aliases : string list;
  description : string;
  run : string list -> int;
}

let commands =
  [
    {
      name = "build";
      aliases = [];
      description = "Build production artifacts";
      run = cmd_build;
    };
    {
      name = "dev";
      aliases = [];
      description = "Start development workflow";
      run = cmd_dev;
    };
    {
      name = "prod";
      aliases = [ "start" ];
      description = "Run built server in production mode";
      run = cmd_prod;
    };
    {
      name = "clean";
      aliases = [];
      description = "Remove generated artifacts";
      run = cmd_clean;
    };
    {
      name = "info";
      aliases = [];
      description = "Print environment and project metadata";
      run = cmd_info;
    };
  ]

let basename path =
  let len = String.length path in
  let rec scan index =
    if index < 0 then path
    else if path.[index] = '/' then
      String.sub path (index + 1) (len - index - 1)
    else scan (index - 1)
  in
  scan (len - 1)

let find_command name =
  let lower = String.lowercase_ascii name in
  List.find_opt
    (fun cmd ->
      cmd.name = lower
      || List.exists (fun alias -> alias = lower) cmd.aliases)
    commands

let print_usage () =
  Printf.printf "%s\n\n" (bold "utopia <command> [options]");
  Printf.printf "Commands:\n";
  List.iter
    (fun cmd ->
      let alias_text =
        match cmd.aliases with
        | [] -> ""
        | values ->
            Printf.sprintf " %s" (dim (Printf.sprintf "(alias: %s)" (String.concat ", " values)))
      in
      Printf.printf "  %-10s %s%s\n" (cyan cmd.name) cmd.description
        alias_text)
    commands;
  Printf.printf
    "\nShortcuts work as executable aliases, e.g. %s.\n"
    (cyan "utopia-build");
  Printf.printf "\nGlobal flags:\n";
  Printf.printf "  %s      Show this help\n" (cyan "-h, --help");
  Printf.printf "  %s   Print CLI version\n" (cyan "-v, --version")

let command_from_executable argv0 =
  let name = basename argv0 in
  if String.length name > 7 && String.sub name 0 7 = "utopia-" then
    let suffix = String.sub name 7 (String.length name - 7) in
    find_command suffix
  else None

let run () =
  let argv = Array.to_list Sys.argv in
  let argv0 = List.hd argv in
  let executable_command = command_from_executable argv0 in
  match List.tl argv with
  | [ "-h" ] | [ "--help" ] ->
      print_usage ();
      0
  | [ "-v" ] | [ "--version" ] ->
      Printf.printf "%s\n" version;
      0
  | args -> (
      match (executable_command, args) with
      | Some cmd, remaining -> cmd.run remaining
      | None, [] ->
          (* Default: run dev *)
          let dev =
            match find_command "dev" with
            | Some command -> command
            | None -> failwith "dev command missing"
          in
          dev.run []
      | None, command_name :: command_args -> (
          match find_command command_name with
          | Some command -> command.run command_args
          | None ->
              Printf.eprintf "Unknown command: %s\n\n%!" command_name;
              print_usage ();
              1))

let () = exit (run ())
