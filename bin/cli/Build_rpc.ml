module Client = Dune_rpc_lwt.V1
module Protocol = Dune_rpc.V1

type lifecycle_hooks = {
  build_started : unit -> unit;
  build_failed : Protocol.Diagnostic.t list -> unit;
  build_succeeded : unit -> unit;
}

let noop_hooks =
  {
    build_started = (fun () -> ());
    build_failed = (fun _diagnostics -> ());
    build_succeeded = (fun () -> ());
  }

let pp_to_string pp_doc =
  let buffer = Buffer.create 256 in
  let formatter = Format.formatter_of_buffer buffer in
  Pp.to_fmt formatter (Pp.map_tags pp_doc ~f:(fun _ -> ()));
  Format.pp_print_flush formatter ();
  Buffer.contents buffer

let format_loc loc =
  let start = Protocol.Loc.start loc in
  Printf.sprintf "%s:%d:%d" start.pos_fname start.pos_lnum
    (start.pos_cnum - start.pos_bol)

let format_diagnostic diagnostic =
  let severity =
    match Protocol.Diagnostic.severity diagnostic with
    | Some Protocol.Diagnostic.Error -> Terminal.red "error"
    | Some Protocol.Diagnostic.Warning -> Terminal.yellow "warning"
    | None -> Terminal.dim "note"
  in
  let location =
    match Protocol.Diagnostic.loc diagnostic with
    | Some loc -> format_loc loc ^ ": "
    | None -> ""
  in
  let message = Protocol.Diagnostic.message diagnostic |> pp_to_string in
  let targets =
    Protocol.Diagnostic.targets diagnostic
    |> List.filter_map (fun target ->
        match target with
        | Protocol.Target.Path path -> Some path
        | Protocol.Target.Alias alias ->
            Some (Printf.sprintf "(alias %s)" alias)
        | Protocol.Target.Library library_name ->
            Some (Printf.sprintf "(library %s)" library_name)
        | Protocol.Target.Executables executables ->
            Some
              (Printf.sprintf "(executables %s)"
                 (String.concat " " executables))
        | Protocol.Target.Preprocess preprocesses ->
            Some
              (Printf.sprintf "(preprocess %s)"
                 (String.concat " " preprocesses))
        | Protocol.Target.Loc loc -> Some (format_loc loc))
  in
  let target_suffix =
    match targets with
    | [] -> ""
    | values ->
        Terminal.dim (Printf.sprintf " [%s]" (String.concat " -> " values))
  in
  Printf.sprintf "    %s%s: %s%s" location severity (String.trim message)
    target_suffix

let format_progress = function
  | Protocol.Progress.Waiting -> Terminal.dim "waiting"
  | Protocol.Progress.In_progress { complete; remaining; failed } ->
      let failed_suffix =
        if failed > 0 then
          Printf.sprintf ", %s failed" (Terminal.red (string_of_int failed))
        else ""
      in
      Printf.sprintf "%d/%d%s" complete (complete + remaining) failed_suffix
  | Protocol.Progress.Failed -> Terminal.red "failed"
  | Protocol.Progress.Interrupted -> Terminal.yellow "interrupted"
  | Protocol.Progress.Success -> Terminal.green "done"

let wait_for_socket ~build_dir ~max_retries ~delay_ms =
  let open Lwt.Syntax in
  let rec loop attempt =
    if attempt >= max_retries then Lwt.return_none
    else
      let* result =
        Lwt.catch
          (fun () ->
            let where = Client.Where.default ~build_dir () in
            let* channel = Client.connect_chan where in
            Lwt.return_some channel)
          (fun _exn ->
            let* () = Lwt_unix.sleep (Float.of_int delay_ms /. 1000.0) in
            loop (attempt + 1))
      in
      Lwt.return result
  in
  loop 0

let active_diagnostics : (int, Protocol.Diagnostic.t) Hashtbl.t =
  Hashtbl.create 64

let diagnostic_id_to_int diagnostic =
  Protocol.Diagnostic.Id.hash (Protocol.Diagnostic.id diagnostic)

let print_active_diagnostics () =
  let diagnostics =
    Hashtbl.fold
      (fun _ diagnostic acc -> diagnostic :: acc)
      active_diagnostics []
  in
  if diagnostics <> [] then (
    Printf.printf "\n";
    diagnostics
    |> List.iter (fun diagnostic ->
        Printf.printf "%s\n%!" (format_diagnostic diagnostic));
    Printf.printf "\n%!")

let active_diagnostics_snapshot () =
  Hashtbl.fold (fun _ diagnostic acc -> diagnostic :: acc) active_diagnostics []

type build_phase = Waiting | Building | Failed | Interrupted | Succeeded

let phase_of_progress = function
  | Protocol.Progress.Waiting -> Waiting
  | Protocol.Progress.In_progress _ -> Building
  | Protocol.Progress.Failed -> Failed
  | Protocol.Progress.Interrupted -> Interrupted
  | Protocol.Progress.Success -> Succeeded

let emit_lifecycle hooks previous_phase next_phase =
  match (previous_phase, next_phase) with
  | Building, Building
  | Failed, Failed
  | Succeeded, Succeeded
  | Interrupted, Interrupted
  | Waiting, Waiting ->
      ()
  | _, Building -> hooks.build_started ()
  | _, Failed -> hooks.build_failed (active_diagnostics_snapshot ())
  | _, Succeeded -> hooks.build_succeeded ()
  | _, (Waiting | Interrupted) -> ()

let run_loop ?(hooks = noop_hooks) ~build_dir ~verbose () =
  let open Lwt.Syntax in
  let init =
    Protocol.Initialize.create ~id:(Protocol.Id.make (Csexp.Atom "utopia-dev"))
  in
  let* channel_opt = wait_for_socket ~build_dir ~max_retries:50 ~delay_ms:200 in
  match channel_opt with
  | None ->
      Terminal.print_warn
        "Could not connect to dune RPC (build watch may not support RPC)";
      let waiter, _wakener = Lwt.wait () in
      waiter
  | Some channel ->
      if verbose then Terminal.print_done "Connected to dune RPC";
      let handler =
        Client.Client.Handler.create
          ~log:(fun message ->
            if verbose then
              Printf.printf "  %s %s\n%!" (Terminal.dim "rpc")
                (Protocol.Message.message message);
            Lwt.return_unit)
          ()
      in
      Client.Client.connect ~handler channel init ~f:(fun client ->
          let* progress_result =
            Client.Client.poll client Protocol.Sub.progress
          in
          let progress_stream =
            match progress_result with
            | Ok stream -> Some stream
            | Error _err ->
                Terminal.print_warn "Could not subscribe to build progress";
                None
          in
          let* diagnostic_result =
            Client.Client.poll client Protocol.Sub.diagnostic
          in
          let diagnostic_stream =
            match diagnostic_result with
            | Ok stream -> Some stream
            | Error _err ->
                Terminal.print_warn "Could not subscribe to diagnostics";
                None
          in
          let progress_loop =
            match progress_stream with
            | None ->
                let waiter, _wakener = Lwt.wait () in
                waiter
            | Some stream ->
                let last_status = ref "" in
                let last_phase = ref Waiting in
                let rec loop () =
                  let* event = Client.Client.Stream.next stream in
                  match event with
                  | None -> Lwt.return_unit
                  | Some progress ->
                      let phase = phase_of_progress progress in
                      emit_lifecycle hooks !last_phase phase;
                      last_phase := phase;
                      let status = format_progress progress in
                      if status <> !last_status then (
                        last_status := status;
                        match progress with
                        | Protocol.Progress.Success ->
                            Printf.printf "  %s Build %s\n%!"
                              (Terminal.green "✓") status
                        | Protocol.Progress.Failed ->
                            Printf.printf "  %s Build %s\n%!" (Terminal.red "✗")
                              status;
                            print_active_diagnostics ()
                        | Protocol.Progress.Interrupted ->
                            Printf.printf "  %s Build %s\n%!"
                              (Terminal.yellow "⚠") status
                        | Protocol.Progress.In_progress _ ->
                            Printf.printf "  %s Build %s\n%!"
                              (Terminal.cyan "▸") status
                        | Protocol.Progress.Waiting ->
                            Printf.printf "  %s Build %s\n%!" (Terminal.dim "◌")
                              status);
                      loop ()
                in
                loop ()
          in
          let diagnostic_loop =
            match diagnostic_stream with
            | None ->
                let waiter, _wakener = Lwt.wait () in
                waiter
            | Some stream ->
                let rec loop () =
                  let* event = Client.Client.Stream.next stream in
                  match event with
                  | None -> Lwt.return_unit
                  | Some events ->
                      events
                      |> List.iter (fun event ->
                          match event with
                          | Protocol.Diagnostic.Event.Add diagnostic ->
                              let key = diagnostic_id_to_int diagnostic in
                              Hashtbl.replace active_diagnostics key diagnostic;
                              if verbose then
                                Printf.printf "%s\n%!"
                                  (format_diagnostic diagnostic)
                          | Protocol.Diagnostic.Event.Remove diagnostic ->
                              let key = diagnostic_id_to_int diagnostic in
                              Hashtbl.remove active_diagnostics key);
                      loop ()
                in
                loop ()
          in
          let disconnected =
            let* () = Client.Client.disconnected client in
            if verbose then Terminal.print_warn "Dune RPC disconnected";
            Lwt.return_unit
          in
          Lwt.pick [ progress_loop; diagnostic_loop; disconnected ])
