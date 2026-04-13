(* Dev event channel: SSE streaming for build diagnostics. *)

type dev_severity = Dev_error | Dev_warning | Dev_note

type dev_diagnostic = {
  dev_severity : dev_severity;
  dev_message : string;
  dev_location : string option;
  dev_targets : string list;
}

type dev_build_status = Dev_building | Dev_failed | Dev_healthy

type dev_build_state = {
  build_id : int;
  status : dev_build_status;
  rebuilding : bool;
  errors : dev_diagnostic list;
  warnings : dev_diagnostic list;
}

let initial_dev_build_state =
  {
    build_id = 0;
    status = Dev_healthy;
    rebuilding = false;
    errors = [];
    warnings = [];
  }

let dev_build_state_ref = ref initial_dev_build_state

let dev_event_condition : dev_build_state Lwt_condition.t =
  Lwt_condition.create ()

let dev_publish_token () = Sys.getenv_opt "UTOPIA_DEV_TOKEN"

let json_escape_dev s =
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

let format_dev_severity = function
  | Dev_error -> "error"
  | Dev_warning -> "warning"
  | Dev_note -> "note"

let format_dev_status = function
  | Dev_building -> "building"
  | Dev_failed -> "failed"
  | Dev_healthy -> "healthy"

let format_dev_diagnostic d =
  let loc =
    match d.dev_location with
    | Some l -> Printf.sprintf {|,"location":"%s"|} (json_escape_dev l)
    | None -> ""
  in
  let targets =
    d.dev_targets
    |> List.map (fun t -> Printf.sprintf {|"%s"|} (json_escape_dev t))
    |> String.concat ","
  in
  Printf.sprintf {|{"severity":"%s","message":"%s"%s,"targets":[%s]}|}
    (format_dev_severity d.dev_severity)
    (json_escape_dev d.dev_message)
    loc targets

let format_dev_build_state_json state =
  let errors =
    state.errors |> List.map format_dev_diagnostic |> String.concat ","
  in
  let warnings =
    state.warnings |> List.map format_dev_diagnostic |> String.concat ","
  in
  Printf.sprintf
    {|{"kind":"build_state","build_id":%d,"status":"%s","rebuilding":%s,"errors":[%s],"warnings":[%s]}|}
    state.build_id
    (format_dev_status state.status)
    (if state.rebuilding then "true" else "false")
    errors warnings

let format_sse_event state =
  Printf.sprintf "data: %s\n\n" (format_dev_build_state_json state)

let handle_dev_events_sse _request =
  Dream.stream
    ~headers:
      [
        ("Content-Type", "text/event-stream");
        ("Cache-Control", "no-cache");
        ("X-Accel-Buffering", "no");
      ]
    (fun stream ->
      let open Lwt.Syntax in
      let* () = Dream.write stream (format_sse_event !dev_build_state_ref) in
      let* () = Dream.flush stream in
      let rec event_loop () =
        let* state = Lwt_condition.wait dev_event_condition in
        let* () = Dream.write stream (format_sse_event state) in
        let* () = Dream.flush stream in
        event_loop ()
      in
      let rec heartbeat_loop () =
        let* () = Lwt_unix.sleep 30.0 in
        Lwt.catch
          (fun () ->
            let* () = Dream.write stream ": heartbeat\n\n" in
            let* () = Dream.flush stream in
            heartbeat_loop ())
          (fun _exn -> Lwt.return_unit)
      in
      Lwt.catch
        (fun () -> Lwt.pick [ event_loop (); heartbeat_loop () ])
        (fun _exn -> Lwt.return_unit))

let parse_dev_severity_json = function
  | "error" -> Dev_error
  | "warning" -> Dev_warning
  | _ -> Dev_note

let parse_dev_build_status_json = function
  | "building" -> Dev_building
  | "failed" -> Dev_failed
  | _ -> Dev_healthy

(* Minimal JSON field extractors for the well-known dev event shape *)
let extract_json_string body field =
  let pattern = Printf.sprintf {|"%s":"|} field in
  let plen = String.length pattern in
  let blen = String.length body in
  let rec search i =
    if i + plen > blen then None
    else if String.sub body i plen = pattern then
      let start = i + plen in
      let rec find_end j =
        if j >= blen then j
        else if body.[j] = '"' && (j = start || body.[j - 1] <> '\\') then j
        else find_end (j + 1)
      in
      let e = find_end start in
      Some (String.sub body start (e - start))
    else search (i + 1)
  in
  search 0

let extract_json_int body field =
  let pattern = Printf.sprintf {|"%s":|} field in
  let plen = String.length pattern in
  let blen = String.length body in
  let rec search i =
    if i + plen > blen then None
    else if String.sub body i plen = pattern then
      let start = i + plen in
      let rec digits j =
        if j >= blen then j
        else match body.[j] with '0' .. '9' -> digits (j + 1) | _ -> j
      in
      let e = digits start in
      try Some (int_of_string (String.sub body start (e - start)))
      with Failure _ -> None
    else search (i + 1)
  in
  search 0

let extract_json_bool body field =
  let pattern = Printf.sprintf {|"%s":|} field in
  let plen = String.length pattern in
  let blen = String.length body in
  let rec search i =
    if i + plen > blen then Some false
    else if String.sub body i plen = pattern then
      let start = i + plen in
      if start + 4 <= blen && String.sub body start 4 = "true" then Some true
      else Some false
    else search (i + 1)
  in
  search 0

let extract_diagnostics_array body array_field =
  let pattern = Printf.sprintf {|"%s":[|} array_field in
  let plen = String.length pattern in
  let blen = String.length body in
  let rec search i =
    if i + plen > blen then []
    else if String.sub body i plen = pattern then (
      let start = i + plen in
      let rec find_end j depth =
        if j >= blen then j
        else
          match body.[j] with
          | '[' -> find_end (j + 1) (depth + 1)
          | ']' -> if depth = 0 then j else find_end (j + 1) (depth - 1)
          | _ -> find_end (j + 1) depth
      in
      let e = find_end start 0 in
      let arr_str = String.sub body start (e - start) in
      if String.length (String.trim arr_str) = 0 then []
      else
        (* Split on top-level commas between objects *)
        let buf = Buffer.create 256 in
        let objects = ref [] in
        let depth = ref 0 in
        String.iter
          (fun c ->
            match c with
            | '{' ->
                incr depth;
                Buffer.add_char buf c
            | '}' ->
                decr depth;
                Buffer.add_char buf c;
                if !depth = 0 then (
                  objects := Buffer.contents buf :: !objects;
                  Buffer.clear buf)
            | ',' when !depth = 0 -> ()
            | c -> Buffer.add_char buf c)
          arr_str;
        List.rev_map
          (fun obj ->
            {
              dev_severity =
                parse_dev_severity_json
                  (Option.value
                     (extract_json_string obj "severity")
                     ~default:"note");
              dev_message =
                Option.value (extract_json_string obj "message") ~default:"";
              dev_location = extract_json_string obj "location";
              dev_targets = [];
            })
          !objects)
    else search (i + 1)
  in
  search 0

let parse_dev_build_state_json body =
  let build_id = Option.value (extract_json_int body "build_id") ~default:0 in
  let status =
    parse_dev_build_status_json
      (Option.value (extract_json_string body "status") ~default:"healthy")
  in
  let rebuilding =
    Option.value (extract_json_bool body "rebuilding") ~default:false
  in
  let errors = extract_diagnostics_array body "errors" in
  let warnings = extract_diagnostics_array body "warnings" in
  { build_id; status; rebuilding; errors; warnings }

let handle_dev_events_publish request =
  let open Lwt.Syntax in
  let expected_token = dev_publish_token () in
  let auth_header = Dream.header request "Authorization" in
  let authorized =
    match (expected_token, auth_header) with
    | Some expected, Some provided ->
        String.equal provided ("Bearer " ^ expected)
    | None, _ -> false
    | _, None -> false
  in
  if not authorized then Dream.respond ~status:`Unauthorized ""
  else
    let* body = Dream.body request in
    let state = parse_dev_build_state_json body in
    dev_build_state_ref := state;
    Lwt_condition.broadcast dev_event_condition state;
    Dream.respond ~status:`OK ""
