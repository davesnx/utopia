(** Dev event channel for build diagnostics via Server-Sent Events. *)

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

val dev_build_state_ref : dev_build_state ref
(** The current build state, updated via {!handle_dev_events_publish}. *)

val json_escape_dev : string -> string
(** Escape a string for safe embedding in JSON values. *)

val format_dev_build_state_json : dev_build_state -> string
(** Format a build state as a JSON string. *)

val handle_dev_events_sse : Dream.request -> Dream.response Lwt.t
(** Dream handler: SSE endpoint that streams build state updates. *)

val parse_dev_build_state_json : string -> dev_build_state
(** Parse a JSON body into a {!dev_build_state}. *)

val handle_dev_events_publish : Dream.request -> Dream.response Lwt.t
(** Dream handler: POST endpoint that receives build state updates from the CLI
    dev process. Requires a Bearer token matching the [UTOPIA_DEV_TOKEN]
    environment variable. *)
