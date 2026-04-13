(** Server-side request context for server components and server functions.

    Provides ambient Lwt-key-based access to the current Dream request so server
    components can read headers/cookies during render and server functions can
    additionally set response cookies. *)

type pending_cookie = {
  name : string;
  value : string;
  expires : float option;
  max_age : float option;
  domain : string option;
  path : string option;
  secure : bool option;
  http_only : bool option;
  same_site : [ `Strict | `Lax | `None ] option;
}
(** A cookie to be set on the response after a server function completes. *)

(** The current execution phase: rendering or handling an action. *)
type phase = Render | Action of pending_cookie list ref

val get_request : unit -> Dream.request
(** Retrieve the current Dream request. Raises if called outside a server
    component or server function. *)

val get_header : string -> string option
(** Read a request header by name. *)

val get_cookie : ?decrypt:bool -> string -> string option
(** Read a cookie value. Set [~decrypt:true] to decrypt encrypted cookies. *)

val set_cookie :
  ?expires:float ->
  ?max_age:float ->
  ?domain:string ->
  ?path:string ->
  ?secure:bool ->
  ?http_only:bool ->
  ?same_site:[ `Strict | `Lax | `None ] ->
  string ->
  string ->
  unit
(** Queue a Set-Cookie header to be sent with the action response. Only valid
    inside a server function; raises during render. *)

val with_render_context : Dream.request -> (unit -> 'a Lwt.t) -> 'a Lwt.t
(** Run [f] in a render context with the given Dream request. *)

val with_action_context :
  Dream.request ->
  (unit -> 'a Lwt.t) ->
  pending_cookie list ref * (unit -> 'a Lwt.t)
(** Prepare an action context. Returns [(pending_cookies_ref, run)]. Call
    [run ()] to execute [f] with the request and action phase set. *)

val serialize_pending_cookies : pending_cookie list -> (string * string) list
(** Serialize accumulated pending cookies into Set-Cookie header pairs. *)
