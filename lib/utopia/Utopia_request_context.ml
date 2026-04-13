(* Request context for server components and server functions.
   Provides ambient Lwt-key-based access to the current Dream request
   so server components can read headers/cookies during render and
   server functions can additionally set response cookies. *)

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

type phase = Render | Action of pending_cookie list ref

let request_key : Dream.request Lwt.key = Lwt.new_key ()
let phase_key : phase Lwt.key = Lwt.new_key ()

let get_request () =
  match Lwt.get request_key with
  | Some request -> request
  | None ->
      failwith
        "Request_context.get_request: no request context. This function must \
         be called inside a server component or server function."

let get_header name = Dream.header (get_request ()) name

let get_cookie ?(decrypt = false) name =
  Dream.cookie ~decrypt (get_request ()) name

let set_cookie ?expires ?max_age ?domain ?path ?secure ?http_only ?same_site
    name value =
  match Lwt.get phase_key with
  | Some (Action pending) ->
      pending :=
        {
          name;
          value;
          expires;
          max_age;
          domain;
          path;
          secure;
          http_only;
          same_site;
        }
        :: !pending
  | Some Render ->
      failwith
        "Request_context.set_cookie: cookies can only be modified in a server \
         function (action), not during render."
  | None ->
      failwith
        "Request_context.set_cookie: no request context. This function must be \
         called inside a server function."

let with_render_context request f =
  Lwt.with_value request_key (Some request) (fun () ->
      Lwt.with_value phase_key (Some Render) f)

let with_action_context request f =
  let pending = ref [] in
  let run () =
    Lwt.with_value request_key (Some request) (fun () ->
        Lwt.with_value phase_key (Some (Action pending)) f)
  in
  (pending, run)

let serialize_pending_cookies pending =
  pending |> List.rev
  |> List.map (fun (cookie : pending_cookie) ->
      let header_value =
        Dream.to_set_cookie ?expires:cookie.expires ?max_age:cookie.max_age
          ?domain:cookie.domain ?path:cookie.path ?secure:cookie.secure
          ?http_only:cookie.http_only ?same_site:cookie.same_site cookie.name
          cookie.value
      in
      ("Set-Cookie", header_value))
