(** Route matching primitives shared between compiler, server, and benchmarks.
*)

open Utopia_types

(** Matched parameter value: single segment or multiple (catch-all). *)
type param_value = One of string | Many of string list

val parse_matcher_segment : string -> (route_segment, string) result
(** Parse a single route segment string into a typed {!route_segment}. *)

val parse_matcher : string -> (route_segment list, string) result
(** Parse a full matcher string (e.g. ["users/:id/posts"]) into segments. *)

val specificity_of_segment : route_segment -> int
(** Numeric specificity of a segment for route priority ordering. Static >
    Single > Catch_all > Optional_catch_all. *)

val compare_specificity : route_segment list -> route_segment list -> int
(** Compare two segment lists by specificity. Returns negative if [left] is more
    specific, positive if [right] is, zero if equal. *)

val normalize_target : string -> string
(** Strip query string and leading slash from a request target.
    ["/users?page=1"] becomes ["users"]. *)

val target_segments : string -> string list
(** Split a normalized target into path segments. *)

val strip_query_and_hash : string -> string
(** Remove query string and hash fragment from a path. *)

val path_segments : string -> string list
(** Split a full path (with optional query/hash) into segments. *)

val render_matcher_segment : route_segment -> string
(** Render a segment back to its matcher string representation. *)

val route_definition_of_segments : route_segment list -> string
(** Render a segment list as a route definition string (e.g. ["/:id/posts"]). *)

val common_prefix_length : 'a list -> 'a list -> int
(** Count the number of equal leading elements in two lists. *)

val match_segments :
  route_segment list ->
  string list ->
  (string * param_value) list ->
  (string * param_value) list option
(** Match a list of route segments against path segments, accumulating parameter
    bindings. Returns [None] if the route does not match. *)
