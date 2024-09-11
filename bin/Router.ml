open Ppx_deriving_router_runtime.Primitives

type t =
  | Home [@GET "/"]
  | About
  | Hello of { name : string } [@GET "/hello/:name"]
[@@deriving router]
