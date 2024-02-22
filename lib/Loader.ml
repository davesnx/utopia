module type PLUG =
  sig
    val path: string
    val make: ?key: string -> unit -> React.element
  end

let p = ref []

let push (module P: PLUG) =
  p := (module P:PLUG) :: !p

let get_plugin () : (module PLUG) list =
  match !p with
  | []-> failwith "No plugin loaded"
  | pages -> pages
