(* dtest_ep *)
module type PLUG =
  sig
    val path: string
    val make: ?key: string -> unit -> React.element
  end

let p = ref None

let get_plugin () : (module PLUG) =
  match !p with
  | Some s -> s
  | None -> failwith "No plugin loaded"
