module type Loader_page = sig
  type data

  val loader : unit -> data
  val path : string
  val make : data -> React.element
end

(* let static_pages : (module Page) list ref = ref [] *)
let loaded_pages : (module Loader_page) Seq.t ref = ref Seq.empty

let register (type a) ~path ~(loader : unit -> a)
    (component : a -> React.element) =
  let module P = struct
    type data = a

    let path = path
    let loader = loader
    let make = component
  end in
  loaded_pages := Seq.cons (module P : Loader_page) !loaded_pages

let page ~path (component : unit -> React.element) =
  let module P = struct
    type data = unit

    let path = path
    let loader () = ()
    let make = component
  end in
  loaded_pages := Seq.cons (module P : Loader_page) !loaded_pages

let get_pages () : (module Loader_page) Seq.t =
  if Seq.is_empty !loaded_pages then failwith "There are no registered Pages";
  !loaded_pages
