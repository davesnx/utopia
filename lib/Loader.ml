module type Page =
  sig
    val path: string
    val make: ?key: string -> unit -> React.element
  end

let p = ref []

let push (module P: Page) =
  p := (module P:Page) :: !p

let get_pages () : (module Page) list =
  match !p with
  | []-> failwith "No Pagein loaded"
  | pages -> pages
