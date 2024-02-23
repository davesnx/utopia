module type Page =
  sig
    val path: string
    val make: ?key: string -> unit -> React.element
  end

let pages = ref []

let register (module P: Page) =
  pages := (module P:Page) :: !pages

let get_pages () : (module Page) list =
  match !pages with
  | []-> failwith "There are no registered Pages"
  | pages -> pages
