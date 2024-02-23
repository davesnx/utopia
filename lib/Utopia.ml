module type Page = sig
  val path: string
  val make: ?key:string -> unit -> React.element
end

module Router = Router
