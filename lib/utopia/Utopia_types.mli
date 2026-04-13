(** Core types shared between the compiler and server runtime. *)

(** Whether a route source is compiled OCaml/Reason or markdown. *)
type page_kind = Code_page | Markdown_page

(** Dynamic path parameter kind. *)
type param_kind = Single | Catch_all | Optional_catch_all

(** A parsed route segment: either a static string or a dynamic parameter. *)
type route_segment = Static of string | Param of string * param_kind

type page_route_meta = {
  route : string;
  matcher : string;
  conflict_key : string;
  params : (string * param_kind) list;
  layouts : string list;
  kind : page_kind;
  source_file : string;
  module_name : string;
  has_metadata : bool;
  static : bool;
  has_paths : bool;
}
(** Compiler-facing route metadata for page routes. *)

type api_route_meta = {
  route : string;
  matcher : string;
  conflict_key : string;
  params : (string * param_kind) list;
  middlewares : string list;
  source_file : string;
  module_name : string;
}
(** Compiler-facing route metadata for API routes. *)

val kind_of_extension : string -> page_kind option
val string_of_kind : page_kind -> string
val parse_kind : string -> page_kind option
val string_of_param_kind : param_kind -> string
val parse_param_kind : string -> param_kind option

type og_image = {
  url : string;
  alt : string option;
  width : int option;
  height : int option;
}
(** OpenGraph image metadata. *)

type open_graph = {
  title : string option;
  description : string option;
  url : string option;
  site_name : string option;
  images : og_image list;
  locale : string option;
  og_type : string option;
}

type twitter = {
  card : string option;
  title : string option;
  description : string option;
  site : string option;
  creator : string option;
  images : string list;
}

type robots = {
  index : bool option;
  follow : bool option;
  no_archive : bool option;
}

type icon = {
  href : string;
  rel : string option;
  sizes : string option;
  mime_type : string option;
}

type metadata = {
  title : string option;
  description : string option;
  keywords : string list;
  authors : string list;
  canonical : string option;
  robots : robots option;
  open_graph : open_graph option;
  twitter : twitter option;
  icons : icon list;
  verification : (string * string) list;
}
(** Page metadata for SEO, social sharing, and document head. *)

val make_og_image :
  url:string -> ?alt:string -> ?width:int -> ?height:int -> unit -> og_image

val make_open_graph :
  ?title:string ->
  ?description:string ->
  ?url:string ->
  ?site_name:string ->
  ?images:og_image list ->
  ?locale:string ->
  ?og_type:string ->
  unit ->
  open_graph

val make_twitter :
  ?card:string ->
  ?title:string ->
  ?description:string ->
  ?site:string ->
  ?creator:string ->
  ?images:string list ->
  unit ->
  twitter

val make_robots :
  ?index:bool -> ?follow:bool -> ?no_archive:bool -> unit -> robots

val make_icon :
  href:string ->
  ?rel:string ->
  ?sizes:string ->
  ?mime_type:string ->
  unit ->
  icon

val make_metadata :
  ?title:string ->
  ?description:string ->
  ?keywords:string list ->
  ?authors:string list ->
  ?canonical:string ->
  ?robots:robots ->
  ?open_graph:open_graph ->
  ?twitter:twitter ->
  ?icons:icon list ->
  ?verification:(string * string) list ->
  unit ->
  metadata

val empty_metadata : metadata
(** Metadata with all fields empty/default. *)
