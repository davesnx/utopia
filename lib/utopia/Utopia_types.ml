type page_kind = Code_page | Markdown_page
type param_kind = Single | Catch_all | Optional_catch_all
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
  has_static_paths : bool;
}

type api_route_meta = {
  route : string;
  matcher : string;
  conflict_key : string;
  params : (string * param_kind) list;
  middlewares : string list;
  source_file : string;
  module_name : string;
}

let kind_of_extension = function
  | ".ml" | ".mlx" | ".re" -> Some Code_page
  | ".md" -> Some Markdown_page
  | _ -> None

let string_of_kind = function
  | Code_page -> "code"
  | Markdown_page -> "markdown"

let parse_kind = function
  | "code" -> Some Code_page
  | "markdown" -> Some Markdown_page
  | _ -> None

let string_of_param_kind = function
  | Single -> "single"
  | Catch_all -> "catch_all"
  | Optional_catch_all -> "optional_catch_all"

let parse_param_kind = function
  | "single" -> Some Single
  | "catch_all" -> Some Catch_all
  | "optional_catch_all" -> Some Optional_catch_all
  | _ -> None

type og_image = {
  url : string;
  alt : string option;
  width : int option;
  height : int option;
}

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

let make_og_image ~url ?alt ?width ?height () = { url; alt; width; height }

let make_open_graph ?title ?description ?url ?site_name ?(images = []) ?locale
    ?og_type () =
  { title; description; url; site_name; images; locale; og_type }

let make_twitter ?card ?title ?description ?site ?creator ?(images = []) () =
  { card; title; description; site; creator; images }

let make_robots ?index ?follow ?no_archive () = { index; follow; no_archive }
let make_icon ~href ?rel ?sizes ?mime_type () = { href; rel; sizes; mime_type }

let make_metadata ?title ?description ?(keywords = []) ?(authors = [])
    ?canonical ?robots ?open_graph ?twitter ?(icons = []) ?(verification = [])
    () =
  {
    title;
    description;
    keywords;
    authors;
    canonical;
    robots;
    open_graph;
    twitter;
    icons;
    verification;
  }

let empty_metadata = make_metadata ()
