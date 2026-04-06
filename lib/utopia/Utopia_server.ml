open Utopia_types

type param_value = One of string | Many of string list

let matched_api_params_field : (string * param_value) list Dream.field =
  Dream.new_field ~name:"utopia_api_params" ()

type generated_route = {
  route : string;
  matcher : string;
  params : (string * param_kind) list;
  layouts : string list;
  kind : page_kind;
  source_file : string;
  render : (unit -> React.element) option;
  metadata : ((string * string) list -> metadata) option;
  layout_renderers : (React.element -> React.element) list;
  router_shell : string -> React.element;
  router_tree : unit -> React.element;
  router_subtree : string -> React.element option;
  static : bool;
  static_paths : (unit -> (string * string) list list) option;
}

type generated_api_route = {
  route : string;
  matcher : string;
  params : (string * param_kind) list;
  middlewares : (Dream.handler -> Dream.handler) list;
  source_file : string;
  handler : Dream.handler;
}

module type Api_handler = sig
  val handler : Dream.request -> Dream.response Lwt.t
end

module type Api_middleware = sig
  val middleware : Dream.handler -> Dream.handler
end

module Generated_route = struct
  let make ~kind ~route ~matcher ~params ~source_file ~layouts ~render ~metadata
      ~layout_renderers ~router_shell ~router_tree ~router_subtree
      ?(static = false) ?(static_paths = None) () =
    {
      route;
      matcher;
      params;
      layouts;
      kind;
      source_file;
      render;
      metadata;
      layout_renderers;
      router_shell;
      router_tree;
      router_subtree;
      static;
      static_paths;
    }

  let code ~route ~matcher ~params ~source_file ~layouts ~render ~metadata
      ~layout_renderers ~router_shell ~router_tree ~router_subtree
      ?(static = false) ?(static_paths = None) () =
    make ~kind:Code_page ~route ~matcher ~params ~source_file ~layouts
      ~render:(Some render) ~metadata ~layout_renderers ~router_shell
      ~router_tree ~router_subtree ~static ~static_paths ()

  let markdown ~route ~matcher ~params ~source_file ~layouts ~metadata
      ~layout_renderers ~router_shell ~router_tree ~router_subtree
      ?(static = false) () =
    make ~kind:Markdown_page ~route ~matcher ~params ~source_file ~layouts
      ~render:None ~metadata ~layout_renderers ~router_shell ~router_tree
      ~router_subtree ~static ()
end

module Generated_api_route = struct
  let make ~route ~matcher ~params ~middlewares ~source_file ~handler () =
    { route; matcher; params; middlewares; source_file; handler }
end

type route_entry = {
  route : string;
  params : (string * param_kind) list;
  layouts : string list;
  kind : page_kind;
  source_file : string;
  segments : route_segment list;
  render : (unit -> React.element) option;
  metadata : ((string * string) list -> metadata) option;
  layout_renderers : (React.element -> React.element) list;
  router_shell : (string -> React.element) option;
  router_tree : (unit -> React.element) option;
  router_subtree : (string -> React.element option) option;
  static : bool;
  static_paths : (unit -> (string * string) list list) option;
}

type api_route_entry = {
  route : string;
  params : (string * param_kind) list;
  source_file : string;
  segments : route_segment list;
  middlewares : (Dream.handler -> Dream.handler) list;
  handler : Dream.handler;
}

let read_file file =
  In_channel.with_open_bin file (fun channel -> In_channel.input_all channel)

type cache_entry = { mtime : float; element : React.element }

let page_cache : (string, cache_entry) Hashtbl.t = Hashtbl.create 64

let file_mtime file =
  try Some (Unix.stat file).Unix.st_mtime with Unix.Unix_error _ -> None

let parse_matcher_segment segment =
  if String.length segment >= 2 && String.sub segment 0 2 = "**" then
    Ok
      (Param
         (String.sub segment 2 (String.length segment - 2), Optional_catch_all))
  else if String.length segment >= 1 && segment.[0] = '*' then
    Ok (Param (String.sub segment 1 (String.length segment - 1), Catch_all))
  else if String.length segment >= 1 && segment.[0] = ':' then
    Ok (Param (String.sub segment 1 (String.length segment - 1), Single))
  else Ok (Static segment)

let parse_matcher matcher =
  if matcher = "" then Ok []
  else
    matcher |> String.split_on_char '/'
    |> List.fold_left
         (fun acc segment ->
           match (acc, parse_matcher_segment segment) with
           | (Error _ as error), _ -> error
           | Ok _, Error message -> Error message
           | Ok segments, Ok parsed_segment -> Ok (parsed_segment :: segments))
         (Ok [])
    |> Result.map List.rev

let specificity_of_segment = function
  | Static _ -> 4
  | Param (_, Single) -> 3
  | Param (_, Catch_all) -> 2
  | Param (_, Optional_catch_all) -> 1

let compare_route_specificity (left : route_entry) (right : route_entry) =
  let rec compare_scores left_scores right_scores =
    match (left_scores, right_scores) with
    | [], [] -> 0
    | _ :: _, [] -> -1
    | [], _ :: _ -> 1
    | left_score :: left_rest, right_score :: right_rest ->
        if left_score > right_score then -1
        else if left_score < right_score then 1
        else compare_scores left_rest right_rest
  in
  let left_scores = List.map specificity_of_segment left.segments in
  let right_scores = List.map specificity_of_segment right.segments in
  compare_scores left_scores right_scores

let route_entry_of_generated_route (generated_route : generated_route) =
  match parse_matcher generated_route.matcher with
  | Error message ->
      Error
        (Printf.sprintf "Invalid generated route '%s': %s" generated_route.route
           message)
  | Ok segments ->
      Ok
        {
          route = generated_route.route;
          params = generated_route.params;
          layouts = generated_route.layouts;
          kind = generated_route.kind;
          source_file = generated_route.source_file;
          segments;
          render = generated_route.render;
          metadata = generated_route.metadata;
          layout_renderers = generated_route.layout_renderers;
          router_shell = Some generated_route.router_shell;
          router_tree = Some generated_route.router_tree;
          router_subtree = Some generated_route.router_subtree;
          static = generated_route.static;
          static_paths = generated_route.static_paths;
        }

let runtime_routes_of_generated_routes (generated_routes : generated_route list)
    =
  generated_routes
  |> List.fold_left
       (fun acc route ->
         match (acc, route_entry_of_generated_route route) with
         | (Error _ as error), _ -> error
         | Ok _, Error message -> Error message
         | Ok routes, Ok parsed_route -> Ok (parsed_route :: routes))
       (Ok [])
  |> Result.map (fun routes ->
      List.rev routes |> List.sort compare_route_specificity)

let compare_api_route_specificity (left : api_route_entry)
    (right : api_route_entry) =
  let rec compare_scores left_scores right_scores =
    match (left_scores, right_scores) with
    | [], [] -> 0
    | _ :: _, [] -> -1
    | [], _ :: _ -> 1
    | left_score :: left_rest, right_score :: right_rest ->
        if left_score > right_score then -1
        else if left_score < right_score then 1
        else compare_scores left_rest right_rest
  in
  let left_scores = List.map specificity_of_segment left.segments in
  let right_scores = List.map specificity_of_segment right.segments in
  compare_scores left_scores right_scores

let api_route_entry_of_generated_route (generated_route : generated_api_route) =
  match parse_matcher generated_route.matcher with
  | Error message ->
      Error
        (Printf.sprintf "Invalid generated API route '%s': %s"
           generated_route.route message)
  | Ok segments ->
      Ok
        {
          route = generated_route.route;
          params = generated_route.params;
          source_file = generated_route.source_file;
          segments;
          middlewares = generated_route.middlewares;
          handler = generated_route.handler;
        }

let runtime_api_routes_of_generated_routes
    (generated_routes : generated_api_route list) =
  generated_routes
  |> List.fold_left
       (fun acc route ->
         match (acc, api_route_entry_of_generated_route route) with
         | (Error _ as error), _ -> error
         | Ok _, Error message -> Error message
         | Ok routes, Ok parsed_route -> Ok (parsed_route :: routes))
       (Ok [])
  |> Result.map (fun routes ->
      List.rev routes |> List.sort compare_api_route_specificity)

let starts_with text prefix =
  let text_len = String.length text in
  let prefix_len = String.length prefix in
  text_len >= prefix_len && String.sub text 0 prefix_len = prefix

let contains_path_traversal path =
  path |> String.split_on_char '/'
  |> List.exists (fun segment -> segment = "..")

let normalize_asset_path path =
  if String.length path > 0 && path.[0] = '/' then
    String.sub path 1 (String.length path - 1)
  else path

let generated_asset_root () =
  let executable_path =
    if Filename.is_relative Sys.executable_name then
      Filename.concat (Sys.getcwd ()) Sys.executable_name
    else Sys.executable_name
  in
  try
    let root = executable_path |> Unix.realpath |> Filename.dirname in
    if Filename.basename root = "_utopia" then Some root else None
  with Unix.Unix_error _ -> None

let source_asset_root_from_generated_root root =
  let marker = Filename.concat "_build" "default" ^ "/" in
  let marker_len = String.length marker in
  let root_len = String.length root in
  let rec find index =
    if index + marker_len > root_len then None
    else if String.sub root index marker_len = marker then Some index
    else find (index + 1)
  in
  match find 0 with
  | None -> None
  | Some index ->
      let prefix = String.sub root 0 index in
      let suffix =
        String.sub root (index + marker_len) (root_len - index - marker_len)
      in
      let candidate = Filename.concat prefix suffix in
      if Sys.file_exists candidate && Sys.is_directory candidate then
        Some candidate
      else None

let asset_roots () =
  match generated_asset_root () with
  | None -> [ "."; "_utopia"; "_build/default/_utopia" ]
  | Some root ->
      let generated_roots =
        match source_asset_root_from_generated_root root with
        | Some source_root ->
            [
              Filename.dirname source_root;
              Filename.dirname root;
              source_root;
              root;
            ]
        | None -> [ Filename.dirname root; root ]
      in
      generated_roots @ [ "."; "_utopia"; "_build/default/_utopia" ]

let first_existing_asset relative_path =
  let relative_path = normalize_asset_path relative_path in
  asset_roots ()
  |> List.find_map (fun root ->
      let candidate = Filename.concat root relative_path in
      if Sys.file_exists candidate && not (Sys.is_directory candidate) then
        Some candidate
      else None)

let content_type_for_asset path =
  match Filename.extension path with
  | ".js" -> "application/javascript; charset=utf-8"
  | ".css" -> "text/css; charset=utf-8"
  | ".json" -> "application/json; charset=utf-8"
  | ".map" -> "application/json; charset=utf-8"
  | ".wasm" -> "application/wasm"
  | ".svg" -> "image/svg+xml"
  | ".png" -> "image/png"
  | ".ico" -> "image/x-icon"
  | ".woff2" -> "font/woff2"
  | ".woff" -> "font/woff"
  | _ -> "application/octet-stream"

let serve_asset target =
  if contains_path_traversal target then
    Dream.respond ~status:`Bad_Request "Invalid asset path"
  else
    match first_existing_asset target with
    | None -> Dream.respond ~status:`Not_Found "Asset not found"
    | Some file ->
        let body = read_file file in
        Dream.respond
          ~headers:[ ("Content-Type", content_type_for_asset file) ]
          body

let normalize_target target =
  let without_query =
    match String.index_opt target '?' with
    | Some index -> String.sub target 0 index
    | None -> target
  in
  if without_query = "/" then ""
  else if String.length without_query > 0 && without_query.[0] = '/' then
    String.sub without_query 1 (String.length without_query - 1)
  else without_query

let target_segments target =
  if target = "" then []
  else
    target |> String.split_on_char '/'
    |> List.filter (fun segment -> segment <> "")

let strip_query_and_hash path =
  let limit = ref (String.length path) in
  (match String.index_opt path '?' with
  | Some index -> limit := min !limit index
  | None -> ());
  (match String.index_opt path '#' with
  | Some index -> limit := min !limit index
  | None -> ());
  String.sub path 0 !limit

let path_segments path =
  strip_query_and_hash path |> String.split_on_char '/'
  |> List.filter (fun segment -> segment <> "")

let render_matcher_segment = function
  | Static segment -> String.lowercase_ascii segment
  | Param (name, Single) -> ":" ^ name
  | Param (name, Catch_all) -> "*" ^ name
  | Param (name, Optional_catch_all) -> "**" ^ name

let route_definition_of_segments segments =
  match List.map render_matcher_segment segments with
  | [] -> "/"
  | parts -> "/" ^ String.concat "/" parts

let common_prefix_length left right =
  let rec loop count left right =
    match (left, right) with
    | left_head :: left_tail, right_head :: right_tail
      when left_head = right_head ->
        loop (count + 1) left_tail right_tail
    | _ -> count
  in
  loop 0 left right

let element ?(props = []) tag children = React.createElement tag props children
let text value = React.string value

let string_prop ?jsx_name name value =
  let jsx_name = match jsx_name with Some value -> value | None -> name in
  React.JSX.string name jsx_name value

let dangerously_inner_html html =
  React.JSX.dangerouslyInnerHtml
    (object
       method __html = html
    end)

let bootstrap_module_paths = [ "/dist/client_entry_melange.js" ]
let stylesheet_paths = [ "/output.css" ]

let available_bootstrap_module_paths () =
  bootstrap_module_paths
  |> List.filter (fun path -> first_existing_asset path <> None)

let available_stylesheet_paths () =
  stylesheet_paths
  |> List.filter (fun path -> first_existing_asset path <> None)

let available_direct_asset_paths () =
  available_stylesheet_paths () |> List.map normalize_asset_path

let rec html_renderable_element (element : React.element) : React.element =
  match element with
  | React.Client_component { client; _ } -> html_renderable_element client
  | React.Fragment children -> React.Fragment (html_renderable_element children)
  | React.List children ->
      React.List (List.map html_renderable_element children)
  | React.Array children ->
      React.Array (Array.map html_renderable_element children)
  | React.Provider ({ children; _ } as provider) ->
      React.Provider
        { provider with children = html_renderable_element children }
  | React.Consumer children -> React.Consumer (html_renderable_element children)
  | React.Upper_case_component (name, render) ->
      React.Upper_case_component
        (name, fun () -> html_renderable_element (render ()))
  | React.Async_component (name, render) ->
      React.Async_component
        (name, fun () -> Lwt.map html_renderable_element (render ()))
  | React.Suspense { key; children; fallback } ->
      React.Suspense
        {
          key;
          children = html_renderable_element children;
          fallback = html_renderable_element fallback;
        }
  | React.Lower_case_element { key; tag; attributes; children } ->
      React.Lower_case_element
        {
          key;
          tag;
          attributes;
          children = List.map html_renderable_element children;
        }
  | (React.Empty | React.Text _ | React.Static _) as value -> value

let should_normalize_client_props import_module =
  match Filename.basename import_module with
  | basename
    when starts_with basename "Utopia_router.re"
         || starts_with basename "Utopia_router_route.re" ->
      true
  | _ -> false

let rec normalize_model_value (model : React.model_value) : React.model_value =
  match model with
  | React.Model.Function _ | React.Model.Json _ | React.Model.Error _
  | React.Model.Promise _ ->
      model
  | React.Model.List values ->
      React.Model.List (List.map normalize_model_value values)
  | React.Model.Assoc entries ->
      React.Model.Assoc
        (List.map
           (fun (name, value) -> (name, normalize_model_value value))
           entries)
  | React.Model.Element element ->
      React.Model.Element (normalize_model_element element)

and normalize_model_element (element : React.element) : React.element =
  match element with
  | React.Empty | React.Text _ | React.Static _ -> element
  | React.Fragment children -> React.Fragment (normalize_model_element children)
  | React.List children ->
      React.List (List.map normalize_model_element children)
  | React.Array children ->
      React.Array (Array.map normalize_model_element children)
  | React.Provider ({ children; _ } as provider) ->
      React.Provider
        { provider with children = normalize_model_element children }
  | React.Consumer children -> React.Consumer (normalize_model_element children)
  | React.Upper_case_component (name, render) ->
      React.Upper_case_component
        (name, fun () -> normalize_model_element (render ()))
  | React.Async_component (name, render) ->
      React.Async_component
        (name, fun () -> Lwt.map normalize_model_element (render ()))
  (* render_html only serializes client props; rewriting the fallback HTML tree here
     injects wrappers into SSR markup and breaks hydration. *)
  | React.Client_component ({ import_module; _ } as component) ->
      React.Client_component
        {
          component with
          props =
            (if should_normalize_client_props import_module then
               List.map
                 (fun (name, value) -> (name, normalize_model_value value))
                 component.props
             else component.props);
        }
  | React.Suspense { key; children; fallback } ->
      React.Suspense
        {
          key;
          children = normalize_model_element children;
          fallback = normalize_model_element fallback;
        }
  | React.Lower_case_element { key; tag; attributes; children } ->
      React.Lower_case_element
        {
          key;
          tag;
          attributes;
          children = List.map normalize_model_element children;
        }

and wrap_raw_inner_html_element (node : React.element) : React.element =
  match node with
  | React.Empty | React.Text _ | React.Static _ -> node
  | React.Fragment children ->
      React.Fragment (wrap_raw_inner_html_element children)
  | React.List children ->
      React.List (List.map wrap_raw_inner_html_element children)
  | React.Array children ->
      React.Array (Array.map wrap_raw_inner_html_element children)
  | React.Provider ({ children; _ } as provider) ->
      React.Provider
        { provider with children = wrap_raw_inner_html_element children }
  | React.Consumer children ->
      React.Consumer (wrap_raw_inner_html_element children)
  | React.Upper_case_component (name, render) ->
      React.Upper_case_component
        (name, fun () -> wrap_raw_inner_html_element (render ()))
  | React.Async_component (name, render) ->
      React.Async_component
        (name, fun () -> Lwt.map wrap_raw_inner_html_element (render ()))
  (* Keep the public wrapper for generated projects, but current SRR versions
     already model dangerouslySetInnerHTML as a prop instead of an element. *)
  | React.Client_component _ as component -> component
  | React.Suspense { key; children; fallback } ->
      React.Suspense
        {
          key;
          children = wrap_raw_inner_html_element children;
          fallback = wrap_raw_inner_html_element fallback;
        }
  | React.Lower_case_element { key; tag; attributes; children } ->
      React.Lower_case_element
        {
          key;
          tag;
          attributes;
          children = List.map wrap_raw_inner_html_element children;
        }

let meta_tag name content =
  element
    ~props:[ string_prop "name" name; string_prop "content" content ]
    "meta" []

let property_tag property content =
  element
    ~props:[ string_prop "property" property; string_prop "content" content ]
    "meta" []

let link_tag ~rel ~href props =
  element
    ~props:([ string_prop "rel" rel; string_prop "href" href ] @ props)
    "link" []

let render_description_meta = function
  | Some desc -> [ meta_tag "description" desc ]
  | None -> []

let render_keywords_meta = function
  | [] -> []
  | keywords -> [ meta_tag "keywords" (String.concat ", " keywords) ]

let render_authors_meta authors =
  authors |> List.map (fun author -> meta_tag "author" author)

let render_canonical_link = function
  | Some url -> [ link_tag ~rel:"canonical" ~href:url [] ]
  | None -> []

let render_robots_meta = function
  | None -> []
  | Some (robots : robots) ->
      let directives =
        (match robots.index with
          | Some true -> [ "index" ]
          | Some false -> [ "noindex" ]
          | None -> [])
        @ (match robots.follow with
          | Some true -> [ "follow" ]
          | Some false -> [ "nofollow" ]
          | None -> [])
        @ match robots.no_archive with Some true -> [ "noarchive" ] | _ -> []
      in
      if directives = [] then []
      else [ meta_tag "robots" (String.concat ", " directives) ]

let render_og_meta = function
  | None -> []
  | Some (og : open_graph) ->
      let opt prop = function
        | Some value -> [ property_tag prop value ]
        | None -> []
      in
      opt "og:title" og.title
      @ opt "og:description" og.description
      @ opt "og:url" og.url
      @ opt "og:site_name" og.site_name
      @ opt "og:locale" og.locale @ opt "og:type" og.og_type
      @ List.concat_map
          (fun (img : og_image) ->
            [ property_tag "og:image" img.url ]
            @ (match img.alt with
              | Some alt -> [ property_tag "og:image:alt" alt ]
              | None -> [])
            @ (match img.width with
              | Some w -> [ property_tag "og:image:width" (string_of_int w) ]
              | None -> [])
            @
            match img.height with
            | Some h -> [ property_tag "og:image:height" (string_of_int h) ]
            | None -> [])
          og.images

let render_twitter_meta = function
  | None -> []
  | Some (tw : twitter) ->
      let opt name = function
        | Some value -> [ meta_tag name value ]
        | None -> []
      in
      opt "twitter:card" tw.card
      @ opt "twitter:title" tw.title
      @ opt "twitter:description" tw.description
      @ opt "twitter:site" tw.site
      @ opt "twitter:creator" tw.creator
      @ List.map (fun url -> meta_tag "twitter:image" url) tw.images

let render_icons_links icons =
  icons
  |> List.map (fun (ic : icon) ->
      let rel = ic.rel |> Option.value ~default:"icon" in
      let extra =
        (match ic.sizes with Some s -> [ string_prop "sizes" s ] | None -> [])
        @
        match ic.mime_type with
        | Some t -> [ string_prop "type" t ]
        | None -> []
      in
      link_tag ~rel ~href:ic.href extra)

let render_verification_meta verification =
  verification
  |> List.map (fun (provider, content) ->
      let name =
        match provider with
        | "google" -> "google-site-verification"
        | "yandex" -> "yandex-verification"
        | "yahoo" -> "y_key"
        | custom -> custom
      in
      meta_tag name content)

let html_page ~title ~meta ~body =
  let stylesheet_links =
    available_stylesheet_paths ()
    |> List.map (fun path ->
        element
          ~props:[ string_prop "rel" "stylesheet"; string_prop "href" path ]
          "link" [])
  in
  let head =
    element "head"
      ([
         element
           ~props:[ string_prop ~jsx_name:"charSet" "charset" "utf-8" ]
           "meta" [];
         element
           ~props:
             [
               string_prop "name" "viewport";
               string_prop "content" "width=device-width, initial-scale=1";
             ]
           "meta" [];
         element "title" [ text title ];
       ]
      @ render_description_meta meta.description
      @ render_keywords_meta meta.keywords
      @ render_authors_meta meta.authors
      @ render_canonical_link meta.canonical
      @ render_robots_meta meta.robots
      @ render_og_meta meta.open_graph
      @ render_twitter_meta meta.twitter
      @ render_icons_links meta.icons
      @ render_verification_meta meta.verification
      @ stylesheet_links)
  in
  let body =
    element "body" [ element ~props:[ string_prop "id" "root" ] "div" [ body ] ]
  in
  element "html" [ head; body ]

let render_param_value = function
  | One value -> value
  | Many values -> String.concat "/" values

let render_params params =
  if params = [] then []
  else
    [
      element "section"
        [
          element "h2" [ text "Params" ];
          element "ul"
            (params
            |> List.map (fun (name, value) ->
                element "li"
                  [
                    element "code" [ text name ];
                    text (" = " ^ render_param_value value);
                  ]));
        ];
    ]

let wrap_with_layouts layouts content =
  List.fold_right
    (fun layout acc ->
      element "section"
        [
          element "div" [ text "Layout: "; element "code" [ text layout ] ]; acc;
        ])
    layouts content

let render_code_page_fresh route source_file params layouts =
  let source = read_file source_file in
  let content =
    element "main"
      ([
         element "h1" [ text ("/" ^ route) ];
         element "p"
           [
             text "Code page from ";
             element "code" [ text source_file ];
             text ".";
           ];
       ]
      @ render_params params
      @ [ element "pre" [ text source ] ])
  in
  html_page ~title:route ~meta:empty_metadata
    ~body:(wrap_with_layouts layouts content)

let render_markdown_html markdown =
  Utopia_markdown.render_string_to_html markdown

let render_markdown_body source_file =
  let markdown = read_file source_file in
  let body_html = render_markdown_html markdown in
  element ~props:[ dangerously_inner_html body_html ] "div" []

let render_markdown_page_fresh source_file params layouts =
  let content =
    element "main" (render_params params @ [ render_markdown_body source_file ])
  in
  html_page ~title:source_file ~meta:empty_metadata
    ~body:(wrap_with_layouts layouts content)

let make_cache_key source_file route params =
  let param_str =
    params
    |> List.map (fun (name, value) ->
        Printf.sprintf "%s=%s" name (render_param_value value))
    |> String.concat "&"
  in
  Printf.sprintf "%s|%s|%s" source_file route param_str

let render_cached ~key ~source_file render_fn =
  let current_mtime = file_mtime source_file in
  match (Hashtbl.find_opt page_cache key, current_mtime) with
  | Some cached, Some mtime when Float.equal cached.mtime mtime ->
      cached.element
  | _, Some mtime ->
      let element = render_fn () in
      Hashtbl.replace page_cache key { mtime; element };
      element
  | _, None -> render_fn ()

let render_code_page route source_file params layouts =
  let key = make_cache_key source_file route params in
  render_cached ~key ~source_file (fun () ->
      render_code_page_fresh route source_file params layouts)

let render_markdown_page source_file params layouts =
  let key = make_cache_key source_file "" params in
  render_cached ~key ~source_file (fun () ->
      render_markdown_page_fresh source_file params layouts)

let apply_layout_renderers layouts layout_renderers body =
  if layout_renderers = [] then wrap_with_layouts layouts body
  else List.fold_right (fun render acc -> render acc) layout_renderers body

let compiled_page_body params render =
  match render_params params with
  | [] -> render ()
  | extra -> element "div" (extra @ [ render () ])

let fallback_title route_entry =
  match route_entry.kind with
  | Code_page -> if route_entry.route = "" then "/" else route_entry.route
  | Markdown_page -> route_entry.source_file

let flatten_params params =
  List.map
    (fun (name, value) ->
      (name, match value with One s -> s | Many ss -> String.concat "/" ss))
    params

let resolve_metadata route_entry params =
  match route_entry.metadata with
  | Some gen -> gen (flatten_params params)
  | None -> empty_metadata

let resolve_title_from_meta meta route_entry =
  match meta.title with Some t -> t | None -> fallback_title route_entry

let render_route_element route_entry params =
  let meta = resolve_metadata route_entry params in
  let title = resolve_title_from_meta meta route_entry in
  match (route_entry.kind, route_entry.render) with
  | Code_page, Some render ->
      let body = compiled_page_body params render in
      let body =
        apply_layout_renderers route_entry.layouts route_entry.layout_renderers
          body
      in
      html_page ~title ~meta ~body
  | Code_page, None ->
      render_code_page route_entry.route route_entry.source_file params
        route_entry.layouts
  | Markdown_page, _ ->
      if route_entry.layout_renderers = [] then
        render_markdown_page route_entry.source_file params route_entry.layouts
      else
        let body =
          let key = make_cache_key route_entry.source_file "" params in
          render_cached ~key ~source_file:route_entry.source_file (fun () ->
              element "main"
                (render_params params
                @ [ render_markdown_body route_entry.source_file ]))
        in
        let body =
          apply_layout_renderers route_entry.layouts
            route_entry.layout_renderers body
        in
        html_page ~title ~meta ~body

let render_route_document route_entry request_target params =
  match route_entry.router_shell with
  | Some render_shell ->
      let meta = resolve_metadata route_entry params in
      let title = resolve_title_from_meta meta route_entry in
      html_page ~title ~meta ~body:(render_shell request_target)
  | None -> render_route_element route_entry params

let take_segments count segments =
  let rec loop remaining acc rest =
    if remaining <= 0 then List.rev acc
    else
      match rest with
      | [] -> List.rev acc
      | segment :: tail -> loop (remaining - 1) (segment :: acc) tail
  in
  loop count [] segments

let diff_parent_route (route_entry : route_entry) current_path request_target =
  let target_segments = path_segments request_target in
  let current_segments = path_segments current_path in
  let shared_segments =
    common_prefix_length current_segments target_segments
    |> min (List.length route_entry.segments)
  in
  route_definition_of_segments
    (take_segments shared_segments route_entry.segments)

let route_navigation_model (route_entry : route_entry) request rendered_element
    =
  match (route_entry.router_tree, route_entry.router_subtree) with
  | Some render_tree, Some render_subtree -> (
      let full_tree () = render_tree () |> normalize_model_element in
      match Dream.header request "X-Utopia-Current-Path" with
      | Some current_path -> (
          let request_target = Dream.target request |> strip_query_and_hash in
          let current_path = strip_query_and_hash current_path in
          if current_path = request_target then
            React.Model.List
              [
                React.Model.Json (`String "full");
                React.Model.Json (`String "");
                React.Model.Element (full_tree ());
              ]
          else
            let parent_route =
              diff_parent_route route_entry current_path request_target
            in
            match render_subtree parent_route with
            | Some subtree ->
                React.Model.List
                  [
                    React.Model.Json (`String "diff");
                    React.Model.Json (`String parent_route);
                    React.Model.Element (normalize_model_element subtree);
                  ]
            | None ->
                React.Model.List
                  [
                    React.Model.Json (`String "full");
                    React.Model.Json (`String "");
                    React.Model.Element (full_tree ());
                  ])
      | None ->
          React.Model.List
            [
              React.Model.Json (`String "full");
              React.Model.Json (`String "");
              React.Model.Element (full_tree ());
            ])
  | _ -> React.Model.Element (normalize_model_element rendered_element)

let rec match_segments route_segments path_segments params =
  match (route_segments, path_segments) with
  | [], [] -> Some (List.rev params)
  | Static expected :: rest_route, current :: rest_path
    when expected = String.lowercase_ascii current ->
      match_segments rest_route rest_path params
  | Param (name, Single) :: rest_route, current :: rest_path ->
      match_segments rest_route rest_path ((name, One current) :: params)
  | [ Param (name, Catch_all) ], rest_path ->
      if rest_path = [] then None
      else Some (List.rev ((name, Many rest_path) :: params))
  | [ Param (name, Optional_catch_all) ], rest_path ->
      Some (List.rev ((name, Many rest_path) :: params))
  | _ -> None

let find_match (routes : route_entry list) path_segments =
  routes
  |> List.find_map (fun (route : route_entry) ->
      match match_segments route.segments path_segments [] with
      | None -> None
      | Some params -> Some (route, params))

let render_index (routes : route_entry list) =
  let links =
    routes
    |> List.map (fun { route; kind; source_file; params; layouts; _ } ->
        let label =
          match kind with Code_page -> "code" | Markdown_page -> "markdown"
        in
        let param_text =
          params |> List.map fst |> function
          | [] -> ""
          | names -> " params: " ^ String.concat ", " names
        in
        let layout_text =
          match layouts with
          | [] -> ""
          | values ->
              " layouts: "
              ^ String.concat ", " (List.map Filename.basename values)
        in
        element "li"
          [
            element
              ~props:[ string_prop "href" ("/" ^ route) ]
              "a"
              [ text ("/" ^ route) ];
            text (Printf.sprintf " - %s%s%s (" label param_text layout_text);
            element "code" [ text source_file ];
            text ")";
          ])
  in
  html_page ~title:"Utopia dev router" ~meta:empty_metadata
    ~body:
      (element "main" [ element "h1" [ text "Routes" ]; element "ul" links ])

let stream_html element =
  let element = normalize_model_element element in
  Dream.stream
    ~headers:[ ("Content-Type", "text/html; charset=utf-8") ]
    (fun response_stream ->
      let open Lwt.Syntax in
      let* html, subscribe =
        ReactServerDOM.render_html
          ~bootstrapModules:(available_bootstrap_module_paths ())
          element
      in
      let* () = Dream.write response_stream html in
      let* () = Dream.flush response_stream in
      subscribe (fun chunk ->
          let* () = Dream.write response_stream chunk in
          Dream.flush response_stream))

let stream_model ~location model =
  Dream.stream
    ~headers:
      [
        ("Content-Type", "application/react.component");
        ("X-Content-Type-Options", "nosniff");
        ("X-Location", location);
      ]
    (fun response_stream ->
      ReactServerDOM.render_model_value
        ~subscribe:(fun chunk ->
          let open Lwt.Syntax in
          let* () = Dream.write response_stream chunk in
          Dream.flush response_stream)
        model)

let action_id_header = "X-Action-ID"
let legacy_action_id_header = "ACTION_ID"

let text_response ~status message =
  Dream.respond ~status
    ~headers:[ ("Content-Type", "text/plain; charset=utf-8") ]
    message

let has_content_type_header headers =
  headers
  |> List.exists (fun (name, _value) ->
      String.lowercase_ascii name = "content-type")

let respond ?(status = `OK) ?(headers = []) json =
  let headers =
    if has_content_type_header headers then headers
    else ("Content-Type", "application/json; charset=utf-8") :: headers
  in
  Dream.respond ~status ~headers json

let matched_api_params request =
  Dream.field request matched_api_params_field |> Option.value ~default:[]

let find_matched_api_param request name =
  matched_api_params request
  |> List.find_map (fun (candidate, value) ->
      if String.equal candidate name then Some value else None)

let api_param_single_exn request name =
  match find_matched_api_param request name with
  | Some (One value) -> value
  | Some (Many _) ->
      failwith
        (Printf.sprintf "API param '%s' was matched with the wrong shape" name)
  | None -> failwith (Printf.sprintf "Missing required API param '%s'" name)

let api_param_many_exn request name =
  match find_matched_api_param request name with
  | Some (Many values) ->
      if values <> [] then values
      else
        failwith
          (Printf.sprintf
             "API param '%s' must contain at least one segment, but was empty"
             name)
  | Some (One _) ->
      failwith
        (Printf.sprintf "API param '%s' was matched with the wrong shape" name)
  | None -> failwith (Printf.sprintf "Missing required API param '%s'" name)

let api_param_optional_many request name =
  match find_matched_api_param request name with
  | Some (Many values) -> values
  | Some (One _) ->
      failwith
        (Printf.sprintf "API param '%s' was matched with the wrong shape" name)
  | None -> []

let json_escape value =
  let buffer = Buffer.create (String.length value + 16) in
  String.iter
    (fun char ->
      match char with
      | '"' -> Buffer.add_string buffer "\\\""
      | '\\' -> Buffer.add_string buffer "\\\\"
      | '\b' -> Buffer.add_string buffer "\\b"
      | '\012' -> Buffer.add_string buffer "\\f"
      | '\n' -> Buffer.add_string buffer "\\n"
      | '\r' -> Buffer.add_string buffer "\\r"
      | '\t' -> Buffer.add_string buffer "\\t"
      | c when Char.code c < 0x20 ->
          Buffer.add_string buffer (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char buffer c)
    value;
  Buffer.contents buffer

let api_error_response ~status ~error ~code request =
  let path = Dream.target request |> json_escape in
  let error = json_escape error in
  let code = json_escape code in
  respond ~status
    (Printf.sprintf "{\"error\":\"%s\",\"code\":\"%s\",\"path\":\"%s\"}" error
       code path)

let request_action_id request =
  match Dream.header request action_id_header with
  | Some _ as action_id -> action_id
  | None -> Dream.header request legacy_action_id_header

let content_type_is request prefix =
  match Dream.header request "Content-Type" with
  | Some value -> starts_with value prefix
  | None -> false

let multipart_form_to_form_data multipart_form =
  let form_data = Js.FormData.make () in
  multipart_form
  |> List.iter (fun (name, values) ->
      values
      |> List.iter (fun (_filename, value) ->
          Js.FormData.append form_data name (`String value)));
  form_data

let invalid_action_request message = text_response ~status:`Bad_Request message

let decode_action_body body =
  try Ok (ReactServerDOM.decodeReply body) with
  | Invalid_argument message | Failure message -> Error message
  | exn -> Error (Printexc.to_string exn)

let decode_action_form multipart_form =
  try
    Ok
      (ReactServerDOM.decodeFormDataReply
         (multipart_form_to_form_data multipart_form))
  with
  | Invalid_argument message | Failure message -> Error message
  | exn -> Error (Printexc.to_string exn)

let stream_action_response response =
  Dream.stream
    ~headers:[ ("Content-Type", "application/react.action") ]
    (fun response_stream ->
      ReactServerDOM.create_action_response
        ~subscribe:(fun chunk ->
          let open Lwt.Syntax in
          let* () = Dream.write response_stream chunk in
          Dream.flush response_stream)
        response)

let handle_body_server_function callback request =
  let open Lwt.Syntax in
  let* body = Dream.body request in
  match decode_action_body body with
  | Ok args -> stream_action_response (callback args)
  | Error message -> invalid_action_request message

let handle_form_data_server_function callback request =
  let open Lwt.Syntax in
  let* multipart_result = Dream.multipart ~csrf:false request in
  match multipart_result with
  | `Ok multipart_form -> (
      match decode_action_form multipart_form with
      | Ok (args, form_data) -> stream_action_response (callback args form_data)
      | Error message -> invalid_action_request message)
  | _ -> invalid_action_request "Invalid multipart action request"

let handle_server_function_request ~lookup_server_function request =
  match request_action_id request with
  | None ->
      text_response ~status:`Bad_Request
        (Printf.sprintf "Missing %s header" action_id_header)
  | Some action_id -> (
      match lookup_server_function action_id with
      | None ->
          text_response ~status:`Not_Found
            (Printf.sprintf "Unknown server function: %s" action_id)
      | Some (ReactServerDOM.Body _)
        when content_type_is request "multipart/form-data" ->
          invalid_action_request
            "This server function expects an encoded request body, not \
             multipart form-data"
      | Some (ReactServerDOM.Body callback) ->
          handle_body_server_function callback request
      | Some (ReactServerDOM.FormData callback)
        when content_type_is request "multipart/form-data" ->
          handle_form_data_server_function callback request
      | Some (ReactServerDOM.FormData _) ->
          invalid_action_request
            "This server function expects a multipart/form-data request")

let is_api_target target =
  String.equal target "api" || starts_with target "api/"

let find_api_match (routes : api_route_entry list) path_segments =
  routes
  |> List.find_map (fun (route : api_route_entry) ->
      match match_segments route.segments path_segments [] with
      | None -> None
      | Some params -> Some (route, params))

let apply_api_middlewares middlewares handler =
  List.fold_right (fun middleware acc -> middleware acc) middlewares handler

let route_api_request (api_routes : api_route_entry list) request =
  let target = Dream.target request |> normalize_target in
  let segments = target_segments target in
  match find_api_match api_routes segments with
  | None ->
      api_error_response ~status:`Not_Found ~error:"API route not found"
        ~code:"api_not_found" request
  | Some (route, params) ->
      Dream.set_field request matched_api_params_field params;
      let handler = apply_api_middlewares route.middlewares route.handler in
      Lwt.catch
        (fun () -> handler request)
        (fun _exn ->
          api_error_response ~status:`Internal_Server_Error
            ~error:"Internal API error" ~code:"api_internal_error" request)

let accepts_react_component request =
  match Dream.header request "Accept" with
  | Some value ->
      let needle = "application/react.component" in
      let needle_len = String.length needle in
      let value_len = String.length value in
      let rec loop index =
        if index + needle_len > value_len then false
        else if String.sub value index needle_len = needle then true
        else loop (index + 1)
      in
      loop 0
  | None -> false

let route_request (routes : route_entry list)
    (api_routes : api_route_entry list) index_html ~lookup_server_function
    request =
  let target = Dream.target request |> normalize_target in
  if
    starts_with target "target/"
    || starts_with target "dist/"
    || List.mem target (available_direct_asset_paths ())
  then serve_asset target
  else if is_api_target target then route_api_request api_routes request
  else
    let segments = target_segments target in
    if Dream.method_ request = `POST then
      handle_server_function_request ~lookup_server_function request
    else
      match find_match routes segments with
      | Some (route_entry, params) ->
          if accepts_react_component request then
            stream_model ~location:(Dream.target request)
              (match (route_entry.router_tree, route_entry.router_subtree) with
              | Some _, Some _ ->
                  route_navigation_model route_entry request React.null
              | _ ->
                  let rendered_element =
                    render_route_document route_entry (Dream.target request)
                      params
                  in
                  route_navigation_model route_entry request rendered_element)
          else
            stream_html
              (render_route_document route_entry (Dream.target request) params)
      | None when segments = [] ->
          if accepts_react_component request then
            stream_model ~location:(Dream.target request)
              (React.Model.Element (normalize_model_element index_html))
          else stream_html index_html
      | None -> Dream.respond ~status:`Not_Found "Route not found"

let max_port = 65535
let valid_port port = port >= 1 && port <= max_port

let port_from_env () =
  match Sys.getenv_opt "PORT" with
  | None -> 8080
  | Some value -> (
      match int_of_string_opt value with
      | Some port when valid_port port -> port
      | _ ->
          Printf.eprintf "Invalid PORT value '%s', defaulting to 8080\n%!" value;
          8080)

let host_from_env () =
  match Sys.getenv_opt "HOST" with None -> "127.0.0.1" | Some value -> value

let rec run_with_port_fallback ~interface ~port pipeline =
  try Dream.run ~interface ~port pipeline
  with Unix.Unix_error (Unix.EADDRINUSE, _, _) ->
    if port >= max_port then (
      Printf.eprintf
        "Port %d is already in use on %s and no higher ports are available\n%!"
        port interface;
      exit 1);
    let next_port = port + 1 in
    Printf.eprintf "Port %d is already in use on %s; retrying with %d\n%!" port
      interface next_port;
    run_with_port_fallback ~interface ~port:next_port pipeline

let start_runtime_routes (routes : route_entry list)
    (api_routes : api_route_entry list) ~lookup_server_function =
  Printexc.record_backtrace true;
  Logs.set_level (Some Info);
  Logs.set_reporter (Logs_fmt.reporter ());
  let index_html = render_index routes in
  let enable_logging = Sys.getenv_opt "NO_LOG" = None in
  let handler =
    route_request routes api_routes index_html ~lookup_server_function
  in
  let pipeline = if enable_logging then Dream.logger @@ handler else handler in
  run_with_port_fallback ~interface:(host_from_env ()) ~port:(port_from_env ())
    pipeline

let ensure_directory path =
  let parts = String.split_on_char '/' path in
  let rec build_path current = function
    | [] -> ()
    | segment :: rest ->
        let next = if current = "" then segment else current ^ "/" ^ segment in
        if next <> "" && not (Sys.file_exists next) then Sys.mkdir next 0o755;
        build_path next rest
  in
  build_path "" parts

let write_file path content =
  let channel = open_out path in
  output_string channel content;
  close_out channel

let ssg_output_dir = "_utopia/static"

let ssg_output_path route =
  if route = "" then ssg_output_dir ^ "/index.html"
  else ssg_output_dir ^ "/" ^ route ^ "/index.html"

let render_ssg_page route_entry request_target params =
  let flat_params =
    params |> List.map (fun (name, value) -> (name, One value))
  in
  let element =
    render_route_document route_entry request_target flat_params
    |> normalize_model_element
  in
  let open Lwt.Syntax in
  Lwt_main.run
    (let* html, subscribe =
       ReactServerDOM.render_html
         ~bootstrapModules:(available_bootstrap_module_paths ())
         element
     in
     let buffer = Buffer.create (String.length html + 1024) in
     Buffer.add_string buffer html;
     let* () =
       subscribe (fun chunk ->
           Buffer.add_string buffer chunk;
           Lwt.return_unit)
     in
     Lwt.return (Buffer.contents buffer))

let copy_ssg_asset relative_path =
  let relative = normalize_asset_path relative_path in
  match first_existing_asset relative_path with
  | Some source ->
      let dest = ssg_output_dir ^ "/" ^ relative in
      ensure_directory (Filename.dirname dest);
      write_file dest (read_file source);
      Printf.printf "  copied %s\n%!" relative
  | None -> ()

let ssg_asset_paths () =
  let rec dedupe seen = function
    | [] -> List.rev seen
    | path :: rest when List.mem path seen -> dedupe seen rest
    | path :: rest -> dedupe (path :: seen) rest
  in
  dedupe [] (available_stylesheet_paths () @ available_bootstrap_module_paths ())

let ssg_generated (generated_routes : generated_route list) =
  match runtime_routes_of_generated_routes generated_routes with
  | Error message ->
      Printf.eprintf "SSG Error: %s\n%!" message;
      exit 1
  | Ok routes ->
      let static_routes =
        routes |> List.filter (fun (r : route_entry) -> r.static)
      in
      if static_routes = [] then (
        Printf.printf "SSG: no static pages found\n%!";
        exit 0);
      Printf.printf "SSG: rendering %d static page(s)\n%!"
        (List.length static_routes);
      ensure_directory ssg_output_dir;
      ssg_asset_paths () |> List.iter copy_ssg_asset;
      let count = ref 0 in
      static_routes
      |> List.iter (fun (route_entry : route_entry) ->
          if route_entry.params = [] then (
            (* Static page without params *)
            let output = ssg_output_path route_entry.route in
            ensure_directory (Filename.dirname output);
            let request_target =
              if route_entry.route = "" then "/" else "/" ^ route_entry.route
            in
            let html = render_ssg_page route_entry request_target [] in
            write_file output html;
            Printf.printf "  %s -> %s\n%!" ("/" ^ route_entry.route) output;
            incr count)
          else
            (* Dynamic page with static_paths *)
            match route_entry.static_paths with
            | None ->
                Printf.eprintf
                  "  warning: %s is static with params but no static_paths\n%!"
                  route_entry.source_file
            | Some get_paths ->
                let param_sets = get_paths () in
                param_sets
                |> List.iter (fun params ->
                    (* Build the route string with params substituted *)
                    let route_str =
                      route_entry.segments
                      |> List.map (fun seg ->
                          match seg with
                          | Static s -> s
                          | Param (name, _) -> (
                              match List.assoc_opt name params with
                              | Some v -> v
                              | None -> name))
                      |> String.concat "/"
                    in
                    let output = ssg_output_path route_str in
                    ensure_directory (Filename.dirname output);
                    let html =
                      render_ssg_page route_entry ("/" ^ route_str) params
                    in
                    write_file output html;
                    Printf.printf "  /%s -> %s\n%!" route_str output;
                    incr count));
      Printf.printf "SSG: rendered %d page(s) to %s/\n%!" !count ssg_output_dir

let start_generated ~(pages : generated_route list)
    ~(api_routes : generated_api_route list) ~lookup_server_function =
  match
    ( runtime_routes_of_generated_routes pages,
      runtime_api_routes_of_generated_routes api_routes )
  with
  | Ok routes, Ok api_routes ->
      start_runtime_routes routes api_routes ~lookup_server_function
  | Error message, _ | _, Error message ->
      Printf.eprintf "Error: %s\n%!" message;
      exit 1
