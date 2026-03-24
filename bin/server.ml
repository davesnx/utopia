type page_kind =
  | Code_page
  | Markdown_page

type param_kind =
  | Single
  | Catch_all
  | Optional_catch_all

type route_segment =
  | Static of string
  | Param of string * param_kind

type param_value =
  | One of string
  | Many of string list

type route_entry = {
  route : string;
  params : (string * param_kind) list;
  layouts : string list;
  kind : page_kind;
  source_file : string;
  segments : route_segment list;
}

let routes_manifest_file = "_utopia/routes.manifest"
let scripts_manifest_file = "_utopia/scripts.manifest"

let read_file file =
  In_channel.with_open_bin file (fun channel -> In_channel.input_all channel)

(* Mtime-based page cache: avoids re-reading and re-rendering on every request.
   Each entry stores (mtime, rendered_html). A stat() call (~1us) gates the
   much more expensive open+read+close+escape+render cycle (~20-50us+). *)
type cache_entry = { mtime : float; html : string }

let page_cache : (string, cache_entry) Hashtbl.t = Hashtbl.create 64

let file_mtime file =
  try Some (Unix.stat file).Unix.st_mtime with Unix.Unix_error _ -> None

let split_fields line =
  match String.split_on_char '\t' line with
  | [ route; kind; source_file; matcher; params; layouts ] ->
      Some (route, kind, source_file, matcher, params, layouts)
  | _ -> None

let split_script_fields line =
  match String.split_on_char '\t' line with
  | [ route; assets ] -> Some (route, assets)
  | [ route ] -> Some (route, "")
  | _ -> None

let parse_kind = function
  | "code" -> Some Code_page
  | "markdown" -> Some Markdown_page
  | _ -> None

let parse_param_kind = function
  | "single" -> Some Single
  | "catch_all" -> Some Catch_all
  | "optional_catch_all" -> Some Optional_catch_all
  | _ -> None

let parse_params params =
  if params = "" then Ok []
  else
    params
    |> String.split_on_char ','
    |> List.fold_left
         (fun acc entry ->
           match (acc, String.split_on_char ':' entry) with
           | Error _ as error, _ -> error
           | Ok _, [ _name ] -> Error (Printf.sprintf "Invalid params spec: %s" entry)
           | Ok parsed, [ name; kind ] -> (
               match parse_param_kind kind with
               | None -> Error (Printf.sprintf "Invalid params kind: %s" kind)
               | Some parsed_kind -> Ok ((name, parsed_kind) :: parsed))
           | Ok _, _ -> Error (Printf.sprintf "Invalid params spec: %s" entry))
         (Ok [])
    |> Result.map List.rev

let parse_layouts layouts =
  if layouts = "" then []
  else layouts |> String.split_on_char ';' |> List.filter (fun item -> item <> "")

let parse_matcher_segment segment =
  if String.length segment >= 2 && String.sub segment 0 2 = "**" then
    Ok (Param (String.sub segment 2 (String.length segment - 2), Optional_catch_all))
  else if String.length segment >= 1 && segment.[0] = '*' then
    Ok (Param (String.sub segment 1 (String.length segment - 1), Catch_all))
  else if String.length segment >= 1 && segment.[0] = ':' then
    Ok (Param (String.sub segment 1 (String.length segment - 1), Single))
  else Ok (Static segment)

let parse_matcher matcher =
  if matcher = "" then Ok []
  else
    matcher
    |> String.split_on_char '/'
    |> List.fold_left
         (fun acc segment ->
           match (acc, parse_matcher_segment segment) with
           | Error _ as error, _ -> error
           | Ok _, Error message -> Error message
           | Ok segments, Ok parsed_segment -> Ok (parsed_segment :: segments))
         (Ok [])
    |> Result.map List.rev

let specificity_of_segment = function
  | Static _ -> 4
  | Param (_, Single) -> 3
  | Param (_, Catch_all) -> 2
  | Param (_, Optional_catch_all) -> 1

let compare_route_specificity left right =
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

let load_routes () =
  if not (Sys.file_exists routes_manifest_file) then
    Error
      (Printf.sprintf
          "Route manifest not found at %s. Run `dune exec bin/compiler.exe` first."
          routes_manifest_file)
  else
    let lines = read_file routes_manifest_file |> String.split_on_char '\n' in
    lines
    |> List.filter (fun line -> String.trim line <> "")
    |> List.fold_left
         (fun acc line ->
           match (acc, split_fields line) with
           | Error _ as error, _ -> error
           | Ok _, None -> Error (Printf.sprintf "Invalid route entry: %s" line)
           | Ok routes, Some (route, kind, source_file, matcher, params, layouts) -> (
               match parse_kind kind with
               | None -> Error (Printf.sprintf "Invalid route kind: %s" kind)
               | Some parsed_kind -> (
                   match (parse_matcher matcher, parse_params params) with
                   | Error message, _ | _, Error message -> Error message
                   | Ok segments, Ok parsed_params ->
                       Ok
                         ({
                            route;
                            params = parsed_params;
                            layouts = parse_layouts layouts;
                            kind = parsed_kind;
                            source_file;
                            segments;
                          }
                         :: routes))))
         (Ok [])
    |> Result.map (fun routes ->
           routes
           |> List.rev
           |> List.sort compare_route_specificity)

let parse_assets assets =
  if assets = "" then []
  else assets |> String.split_on_char ';' |> List.filter (fun asset -> asset <> "")

let load_scripts_manifest () =
  if not (Sys.file_exists scripts_manifest_file) then []
  else
    let lines = read_file scripts_manifest_file |> String.split_on_char '\n' in
    lines
    |> List.filter (fun line -> String.trim line <> "")
    |> List.fold_left
         (fun acc line ->
           match split_script_fields line with
           | None -> acc
           | Some (route, assets) -> (route, parse_assets assets) :: acc)
         []
    |> List.rev

let scripts_for_route scripts_manifest route =
  match List.assoc_opt route scripts_manifest with
  | Some scripts -> scripts
  | None -> []

let starts_with text prefix =
  let text_len = String.length text in
  let prefix_len = String.length prefix in
  text_len >= prefix_len && String.sub text 0 prefix_len = prefix

let contains_path_traversal path =
  path
  |> String.split_on_char '/'
  |> List.exists (fun segment -> segment = "..")

let asset_roots = [ "_utopia"; "_build/default/_utopia" ]

let first_existing_asset relative_path =
  asset_roots
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
  if target = "/" then ""
  else if String.length target > 0 && target.[0] = '/' then
    String.sub target 1 (String.length target - 1)
  else target

let target_segments target =
  if target = "" then []
  else target |> String.split_on_char '/' |> List.filter (fun segment -> segment <> "")

let escape_html text =
  let buffer = Buffer.create (String.length text) in
  String.iter
    (function
      | '&' -> Buffer.add_string buffer "&amp;"
      | '<' -> Buffer.add_string buffer "&lt;"
      | '>' -> Buffer.add_string buffer "&gt;"
      | '"' -> Buffer.add_string buffer "&quot;"
      | '\'' -> Buffer.add_string buffer "&#39;"
      | c -> Buffer.add_char buffer c)
    text;
  Buffer.contents buffer

let html_page ~title ~body =
  Printf.sprintf
    "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><meta name=\"viewport\" \
     content=\"width=device-width, initial-scale=1\"><title>%s</title></head><body>%s</body></html>"
    (escape_html title) body

let render_script_tags script_assets =
  script_assets
  |> List.map (fun asset ->
         Printf.sprintf "<script src=\"/%s\" defer></script>" (escape_html asset))
  |> String.concat ""

let render_param_value = function
  | One value -> value
  | Many values -> String.concat "/" values

let render_params params =
  if params = [] then ""
  else
    params
    |> List.map (fun (name, value) ->
           Printf.sprintf "<li><code>%s</code> = %s</li>" (escape_html name)
             (escape_html (render_param_value value)))
    |> String.concat ""
    |> Printf.sprintf "<h2>Params</h2><ul>%s</ul>"

let wrap_with_layouts layouts content =
  List.fold_right
    (fun layout acc ->
      Printf.sprintf
        "<section><div>Layout: <code>%s</code></div>%s</section>"
        (escape_html layout) acc)
    layouts content

let render_code_page_fresh route source_file params layouts script_assets =
  let source = read_file source_file |> escape_html in
  let content =
    Printf.sprintf
      "<main><h1>/%s</h1><p>Code page from <code>%s</code>.</p>%s<pre>%s</pre></main>"
      (escape_html route)
      (escape_html source_file)
      (render_params params)
      source
  in
  html_page ~title:route
    ~body:(wrap_with_layouts layouts (content ^ render_script_tags script_assets))

let render_markdown_page_fresh source_file params layouts script_assets =
  let markdown = read_file source_file in
  let doc = Cmarkit.Doc.of_string ~layout:true ~strict:false markdown in
  Cmarkit_html.of_doc ~safe:false doc
  |> fun body ->
  html_page ~title:source_file
    ~body:(wrap_with_layouts layouts
             (Printf.sprintf "<main>%s%s</main>%s" (render_params params) body
                (render_script_tags script_assets)))

(* Cache key combines source_file + route + params to handle the same file
   being served at different param values (e.g. /users/1 vs /users/2). *)
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
  | Some cached, Some mtime when Float.equal cached.mtime mtime -> cached.html
  | _, Some mtime ->
      let html = render_fn () in
      Hashtbl.replace page_cache key { mtime; html };
      html
  | _, None ->
      (* File disappeared; render anyway, don't cache *)
      render_fn ()

let render_code_page route source_file params layouts script_assets =
  let key = make_cache_key source_file route params in
  render_cached ~key ~source_file (fun () ->
      render_code_page_fresh route source_file params layouts script_assets)

let render_markdown_page source_file params layouts script_assets =
  let key = make_cache_key source_file "" params in
  render_cached ~key ~source_file (fun () ->
      render_markdown_page_fresh source_file params layouts script_assets)

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

let find_match routes path_segments =
  routes
  |> List.find_map (fun route ->
         match match_segments route.segments path_segments [] with
         | None -> None
         | Some params -> Some (route, params))

let render_index routes =
  let links =
    routes
    |> List.map (fun { route; kind; source_file; params; layouts; _ } ->
           let label =
             match kind with
             | Code_page -> "code"
             | Markdown_page -> "markdown"
           in
           let param_text =
             params
             |> List.map fst
             |> function
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
           Printf.sprintf
             "<li><a href=\"/%s\">/%s</a> - %s%s%s (<code>%s</code>)</li>"
             (escape_html route)
             (escape_html route)
             label param_text layout_text
             (escape_html source_file))
    |> String.concat ""
  in
  html_page ~title:"Utopia dev router"
    ~body:(Printf.sprintf "<main><h1>Routes</h1><ul>%s</ul></main>" links)

let route_request routes index_html scripts_manifest request =
  let target = Dream.target request |> normalize_target in
  if starts_with target "target/" then serve_asset target
  else
  let segments = target_segments target in
  if segments = [] then Dream.html index_html
  else
    match find_match routes segments with
    | None -> Dream.respond ~status:`Not_Found "Route not found"
    | Some ({ route; kind = Code_page; source_file; layouts; _ }, params) ->
        let script_assets = scripts_for_route scripts_manifest route in
        Dream.html (render_code_page route source_file params layouts script_assets)
    | Some ({ route; kind = Markdown_page; source_file; layouts; _ }, params) ->
        let script_assets = scripts_for_route scripts_manifest route in
        Dream.html
          (render_markdown_page source_file params layouts script_assets)

let port_from_env () =
  match Sys.getenv_opt "PORT" with
  | None -> 8080
  | Some value -> (
      try int_of_string value
      with Failure _ ->
        Printf.eprintf "Invalid PORT value '%s', defaulting to 8080\n" value;
        8080)

let () =
  Printexc.record_backtrace true;
  Logs.set_level (Some Info);
  Logs.set_reporter (Logs_fmt.reporter ());
  let routes =
    match load_routes () with
    | Ok routes ->
        Printf.printf "Loaded %d routes from %s\n%!" (List.length routes) routes_manifest_file;
        routes
    | Error message ->
        Printf.eprintf "Error: %s\n%!" message;
        exit 1
  in
  let scripts_manifest = load_scripts_manifest () in
  let index_html = render_index routes in
  let enable_logging = Sys.getenv_opt "NO_LOG" = None in
  let handler = route_request routes index_html scripts_manifest in
  let pipeline =
    if enable_logging then Dream.logger @@ handler else handler
  in
  Dream.run ~port:(port_from_env ()) pipeline
