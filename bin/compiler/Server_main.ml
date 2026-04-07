open Utopia_types

let ocaml_expr_of_param_kind = function
  | Single -> "Utopia_types.Single"
  | Catch_all -> "Utopia_types.Catch_all"
  | Optional_catch_all -> "Utopia_types.Optional_catch_all"

let ocaml_string_list values =
  values
  |> List.map (fun value -> Printf.sprintf "%S" value)
  |> String.concat "; " |> Printf.sprintf "[%s]"

let ocaml_params_list params =
  params
  |> List.map (fun (name, kind) ->
      Printf.sprintf "(%S, %s)" name (ocaml_expr_of_param_kind kind))
  |> String.concat "; " |> Printf.sprintf "[%s]"

let page_render_branch (entry : Routes.route_entry) =
  match entry.kind with
  | Markdown_page -> None
  | Code_page ->
      Some
        (Printf.sprintf
           "  | %S -> Some (fun () -> \
            Utopia_server.wrap_raw_inner_html_element (%s.make (%s.makeProps \
            ())))"
           entry.source_file
           (Names.compiled_page_module_name_of_source entry.source_file)
           (Names.compiled_page_module_name_of_source entry.source_file))

let page_metadata_branch (entry : Routes.route_entry) =
  if not entry.has_metadata then None
  else
    Some
      (Printf.sprintf "  | %S -> Some %s.metadata" entry.source_file
         (Names.compiled_page_module_name_of_source entry.source_file))

let page_static_paths_branch (entry : Routes.route_entry) =
  if not entry.has_static_paths then None
  else
    Some
      (Printf.sprintf "  | %S -> Some %s.static_paths" entry.source_file
         (Names.compiled_page_module_name_of_source entry.source_file))

let layout_info_branch source_file =
  let path = Routes.layout_route_path source_file in
  Printf.sprintf
    "  | %S -> Some { Utopia_route_builder.path = %S; render = (fun children \
     -> Utopia_server.wrap_raw_inner_html_element (%s.make (%s.makeProps \
     ~children ()))) }"
    source_file path
    (Names.compiled_page_module_name_of_source source_file)
    (Names.compiled_page_module_name_of_source source_file)

let api_handler_branch (entry : Routes.api_route_entry) =
  Printf.sprintf "  | %S -> Some %s.handler" entry.source_file entry.module_name

let api_middleware_branch source_file =
  Printf.sprintf "  | %S -> Some %s.middleware" source_file
    (Names.compiled_api_module_name_of_source source_file)

let render_match_with_default ~subject ~default branches =
  match branches with
  | [] -> default
  | _ ->
      String.concat "\n"
        ([ "match " ^ subject ^ " with" ] @ branches @ [ "  | _ -> " ^ default ])

let resolver_argument_name branches =
  match branches with [] -> "_source_file" | _ -> "source_file"

let render_route_modules route_entries api_entries =
  let page_render_branches =
    route_entries |> List.filter_map page_render_branch
  in
  let page_render_arg = resolver_argument_name page_render_branches in
  let page_metadata_branches =
    route_entries |> List.filter_map page_metadata_branch
  in
  let page_metadata_arg = resolver_argument_name page_metadata_branches in
  let page_static_paths_branches =
    route_entries |> List.filter_map page_static_paths_branch
  in
  let page_static_paths_arg =
    resolver_argument_name page_static_paths_branches
  in
  let layout_sources =
    route_entries
    |> List.concat_map (fun (entry : Routes.route_entry) -> entry.layouts)
    |> List.sort_uniq String.compare
  in
  let layout_branches = layout_sources |> List.map layout_info_branch in
  let layout_arg = resolver_argument_name layout_branches in
  let api_handler_branches = api_entries |> List.map api_handler_branch in
  let api_handler_arg = resolver_argument_name api_handler_branches in
  let api_middleware_sources =
    api_entries
    |> List.concat_map (fun (entry : Routes.api_route_entry) ->
        entry.middlewares)
    |> List.sort_uniq String.compare
  in
  let api_middleware_branches =
    api_middleware_sources |> List.map api_middleware_branch
  in
  let api_middleware_arg = resolver_argument_name api_middleware_branches in
  String.concat "\n"
    [
      "module Route_modules = struct";
      Printf.sprintf "  let resolve_page_render %s =" page_render_arg;
      render_match_with_default ~subject:page_render_arg ~default:"None"
        page_render_branches
      |> String.split_on_char '\n'
      |> List.map (fun line -> "  " ^ line)
      |> String.concat "\n";
      "";
      Printf.sprintf "  let resolve_page_metadata %s =" page_metadata_arg;
      render_match_with_default ~subject:page_metadata_arg ~default:"None"
        page_metadata_branches
      |> String.split_on_char '\n'
      |> List.map (fun line -> "  " ^ line)
      |> String.concat "\n";
      "";
      Printf.sprintf "  let resolve_page_static_paths %s ="
        page_static_paths_arg;
      render_match_with_default ~subject:page_static_paths_arg ~default:"None"
        page_static_paths_branches
      |> String.split_on_char '\n'
      |> List.map (fun line -> "  " ^ line)
      |> String.concat "\n";
      "";
      Printf.sprintf "  let resolve_layout_info %s =" layout_arg;
      render_match_with_default ~subject:layout_arg ~default:"None"
        layout_branches
      |> String.split_on_char '\n'
      |> List.map (fun line -> "  " ^ line)
      |> String.concat "\n";
      "";
      "  let resolve_layout_infos source_files =";
      "    source_files |> List.filter_map resolve_layout_info";
      "";
      "  let resolve_markdown_entry source_file markdown_entries =";
      "    markdown_entries";
      "    |> List.find_opt (fun (entry : Routes.Markdown.entry) ->";
      "           String.equal entry.source_file source_file)";
      "";
      "  let resolve_page markdown_entries (meta : \
       Utopia_types.page_route_meta) =";
      "    let layouts = resolve_layout_infos meta.layouts in";
      "    let metadata = resolve_page_metadata meta.source_file in";
      "    let static_paths =";
      "      if meta.has_static_paths then resolve_page_static_paths \
       meta.source_file";
      "      else None";
      "    in";
      "    match meta.kind with";
      "    | Utopia_types.Code_page -> (";
      "        match resolve_page_render meta.source_file with";
      "        | None -> None";
      "        | Some render ->";
      "            let router =";
      "              Utopia_route_builder.build_router ~matcher:meta.matcher \
       ~make_page:render ~layouts";
      "            in";
      "            Some";
      "              (Utopia_server.Generated_route.code ~route:meta.route \
       ~matcher:meta.matcher";
      "                 ~params:meta.params ~source_file:meta.source_file \
       ~layouts:meta.layouts";
      "                 ~render ~metadata ~layout_renderers:(List.map (fun (l \
       : Utopia_route_builder.layout_info) -> l.render) layouts)";
      "                 ~router_shell:router.Utopia_route_builder.shell";
      "                 ~router_tree:router.Utopia_route_builder.tree";
      "                 ~router_subtree:router.Utopia_route_builder.subtree";
      "                 ~static:meta.static ~static_paths ()))";
      "    | Utopia_types.Markdown_page -> (";
      "        match resolve_markdown_entry meta.source_file markdown_entries \
       with";
      "        | Some markdown ->";
      "            let render = fun () -> Utopia_server.render_markdown_body \
       markdown.body in";
      "            let router =";
      "              Utopia_route_builder.build_router ~matcher:meta.matcher \
       ~make_page:render ~layouts";
      "            in";
      "            Some";
      "              (Utopia_server.Generated_route.markdown ~route:meta.route \
       ~matcher:meta.matcher";
      "                 ~params:meta.params ~source_file:meta.source_file \
       ~layouts:meta.layouts";
      "                 ~metadata";
      "                 ~layout_renderers:(List.map (fun (l : \
       Utopia_route_builder.layout_info) -> l.render) layouts)";
      "                 ~router_shell:router.Utopia_route_builder.shell";
      "                 ~router_tree:router.Utopia_route_builder.tree";
      "                 ~router_subtree:router.Utopia_route_builder.subtree";
      "                 ~markdown:";
      "                   (Utopia_server.make_markdown_payload \
       ~markdown_body:markdown.body";
      "                      ~frontmatter_object:markdown.frontmatter \
       ?meta_title:markdown.title";
      "                      ?meta_description:markdown.description ())";
      "                 ~static:meta.static ())";
      "        | None -> None)";
      "";
      "  let resolve_pages markdown_entries metadata =";
      "    metadata |> List.filter_map (resolve_page markdown_entries)";
      "";
      Printf.sprintf "  let resolve_api_handler %s =" api_handler_arg;
      render_match_with_default ~subject:api_handler_arg ~default:"None"
        api_handler_branches
      |> String.split_on_char '\n'
      |> List.map (fun line -> "  " ^ line)
      |> String.concat "\n";
      "";
      Printf.sprintf "  let resolve_api_middleware %s =" api_middleware_arg;
      render_match_with_default ~subject:api_middleware_arg ~default:"None"
        api_middleware_branches
      |> String.split_on_char '\n'
      |> List.map (fun line -> "  " ^ line)
      |> String.concat "\n";
      "";
      "  let resolve_api_middlewares source_files =";
      "    source_files |> List.filter_map resolve_api_middleware";
      "";
      "  let resolve_api (metadata : Utopia_types.api_route_meta list) =";
      "    metadata";
      "    |> List.filter_map (fun (meta : Utopia_types.api_route_meta) ->";
      "           match resolve_api_handler meta.source_file with";
      "           | None -> None";
      "           | Some handler ->";
      "               Some";
      "                 (Utopia_server.Generated_api_route.make \
       ~route:meta.route";
      "                    ~matcher:meta.matcher ~params:meta.params \
       ~source_file:meta.source_file";
      "                    ~middlewares:(resolve_api_middlewares \
       meta.middlewares) ~handler ()))";
      "end";
    ]

let generate route_entries api_entries =
  let route_modules = render_route_modules route_entries api_entries in
  String.concat "\n\n"
    [
      route_modules;
      "let page_meta = Routes.get_all ()";
      "let markdown_meta = Routes.Markdown.get_all ()";
      "let api_meta = Routes.Api.get_all ()";
      "let pages = Route_modules.resolve_pages markdown_meta page_meta";
      "let api_routes = Route_modules.resolve_api api_meta";
      "";
      "let () =";
      "  match Array.to_list Sys.argv with";
      "  | [ _; \"--ssg\" ] -> Utopia_server.ssg_generated pages";
      "  | _ ->";
      "      Utopia_server.start_generated ~pages ~api_routes \
       ~lookup_server_function:FunctionReferences.get";
    ]
