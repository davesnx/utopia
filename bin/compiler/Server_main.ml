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

let ocaml_make_page_expr entry =
  match entry.Routes.kind with
  | Code_page ->
      Printf.sprintf
        "fun () -> Utopia_server.wrap_raw_inner_html_element (%s.make ())"
        (Names.compiled_page_module_name_of_source entry.Routes.source_file)
  | Markdown_page ->
      Printf.sprintf "fun () -> Utopia_server.render_markdown_body %S"
        entry.Routes.source_file

let ocaml_metadata_expr entry =
  if entry.Routes.has_metadata then
    Printf.sprintf "~metadata:(Some %s.metadata)"
      (Names.compiled_page_module_name_of_source entry.Routes.source_file)
  else "~metadata:None"

let ocaml_layout_info layout =
  let path = Routes.layout_route_path layout in
  Printf.sprintf
    "{ Utopia_route_builder.path = %S;\n\
    \      render = (fun children ->\n\
    \        Utopia_server.wrap_raw_inner_html_element (%s.make ~children ())) \
     }"
    path
    (Names.compiled_page_module_name_of_source layout)

let ocaml_layout_infos_list layouts =
  if layouts = [] then "[]"
  else
    layouts |> List.map ocaml_layout_info |> String.concat ";\n    "
    |> Printf.sprintf "[\n    %s\n  ]"

let ocaml_route_bindings entry =
  let make_page_name =
    Names.generated_route_binding_name entry.Routes.source_file "make_page"
  in
  let layouts_name =
    Names.generated_route_binding_name entry.Routes.source_file "layouts"
  in
  let router_name =
    Names.generated_route_binding_name entry.Routes.source_file "router"
  in
  Printf.sprintf
    "let %s =\n\
    \  %s\n\n\
     let %s =\n\
    \  %s\n\n\
     let %s =\n\
    \  Utopia_route_builder.build_router ~matcher:%S ~make_page:%s ~layouts:%s\n"
    make_page_name
    (ocaml_make_page_expr entry)
    layouts_name
    (ocaml_layout_infos_list entry.Routes.layouts)
    router_name entry.Routes.matcher make_page_name layouts_name

let ocaml_route_entry entry =
  let make_page_name =
    Names.generated_route_binding_name entry.Routes.source_file "make_page"
  in
  let layouts_name =
    Names.generated_route_binding_name entry.Routes.source_file "layouts"
  in
  let router_name =
    Names.generated_route_binding_name entry.Routes.source_file "router"
  in
  let kind_constructor =
    match entry.Routes.kind with
    | Code_page -> "Utopia_server.Generated_route.code"
    | Markdown_page -> "Utopia_server.Generated_route.markdown"
  in
  let render_arg =
    match entry.Routes.kind with
    | Code_page -> Printf.sprintf " ~render:%s" make_page_name
    | Markdown_page -> ""
  in
  Printf.sprintf
    "  %s\n\
    \    ~route:%S ~matcher:%S ~params:%s ~source_file:%S\n\
    \    ~layouts:%s%s %s\n\
    \    ~layout_renderers:(List.map (fun (l : \
     Utopia_route_builder.layout_info) -> l.render) %s)\n\
    \    ~router_shell:%s.Utopia_route_builder.shell\n\
    \    ~router_tree:%s.Utopia_route_builder.tree\n\
    \    ~router_subtree:%s.Utopia_route_builder.subtree;"
    kind_constructor entry.Routes.route entry.Routes.matcher
    (ocaml_params_list entry.Routes.params)
    entry.Routes.source_file
    (ocaml_string_list entry.Routes.layouts)
    render_arg
    (ocaml_metadata_expr entry)
    layouts_name router_name router_name router_name

let generate route_entries =
  let sorted_entries =
    route_entries
    |> List.sort (fun left right ->
        String.compare left.Routes.route right.Routes.route)
  in
  let route_bindings =
    sorted_entries |> List.map ocaml_route_bindings |> String.concat "\n"
  in
  let route_lines =
    sorted_entries |> List.map ocaml_route_entry |> String.concat "\n"
  in
  Printf.sprintf
    "%s\n\
     let generated_routes = [\n\
     %s\n\
     ]\n\n\
     let () = Utopia_server.start_generated generated_routes \
     ~lookup_server_function:FunctionReferences.get\n"
    route_bindings route_lines
