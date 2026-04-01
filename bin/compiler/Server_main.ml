open Utopia_types

let ocaml_expr_of_param_kind = function
  | Single -> "Utopia_types.Single"
  | Catch_all -> "Utopia_types.Catch_all"
  | Optional_catch_all -> "Utopia_types.Optional_catch_all"

let ocaml_expr_of_kind = function
  | Code_page -> "Utopia_server.Generated_route.code"
  | Markdown_page -> "Utopia_server.Generated_route.markdown"

let ocaml_string_list values =
  values
  |> List.map (fun value -> Printf.sprintf "%S" value)
  |> String.concat "; " |> Printf.sprintf "[%s]"

let ocaml_params_list params =
  params
  |> List.map (fun (name, kind) ->
      Printf.sprintf "(%S, %s)" name (ocaml_expr_of_param_kind kind))
  |> String.concat "; " |> Printf.sprintf "[%s]"

let ocaml_layout_renderers layouts =
  layouts
  |> List.map (fun layout ->
      Printf.sprintf
        "(fun children -> Utopia_server.wrap_raw_inner_html_element (%s.make \
         ~children ()))"
        (Names.compiled_page_module_name_of_source layout))
  |> String.concat "; " |> Printf.sprintf "[%s]"

let ocaml_render_expr entry =
  match entry.Routes.kind with
  | Code_page ->
      Printf.sprintf
        "~render:(fun () -> Utopia_server.wrap_raw_inner_html_element (%s.make \
         ()))"
        (Names.compiled_page_module_name_of_source entry.Routes.source_file)
  | Markdown_page -> ""

let ocaml_metadata_expr entry =
  if entry.Routes.has_metadata then
    Printf.sprintf "~metadata:(Some %s.metadata)"
      (Names.compiled_page_module_name_of_source entry.Routes.source_file)
  else "~metadata:None"

let slash_matcher matcher = if matcher = "" then "/" else "/" ^ matcher

let ocaml_page_element_expr entry =
  match entry.Routes.kind with
  | Code_page ->
      Printf.sprintf "Utopia_server.wrap_raw_inner_html_element (%s.make ())"
        (Names.compiled_page_module_name_of_source entry.Routes.source_file)
  | Markdown_page ->
      Printf.sprintf "Utopia_server.render_markdown_body %S"
        entry.Routes.source_file

let ocaml_route_expr ~path ~layout_expr ~pageconsumer_expr =
  match pageconsumer_expr with
  | None ->
      Printf.sprintf
        "Utopia.Router.Boundary.make ~path:%S ~layout:(%s) ~pageconsumer:None \
         ()"
        path layout_expr
  | Some pageconsumer_expr ->
      Printf.sprintf
        "Utopia.Router.Boundary.make ~path:%S ~layout:(%s) ~pageconsumer:(Some \
         (%s)) ()"
        path layout_expr pageconsumer_expr

let route_layout_nodes entry =
  entry.Routes.layouts
  |> List.map (fun layout ->
      let path = Routes.layout_route_path layout in
      let layout_expr =
        Printf.sprintf
          "Utopia_server.wrap_raw_inner_html_element (%s.make \
           ~children:(Utopia.Router.Boundary.PageConsumer.make ()) ())"
          (Names.compiled_page_module_name_of_source layout)
      in
      (path, layout_expr))

let pass_through_layout_expr =
  "Utopia.PassThroughLayout.make \
   ~children:(Utopia.Router.Boundary.PageConsumer.make ()) ()"

let route_root_has_layout entry =
  route_layout_nodes entry |> List.exists (fun (path, _expr) -> path = "/")

let route_path_prefixes path =
  let segments =
    path |> String.split_on_char '/'
    |> List.filter (fun segment -> segment <> "")
  in
  let rec loop current acc remaining =
    match remaining with
    | [] -> List.rev acc
    | segment :: rest ->
        let next =
          if current = "" then "/" ^ segment else current ^ "/" ^ segment
        in
        loop next (next :: acc) rest
  in
  loop "" [] segments

let route_descendant_boundary_nodes entry =
  let page_path = slash_matcher entry.Routes.matcher in
  let descendant_layout_nodes =
    route_layout_nodes entry |> List.filter (fun (path, _expr) -> path <> "/")
  in
  let boundary_paths =
    match List.rev (route_path_prefixes page_path) with
    | [] -> []
    | _page_path :: ancestor_paths_rev ->
        let ancestor_paths = List.rev ancestor_paths_rev in
        let has_same_path_layout =
          descendant_layout_nodes
          |> List.exists (fun (path, _expr) -> path = page_path)
        in
        if has_same_path_layout then ancestor_paths @ [ page_path ]
        else ancestor_paths
  in
  boundary_paths
  |> List.map (fun path ->
      let layout_expr =
        descendant_layout_nodes
        |> List.find_map (fun (candidate_path, candidate_expr) ->
            if candidate_path = path then Some candidate_expr else None)
        |> Option.value ~default:pass_through_layout_expr
      in
      (path, layout_expr))

let ocaml_router_root_layout_expr entry =
  match
    route_layout_nodes entry |> List.find_opt (fun (path, _expr) -> path = "/")
  with
  | Some (_path, layout_expr) -> layout_expr
  | None -> pass_through_layout_expr

let rec ocaml_router_child_expr entry current_path remaining_layouts =
  match remaining_layouts with
  | (path, layout_expr) :: rest ->
      let child_expr = ocaml_router_child_expr entry path rest in
      ocaml_route_expr ~path ~layout_expr ~pageconsumer_expr:(Some child_expr)
  | [] ->
      let page_expr = ocaml_page_element_expr entry in
      if slash_matcher entry.Routes.matcher = current_path then page_expr
      else
        ocaml_route_expr
          ~path:(slash_matcher entry.Routes.matcher)
          ~layout_expr:page_expr ~pageconsumer_expr:None

let remaining_layouts_after parent_path layouts =
  let rec loop layouts =
    match layouts with
    | [] -> []
    | (path, _expr) :: rest when path = parent_path -> rest
    | _ :: rest -> loop rest
  in
  if parent_path = "/" then layouts else loop layouts

let ocaml_router_tree_expr entry =
  let page_path = slash_matcher entry.Routes.matcher in
  let root_has_layout = route_root_has_layout entry in
  if page_path = "/" && not root_has_layout then
    ocaml_route_expr ~path:"/"
      ~layout_expr:(ocaml_page_element_expr entry)
      ~pageconsumer_expr:None
  else
    let child_expr =
      ocaml_router_child_expr entry "/" (route_descendant_boundary_nodes entry)
    in
    ocaml_route_expr ~path:"/"
      ~layout_expr:(ocaml_router_root_layout_expr entry)
      ~pageconsumer_expr:(Some child_expr)

let ocaml_router_subtree_expr entry =
  let cases =
    "/" :: (route_descendant_boundary_nodes entry |> List.map fst)
    |> List.sort_uniq String.compare
  in
  cases
  |> List.map (fun parent_path ->
      let child_expr =
        ocaml_router_child_expr entry parent_path
          (remaining_layouts_after parent_path
             (route_descendant_boundary_nodes entry))
      in
      Printf.sprintf "  | %S -> Some (%s)" parent_path child_expr)
  |> String.concat "\n"

let ocaml_router_bindings entry =
  let tree_name =
    Names.generated_route_binding_name entry.Routes.source_file "router_tree"
  in
  let shell_name =
    Names.generated_route_binding_name entry.Routes.source_file "router_shell"
  in
  let subtree_name =
    Names.generated_route_binding_name entry.Routes.source_file "router_subtree"
  in
  let tree_expr = ocaml_router_tree_expr entry in
  let subtree_cases = ocaml_router_subtree_expr entry in
  Printf.sprintf
    "let %s () =\n\
    \  %s\n\n\
     let %s location =\n\
    \  Utopia.make ~initialPath:location ~children:(%s ()) ()\n\n\
     let %s parent_route =\n\
    \  match parent_route with\n\
     %s\n\
    \  | _ -> None\n"
    tree_name tree_expr shell_name tree_name subtree_name subtree_cases

let generate route_entries =
  let sorted_entries =
    route_entries
    |> List.sort (fun left right ->
        String.compare left.Routes.route right.Routes.route)
  in
  let router_bindings =
    sorted_entries |> List.map ocaml_router_bindings |> String.concat "\n"
  in
  let route_lines =
    sorted_entries
    |> List.map (fun entry ->
        let shell_name =
          Names.generated_route_binding_name entry.Routes.source_file
            "router_shell"
        in
        let tree_name =
          Names.generated_route_binding_name entry.Routes.source_file
            "router_tree"
        in
        let subtree_name =
          Names.generated_route_binding_name entry.Routes.source_file
            "router_subtree"
        in
        Printf.sprintf
          "  %s ~route:%S ~matcher:%S ~params:%s ~source_file:%S ~layouts:%s \
           %s %s ~layout_renderers:%s ~router_shell:%s ~router_tree:%s \
           ~router_subtree:%s;"
          (ocaml_expr_of_kind entry.Routes.kind)
          entry.Routes.route entry.Routes.matcher
          (ocaml_params_list entry.Routes.params)
          entry.Routes.source_file
          (ocaml_string_list entry.Routes.layouts)
          (ocaml_render_expr entry)
          (ocaml_metadata_expr entry)
          (ocaml_layout_renderers entry.Routes.layouts)
          shell_name tree_name subtree_name)
    |> String.concat "\n"
  in
  Printf.sprintf
    "%s\n\
     let generated_routes = [\n\
     %s\n\
     ]\n\n\
     let () = Utopia_server.start_generated generated_routes \
     ~lookup_server_function:FunctionReferences.get\n"
    router_bindings route_lines
