open Utopia_types
open Ppxlib
open Ocaml_gen

type tree = {
  route_entry : Routes.route_entry option;
  children : (route_segment * tree) list;
}

let empty_tree = { route_entry = None; children = [] }

let route_segments (entry : Routes.route_entry) =
  if entry.Routes.route = "" then []
  else
    entry.Routes.route |> String.split_on_char '/'
    |> List.map (fun segment ->
        match Routes.parse_param_segment segment with
        | Ok parsed -> parsed
        | Error message -> invalid_arg message)

let specificity_of_segment = function
  | Static _ -> 4
  | Param (_, Single) -> 3
  | Param (_, Catch_all) -> 2
  | Param (_, Optional_catch_all) -> 1

let compare_route_specificity (left : Routes.route_entry)
    (right : Routes.route_entry) =
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
  let result =
    compare_scores
      (List.map specificity_of_segment (route_segments left))
      (List.map specificity_of_segment (route_segments right))
  in
  if result = 0 then String.compare left.Routes.route right.Routes.route
  else result

let rec add_entry tree segments (entry : Routes.route_entry) =
  match segments with
  | [] -> { tree with route_entry = Some entry }
  | segment :: rest ->
      let existing_child =
        tree.children
        |> List.find_opt (fun (candidate, _child) -> candidate = segment)
      in
      let child_tree =
        existing_child |> Option.map snd |> Option.value ~default:empty_tree
      in
      let updated_child = add_entry child_tree rest entry in
      let other_children =
        tree.children
        |> List.filter (fun (candidate, _child) -> candidate <> segment)
      in
      { tree with children = other_children @ [ (segment, updated_child) ] }

let build_tree (route_entries : Routes.route_entry list) =
  route_entries
  |> List.sort (fun (left : Routes.route_entry) (right : Routes.route_entry) ->
      String.compare left.Routes.route right.Routes.route)
  |> List.fold_left
       (fun tree entry -> add_entry tree (route_segments entry) entry)
       empty_tree

let module_name_of_segment = function
  | Static segment ->
      Names.sanitize_module_component (String.lowercase_ascii segment)
  | Param (name, Single) -> Names.sanitize_module_component ("param_" ^ name)
  | Param (name, Catch_all) ->
      Names.sanitize_module_component ("catch_all_" ^ name)
  | Param (name, Optional_catch_all) ->
      Names.sanitize_module_component ("optional_catch_all_" ^ name)

let module_path_of_entry (entry : Routes.route_entry) =
  route_segments entry |> List.map module_name_of_segment |> String.concat "."

let constructor_name_of_entry (entry : Routes.route_entry) =
  Names.route_constructor_name_of_source entry.Routes.source_file

let params_module_ref (entry : Routes.route_entry) =
  let path = module_path_of_entry entry in
  if path = "" then "Route_params" else path ^ ".Route_params"

let route_value_available (entry : Routes.route_entry) =
  (not entry.Routes.route_schema_has_params)
  && entry.Routes.params
     |> List.for_all (fun (_name, kind) -> kind = Optional_catch_all)

let query_module_ref (entry : Routes.route_entry) =
  let path = module_path_of_entry entry in
  if path = "" then "Route_query" else path ^ ".Route_query"

let hash_module_ref (entry : Routes.route_entry) =
  let path = module_path_of_entry entry in
  if path = "" then "Route_hash" else path ^ ".Route_hash"

let expr_of_page_kind = function
  | Code_page -> evar "Utopia_types.Code_page"
  | Markdown_page -> evar "Utopia_types.Markdown_page"

let expr_of_param_kind = function
  | Single -> evar "Utopia_types.Single"
  | Catch_all -> evar "Utopia_types.Catch_all"
  | Optional_catch_all -> evar "Utopia_types.Optional_catch_all"

let expr_of_param (name, kind) = tuple [ string name; expr_of_param_kind kind ]
let expr_of_params params = list (List.map expr_of_param params)
let expr_of_string_list values = list (List.map string values)

let expr_of_option render = function
  | None -> none
  | Some value -> some (render value)

let rec expr_of_frontmatter_value = function
  | Utopia_markdown.Null -> evar "Utopia_markdown.Null"
  | Utopia_markdown.Bool value ->
      construct "Utopia_markdown.Bool" (Some (bool value))
  | Utopia_markdown.Number value ->
      construct "Utopia_markdown.Number" (Some (float value))
  | Utopia_markdown.String value ->
      construct "Utopia_markdown.String" (Some (string value))
  | Utopia_markdown.List values ->
      construct "Utopia_markdown.List"
        (Some (list (List.map expr_of_frontmatter_value values)))
  | Utopia_markdown.Object values ->
      construct "Utopia_markdown.Object"
        (Some
           (list
              (List.map
                 (fun (key, value) ->
                   tuple [ string key; expr_of_frontmatter_value value ])
                 values)))

let expr_of_frontmatter_object values =
  call "Utopia_markdown.frontmatter_object_of_list"
    [
      ( Nolabel,
        list
          (List.map
             (fun (key, value) ->
               tuple [ string key; expr_of_frontmatter_value value ])
             values) );
    ]

let page_meta_expr (entry : Routes.route_entry) =
  record
    [
      ("route", string entry.route);
      ("matcher", string entry.matcher);
      ("conflict_key", string entry.conflict_key);
      ("params", expr_of_params entry.params);
      ("layouts", expr_of_string_list entry.layouts);
      ("kind", expr_of_page_kind entry.kind);
      ("source_file", string entry.source_file);
      ( "module_name",
        string (Names.compiled_page_module_name_of_source entry.source_file) );
      ("has_metadata", bool entry.has_metadata);
      ("static", bool entry.static);
      ("has_paths", bool entry.has_paths);
    ]

let api_meta_expr (entry : Routes.api_route_entry) =
  record
    [
      ("route", string entry.route);
      ("matcher", string entry.matcher);
      ("conflict_key", string entry.conflict_key);
      ("params", expr_of_params entry.params);
      ("middlewares", expr_of_string_list entry.middlewares);
      ("source_file", string entry.source_file);
      ("module_name", string entry.module_name);
    ]

let markdown_meta_expr (entry : Routes.route_entry) =
  record
    [
      ("route", string entry.route);
      ("matcher", string entry.matcher);
      ("source_file", string entry.source_file);
      ("body", string (Option.value entry.markdown_body ~default:""));
      ( "frontmatter",
        expr_of_option expr_of_frontmatter_object entry.markdown_frontmatter );
      ("title", expr_of_option string entry.markdown_title);
      ("description", expr_of_option string entry.markdown_description);
    ]

let api_param_entries api_entries =
  let seen = Hashtbl.create 16 in
  api_entries
  |> List.iter (fun (entry : Routes.api_route_entry) ->
      entry.params
      |> List.iter (fun (name, kind) ->
          if not (Hashtbl.mem seen name) then Hashtbl.add seen name kind));
  seen |> Hashtbl.to_seq |> List.of_seq
  |> List.sort (fun (left, _) (right, _) -> String.compare left right)

let api_param_accessor_item (name, kind) =
  let runtime_name =
    match kind with
    | Single -> "Utopia_server.api_param_single_exn"
    | Catch_all -> "Utopia_server.api_param_many_exn"
    | Optional_catch_all -> "Utopia_server.api_param_optional_many"
  in
  let_function name
    [ typed_pat "request" "Dream.request" ]
    (call runtime_name [ (Nolabel, evar "request"); (Nolabel, string name) ])

let api_module_item api_entries =
  let sorted_entries =
    api_entries
    |> List.sort (fun (left : Routes.api_route_entry) right ->
        String.compare left.route right.route)
  in
  let get_all =
    let_function
      ~result_type:(list_type (core_type "Utopia_types.api_route_meta"))
      "get_all" [ unit_pat ]
      (list (List.map api_meta_expr sorted_entries))
  in
  let params_module =
    module_ "Params"
      (api_param_entries sorted_entries |> List.map api_param_accessor_item)
  in
  module_ ~attrs:[ native_platform_attr ] "Api" [ get_all; params_module ]

let page_metadata_loader_item route_entries =
  let sorted_entries =
    route_entries
    |> List.sort (fun (left : Routes.route_entry) right ->
        String.compare left.route right.route)
  in
  let_function
    ~result_type:(list_type (core_type "Utopia_types.page_route_meta"))
    "get_all" [ unit_pat ]
    (list (List.map page_meta_expr sorted_entries))

let markdown_metadata_loader_item route_entries =
  let markdown_entries =
    route_entries
    |> List.filter (fun (entry : Routes.route_entry) ->
        entry.kind = Markdown_page)
    |> List.sort (fun (left : Routes.route_entry) right ->
        String.compare left.route right.route)
  in
  let entry_type =
    type_record "entry"
      [
        ("route", core_type "string");
        ("matcher", core_type "string");
        ("source_file", core_type "string");
        ("body", core_type "string");
        ( "frontmatter",
          option_type (core_type "Utopia_markdown.frontmatter_object") );
        ("title", option_type (core_type "string"));
        ("description", option_type (core_type "string"));
      ]
  in
  let get_all =
    let_function
      ~result_type:(list_type (core_type "entry"))
      "get_all" [ unit_pat ]
      (list (List.map markdown_meta_expr markdown_entries))
  in
  module_ ~attrs:[ native_platform_attr ] "Markdown" [ entry_type; get_all ]

let page_render_expr (entry : Routes.route_entry) =
  match entry.kind with
  | Markdown_page -> None
  | Code_page ->
      let module_name =
        Names.compiled_page_module_name_of_source entry.source_file
      in
      Some
        (tuple
           [
             string entry.source_file;
             fun0
               (call "Utopia_server.wrap_raw_inner_html_element"
                  [
                    ( Nolabel,
                      call (module_name ^ ".make")
                        [
                          ( Nolabel,
                            call
                              (module_name ^ ".makeProps")
                              [ (Nolabel, unit) ] );
                        ] );
                  ]);
           ])

let page_metadata_expr (entry : Routes.route_entry) =
  if not entry.has_metadata then None
  else
    let module_name =
      Names.compiled_page_module_name_of_source entry.source_file
    in
    Some (tuple [ string entry.source_file; evar (module_name ^ ".metadata") ])

let page_paths_expr (entry : Routes.route_entry) =
  if not entry.has_paths then None
  else
    let module_name =
      Names.compiled_page_module_name_of_source entry.source_file
    in
    Some (tuple [ string entry.source_file; evar (module_name ^ ".paths") ])

let layout_info_expr source_file =
  let module_name = Names.compiled_page_module_name_of_source source_file in
  let path = Routes.layout_route_path source_file in
  tuple
    [
      string source_file;
      record
        [
          ("Utopia_route_builder.path", string path);
          ( "render",
            fun1 "children"
              (call "Utopia_server.wrap_raw_inner_html_element"
                 [
                   ( Nolabel,
                     call (module_name ^ ".make")
                       [
                         ( Nolabel,
                           call
                             (module_name ^ ".makeProps")
                             [
                               (Labelled "children", evar "children");
                               (Nolabel, unit);
                             ] );
                       ] );
                 ]) );
        ];
    ]

let api_handler_expr (entry : Routes.api_route_entry) =
  tuple [ string entry.source_file; evar (entry.module_name ^ ".handler") ]

let api_middleware_expr source_file =
  tuple
    [
      string source_file;
      evar (Names.compiled_api_module_name_of_source source_file ^ ".middleware");
    ]

let server_registry_items route_entries api_entries =
  let layout_infos =
    route_entries
    |> List.concat_map (fun (entry : Routes.route_entry) -> entry.layouts)
    |> List.sort_uniq String.compare
    |> List.map layout_info_expr
  in
  let api_middlewares =
    api_entries
    |> List.concat_map (fun (entry : Routes.api_route_entry) ->
        entry.middlewares)
    |> List.sort_uniq String.compare
    |> List.map api_middleware_expr
  in
  [
    let_value "page_renders"
      (list (route_entries |> List.filter_map page_render_expr));
    let_value "page_metadata"
      (list (route_entries |> List.filter_map page_metadata_expr));
    let_value "page_paths"
      (list (route_entries |> List.filter_map page_paths_expr));
    let_value "layout_infos" (list layout_infos);
    let_value "api_handlers" (list (List.map api_handler_expr api_entries));
    let_value "api_middlewares" (list api_middlewares);
  ]

let not_found_page_item ~not_found_file ~not_found_layouts =
  match not_found_file with
  | None -> let_value "not_found_page" none
  | Some source_file ->
      let module_name = Names.compiled_page_module_name_of_source source_file in
      let render =
        fun0
          (call "Utopia_server.wrap_raw_inner_html_element"
             [
               ( Nolabel,
                 call (module_name ^ ".make")
                   [
                     ( Nolabel,
                       call (module_name ^ ".makeProps") [ (Nolabel, unit) ] );
                   ] );
             ])
      in
      let_value "not_found_page"
        (some
           (call "Utopia_server.Generated_not_found_registry.make"
              [
                (Labelled "layouts", expr_of_string_list not_found_layouts);
                (Labelled "render", render);
                (Nolabel, unit);
              ]))

let generate_shim route_entries api_entries =
  structure_to_string
    [
      include_module "Routes_client";
      page_metadata_loader_item route_entries;
      markdown_metadata_loader_item route_entries;
      api_module_item api_entries;
    ]

let generate_server route_entries api_entries ~not_found_file ~not_found_layouts
    =
  structure_to_string
    ([ include_module "Routes" ]
    @ server_registry_items route_entries api_entries
    @ [ not_found_page_item ~not_found_file ~not_found_layouts ])

let list_concat_expr values =
  match values with
  | [] -> list []
  | head :: tail -> List.fold_left (infix "@") head tail

let lower_path_pat_of_segment = function
  | Static segment -> pstring (String.lowercase_ascii segment)
  | Param (_, Single) -> pwild
  | Param (_, Catch_all) -> pwild
  | Param (_, Optional_catch_all) -> pwild

let original_exact_pat_of_segment = function
  | Static _ -> pwild
  | Param (name, Single) -> pvar name
  | Param (_, Catch_all) -> pwild
  | Param (_, Optional_catch_all) -> pwild

let prefix_pair_patterns prefix =
  ( List.map lower_path_pat_of_segment prefix,
    List.map original_exact_pat_of_segment prefix )

let many_params_expr values =
  call "Utopia_route.Params.many" [ (Nolabel, values) ]

let raw_params_of_exact_segments_exprs segments =
  segments
  |> List.filter_map (function
    | Param (name, Single) ->
        Some
          (tuple
             [
               string name;
               call "Utopia_route.Params.one" [ (Nolabel, evar name) ];
             ])
    | _ -> None)

let current_field_exprs_of_exact_segments_ast segments =
  segments
  |> List.filter_map (function
    | Param (name, Single) -> Some (name, evar name)
    | _ -> None)

let current_constructor_fields (entry : Routes.route_entry) =
  let path_fields =
    if entry.Routes.route_schema_has_params then
      [ ("params", core_type (params_module_ref entry ^ ".t")) ]
    else
      entry.Routes.params
      |> List.map (fun (name, kind) ->
          ( name,
            match kind with
            | Single -> core_type "string"
            | Catch_all ->
                core_type_apply "Utopia_route.Nonempty.t" [ core_type "string" ]
            | Optional_catch_all -> option_type (list_type (core_type "string"))
          ))
  in
  path_fields
  @ (if entry.Routes.route_schema_has_query then
       [ ("query", option_type (core_type (query_module_ref entry ^ ".t"))) ]
     else [])
  @
  if entry.Routes.route_schema_has_hash then
    [ ("hash", option_type (core_type (hash_module_ref entry ^ ".t"))) ]
  else []

let current_success_expr (entry : Routes.route_entry) field_exprs =
  let ctor = constructor_name_of_entry entry in
  let fields =
    (if entry.Routes.route_schema_has_params then [ ("params", evar "params") ]
     else field_exprs)
    @ (if entry.Routes.route_schema_has_query then [ ("query", evar "query") ]
       else [])
    @
    if entry.Routes.route_schema_has_hash then [ ("hash", evar "hash") ] else []
  in
  some
    (if fields = [] then construct ctor None
     else construct ctor (Some (record fields)))

let option_map_some var_name decoded_expr =
  call "Option.map"
    [ (Nolabel, fun1 var_name (some (evar var_name))); (Nolabel, decoded_expr) ]

let decode_path_params_if_needed (entry : Routes.route_entry) raw_params success
    =
  if not entry.Routes.route_schema_has_params then success
  else
    let_in (pvar "params")
      (call
         (params_module_ref entry ^ ".decode")
         [ (Nolabel, list raw_params) ])
      (match_ (evar "params")
         [
           case (pconstruct "Some" (Some ([], pvar "params"))) success;
           case pwild none;
         ])

let decode_query_if_needed (entry : Routes.route_entry) success =
  if not entry.Routes.route_schema_has_query then success
  else
    let_in (pvar "query_entries")
      (call "Utopia_route.query_entries" [ (Nolabel, evar "route") ])
      (let_in (pvar "query")
         (if_
            (infix "=" (evar "query_entries") (list []))
            (some none)
            (option_map_some "value"
               (call
                  (query_module_ref entry ^ ".decode")
                  [ (Nolabel, evar "query_entries") ])))
         (match_ (evar "query")
            [
              case (pconstruct "Some" (Some ([], pvar "query"))) success;
              case pwild none;
            ]))

let decode_hash_if_needed (entry : Routes.route_entry) success =
  if not entry.Routes.route_schema_has_hash then success
  else
    let_in (pvar "hash_value")
      (call "Utopia_route.hash" [ (Nolabel, evar "route") ])
      (let_in (pvar "hash")
         (match_ (evar "hash_value")
            [
              case (pconstruct "None" None) (some none);
              case
                (pconstruct "Some" (Some ([], pvar "value")))
                (option_map_some "decoded"
                   (call
                      (hash_module_ref entry ^ ".decode")
                      [ (Nolabel, evar "value") ]));
            ])
         (match_ (evar "hash")
            [
              case (pconstruct "Some" (Some ([], pvar "hash"))) success;
              case pwild none;
            ]))

let decode_current_expr (entry : Routes.route_entry) ~field_exprs ~raw_params =
  let success = current_success_expr entry field_exprs in
  success
  |> decode_path_params_if_needed entry raw_params
  |> decode_query_if_needed entry
  |> decode_hash_if_needed entry

let exact_case (entry : Routes.route_entry) =
  let segments = route_segments entry in
  let lower_pat = list_pat (List.map lower_path_pat_of_segment segments) in
  let original_pat =
    list_pat (List.map original_exact_pat_of_segment segments)
  in
  case
    (ptuple [ lower_pat; original_pat ])
    (decode_current_expr entry
       ~field_exprs:(current_field_exprs_of_exact_segments_ast segments)
       ~raw_params:(raw_params_of_exact_segments_exprs segments))

let catch_all_case (entry : Routes.route_entry) name prefix =
  let lower_prefix, original_prefix = prefix_pair_patterns prefix in
  let lower_pat = list_pat ~tail:pwild (lower_prefix @ [ pwild ]) in
  let original_pat =
    list_pat
      ~tail:(pvar (name ^ "_tail"))
      (original_prefix @ [ pvar (name ^ "_head") ])
  in
  case
    (ptuple [ lower_pat; original_pat ])
    (decode_current_expr entry
       ~field_exprs:
         (current_field_exprs_of_exact_segments_ast prefix
         @ [
             ( name,
               call "Utopia_route.Nonempty.make"
                 [
                   (Labelled "head", evar (name ^ "_head"));
                   (Labelled "tail", evar (name ^ "_tail"));
                   (Nolabel, unit);
                 ] );
           ])
       ~raw_params:
         (raw_params_of_exact_segments_exprs prefix
         @ [
             tuple
               [
                 string name;
                 many_params_expr
                   (infix "::" (evar (name ^ "_head")) (evar (name ^ "_tail")));
               ];
           ]))

let optional_catch_all_cases (entry : Routes.route_entry) name prefix =
  let lower_prefix, original_prefix = prefix_pair_patterns prefix in
  let absent_case =
    case
      (ptuple [ list_pat lower_prefix; list_pat original_prefix ])
      (decode_current_expr entry
         ~field_exprs:
           (current_field_exprs_of_exact_segments_ast prefix @ [ (name, none) ])
         ~raw_params:
           (raw_params_of_exact_segments_exprs prefix
           @ [ tuple [ string name; many_params_expr (list []) ] ]))
  in
  let present_case =
    case
      (ptuple
         [
           list_pat ~tail:pwild (lower_prefix @ [ pwild ]);
           list_pat
             ~tail:(pvar (name ^ "_tail"))
             (original_prefix @ [ pvar (name ^ "_head") ]);
         ])
      (decode_current_expr entry
         ~field_exprs:
           (current_field_exprs_of_exact_segments_ast prefix
           @ [
               ( name,
                 some
                   (infix "::" (evar (name ^ "_head")) (evar (name ^ "_tail")))
               );
             ])
         ~raw_params:
           (raw_params_of_exact_segments_exprs prefix
           @ [
               tuple
                 [
                   string name;
                   many_params_expr
                     (infix "::"
                        (evar (name ^ "_head"))
                        (evar (name ^ "_tail")));
                 ];
             ]))
  in
  [ absent_case; present_case ]

let current_cases (entry : Routes.route_entry) =
  match List.rev (route_segments entry) with
  | Param (name, Catch_all) :: prefix_rev ->
      [ catch_all_case entry name (List.rev prefix_rev) ]
  | Param (name, Optional_catch_all) :: prefix_rev ->
      optional_catch_all_cases entry name (List.rev prefix_rev)
  | _ -> [ exact_case entry ]

let make_params (entry : Routes.route_entry) =
  let path_params =
    if entry.Routes.route_schema_has_params then
      if entry.Routes.params = [] then []
      else [ labelled_param "params" (pvar "params") ]
    else
      entry.Routes.params
      |> List.map (fun (name, kind) ->
          match kind with
          | Single | Catch_all -> labelled_param name (pvar name)
          | Optional_catch_all -> optional_param name (pvar name))
  in
  let query_params =
    if entry.Routes.route_schema_has_query then
      [ optional_param "query" (pvar "query") ]
    else []
  in
  let hash_params =
    if entry.Routes.route_schema_has_hash then
      [ optional_param "hash" (pvar "hash") ]
    else []
  in
  path_params @ query_params @ hash_params @ [ value_param unit_pat ]

let client_segments_expr (entry : Routes.route_entry) =
  let pieces =
    route_segments entry
    |> List.map (function
      | Static segment -> list [ string (String.lowercase_ascii segment) ]
      | Param (name, kind) when entry.Routes.route_schema_has_params ->
          call "Utopia_route.Params.segments_exn"
            [
              (Labelled "route", string (Routes.pp_route entry.Routes.route));
              (Labelled "name", string name);
              (Labelled "kind", expr_of_param_kind kind);
              (Nolabel, evar "encoded_params");
            ]
      | Param (name, Single) -> list [ evar name ]
      | Param (name, Catch_all) ->
          call "Utopia_route.Nonempty.to_list" [ (Nolabel, evar name) ]
      | Param (name, Optional_catch_all) ->
          match_ (evar name)
            [
              case (pconstruct "None" None) (list []);
              case (pconstruct "Some" (Some ([], pvar "value"))) (evar "value");
            ])
  in
  list_concat_expr pieces

let make_body (entry : Routes.route_entry) =
  let final_call =
    call "Utopia_route.from_segments"
      ([ (Labelled "segments", client_segments_expr entry) ]
      @ (if entry.Routes.route_schema_has_query then
           [ (Labelled "query", evar "query") ]
         else [])
      @ (if entry.Routes.route_schema_has_hash then
           [ (Optional "hash", evar "hash") ]
         else [])
      @ [ (Nolabel, unit) ])
  in
  let body =
    if entry.Routes.route_schema_has_hash then
      let_in (pvar "hash")
        (match_ (evar "hash")
           [
             case (pconstruct "None" None) none;
             case
               (pconstruct "Some" (Some ([], pvar "value")))
               (some (call "Route_hash.encode" [ (Nolabel, evar "value") ]));
           ])
        final_call
    else final_call
  in
  let body =
    if entry.Routes.route_schema_has_query then
      let_in (pvar "query")
        (match_ (evar "query")
           [
             case (pconstruct "None" None) (list []);
             case
               (pconstruct "Some" (Some ([], pvar "value")))
               (call "Route_query.encode" [ (Nolabel, evar "value") ]);
           ])
        body
    else body
  in
  if entry.Routes.route_schema_has_params then
    let_in (pvar "encoded_params")
      (call "Route_params.encode" [ (Nolabel, evar "params") ])
      body
  else body

let client_route_entry_items (entry : Routes.route_entry) =
  let aliases =
    [
      ( entry.Routes.route_schema_has_params,
        "Route_params",
        Option.map
          (fun module_name -> module_name ^ ".Params")
          entry.Routes.route_schema_module );
      ( entry.Routes.route_schema_has_query,
        "Route_query",
        Option.map
          (fun module_name -> module_name ^ ".Query")
          entry.Routes.route_schema_module );
      ( entry.Routes.route_schema_has_hash,
        "Route_hash",
        Option.map
          (fun module_name -> module_name ^ ".Hash")
          entry.Routes.route_schema_module );
    ]
    |> List.filter_map (fun (enabled, alias_name, target) ->
        match (enabled, target) with
        | true, Some target -> Some (module_alias alias_name target)
        | _ -> None)
  in
  let make_item =
    let_function_params "make" (make_params entry) (make_body entry)
  in
  let route_items =
    if route_value_available entry then
      [ let_value "route" (call "make" [ (Nolabel, unit) ]) ]
    else []
  in
  aliases @ [ make_item ] @ route_items

let rec client_tree_structure tree =
  let route_items =
    match tree.route_entry with
    | None -> []
    | Some entry -> client_route_entry_items entry
  in
  let child_items =
    tree.children
    |> List.sort (fun (left, _) (right, _) ->
        String.compare
          (module_name_of_segment left)
          (module_name_of_segment right))
    |> List.map (fun (segment, child) ->
        module_ (module_name_of_segment segment) (client_tree_structure child))
  in
  route_items @ child_items

let current_type_item (route_entries : Routes.route_entry list) =
  let constructors =
    match route_entries with
    | [] -> [ ("No_match", []) ]
    | _ ->
        route_entries
        |> List.map (fun entry ->
            (constructor_name_of_entry entry, current_constructor_fields entry))
  in
  type_variant "t" constructors

let current_module_item (route_entries : Routes.route_entry list) =
  let cases =
    route_entries
    |> List.sort compare_route_specificity
    |> List.concat_map current_cases
  in
  let body =
    let_in (pvar "path_segments")
      (call "Utopia_route.path_segments" [ (Nolabel, evar "route") ])
      (let_in
         (pvar "lowercase_segments")
         (call "List.map"
            [
              (Nolabel, evar "String.lowercase_ascii");
              (Nolabel, evar "path_segments");
            ])
         (match_
            (tuple [ evar "lowercase_segments"; evar "path_segments" ])
            (cases @ [ case pwild none ])))
  in
  [
    current_type_item route_entries;
    let_function "of_route" [ pvar "route" ] body;
  ]

let generate_client route_entries =
  structure_to_string
    (client_tree_structure (build_tree route_entries)
    @ current_module_item route_entries)
