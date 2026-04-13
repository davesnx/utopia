open Utopia_types

let app_directory = "app"
let app_api_directory = "app/api"
let pages_directory = "pages"
let api_directory = "api"

type route_entry = {
  route : string;
  matcher : string;
  conflict_key : string;
  params : (string * param_kind) list;
  layouts : string list;
  kind : page_kind;
  source_file : string;
  has_metadata : bool;
  static : bool;
  has_paths : bool;
  before_export_origin : Analysis.origin option;
  paths_export_origin : Analysis.origin option;
  markdown_frontmatter : Utopia_markdown.frontmatter_object option;
  markdown_body : string option;
  markdown_title : string option;
  markdown_description : string option;
  route_schema_source : string option;
  route_schema_has_params : bool;
  route_schema_module : string option;
  route_schema_has_query : bool;
  route_schema_has_hash : bool;
}

type route_signature = {
  route : string;
  matcher : string;
  conflict_key : string;
  params : (string * param_kind) list;
}

type api_route_entry = {
  route : string;
  matcher : string;
  conflict_key : string;
  params : (string * param_kind) list;
  middlewares : string list;
  source_file : string;
  module_name : string;
}

let is_identifier_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

let is_valid_identifier name =
  String.length name > 0
  && String.for_all is_identifier_char name
  && match name.[0] with '0' .. '9' -> false | _ -> true

let parse_param_segment segment =
  let len = String.length segment in
  if len < 3 || segment.[0] <> '[' || segment.[len - 1] <> ']' then
    Ok (Static segment)
  else
    let inner = String.sub segment 1 (len - 2) in
    let inner_len = String.length inner in
    if inner_len >= 5 && String.sub inner 0 4 = "[...]" then
      Error (Printf.sprintf "Invalid segment '%s'" segment)
    else if
      inner_len >= 6
      && inner.[0] = '['
      && inner.[inner_len - 1] = ']'
      && String.sub inner 1 3 = "..."
    then
      let name = String.sub inner 4 (inner_len - 5) in
      if is_valid_identifier name then Ok (Param (name, Optional_catch_all))
      else
        Error (Printf.sprintf "Invalid optional catch-all parameter '%s'" name)
    else if inner_len >= 4 && String.sub inner 0 3 = "..." then
      let name = String.sub inner 3 (inner_len - 3) in
      if is_valid_identifier name then Ok (Param (name, Catch_all))
      else Error (Printf.sprintf "Invalid catch-all parameter '%s'" name)
    else if is_valid_identifier inner then Ok (Param (inner, Single))
    else Error (Printf.sprintf "Invalid parameter '%s'" inner)

let is_group_segment segment =
  let len = String.length segment in
  len >= 2 && segment.[0] = '(' && segment.[len - 1] = ')'

let is_parallel_slot_segment segment =
  String.length segment > 0 && segment.[0] = '@'

let normalize_path_segments path_without_extension =
  let segments = String.split_on_char '/' path_without_extension in
  let visible_segments =
    segments
    |> List.filter (fun segment ->
        segment <> ""
        && (not (is_group_segment segment))
        && not (is_parallel_slot_segment segment))
  in
  match List.rev visible_segments with
  | "index" :: rest -> List.rev rest
  | _ -> visible_segments

let render_route_segment = function
  | Static segment -> String.lowercase_ascii segment
  | Param (name, Single) -> Printf.sprintf "[%s]" name
  | Param (name, Catch_all) -> Printf.sprintf "[...%s]" name
  | Param (name, Optional_catch_all) -> Printf.sprintf "[[...%s]]" name

let render_matcher_segment = function
  | Static segment -> String.lowercase_ascii segment
  | Param (name, Single) -> ":" ^ name
  | Param (name, Catch_all) -> "*" ^ name
  | Param (name, Optional_catch_all) -> "**" ^ name

let render_conflict_segment = function
  | Static segment -> String.lowercase_ascii segment
  | Param (_name, Single) -> ":"
  | Param (_name, Catch_all) -> "*"
  | Param (_name, Optional_catch_all) -> "**"

let route_of_segments segments =
  segments |> List.map render_route_segment |> String.concat "/"

let matcher_of_segments segments =
  segments |> List.map render_matcher_segment |> String.concat "/"

let conflict_key_of_segments segments =
  segments |> List.map render_conflict_segment |> String.concat "/"

let validate_segment_rules segments =
  let has_non_terminal_spread =
    segments
    |> List.mapi (fun index segment -> (index, segment))
    |> List.exists (fun (index, segment) ->
        let is_last = index = List.length segments - 1 in
        match segment with
        | Param (_, (Catch_all | Optional_catch_all)) -> not is_last
        | _ -> false)
  in
  if has_non_terminal_spread then
    Error "Catch-all and optional catch-all segments must be the last segment"
  else
    let names =
      segments
      |> List.filter_map (function
        | Param (name, kind) -> Some (name, kind)
        | _ -> None)
    in
    let unique_names = names |> List.map fst |> List.sort_uniq String.compare in
    if List.length unique_names <> List.length names then
      Error "Route has duplicated parameter names"
    else Ok names

let relative_directory file =
  let dir = Filename.dirname file in
  if dir = "." then "" else dir

let starts_with_directory_prefix path prefix =
  String.equal path prefix || String.starts_with ~prefix:(prefix ^ "/") path

let basename_without_extension file =
  file |> Filename.basename |> Filename.remove_extension

let is_layout_file file = basename_without_extension file = "layout"
let is_not_found_file file = basename_without_extension file = "not-found"

let ancestor_directories dir =
  if dir = "" then [ "" ]
  else
    let parts = String.split_on_char '/' dir in
    let rec build prefixes remaining =
      match remaining with
      | [] -> prefixes
      | segment :: rest ->
          let next =
            match List.rev prefixes with
            | [] | "" :: _ -> segment
            | prev :: _ -> Filename.concat prev segment
          in
          build (prefixes @ [ next ]) rest
    in
    build [ "" ] parts

let route_signature_of_path ~source_file path_without_extension =
  let normalized_segments = normalize_path_segments path_without_extension in
  let parsed_segments_result =
    normalized_segments
    |> List.map parse_param_segment
    |> List.fold_left
         (fun acc result ->
           match (acc, result) with
           | (Error _ as error), _ -> error
           | Ok _, Error message -> Error message
           | Ok segments, Ok segment -> Ok (segment :: segments))
         (Ok [])
    |> Result.map List.rev
  in
  match parsed_segments_result with
  | Error message ->
      Error
        (Printf.sprintf "In %s: %s (segment path: %s)" source_file message
           path_without_extension)
  | Ok segments -> (
      match validate_segment_rules segments with
      | Error message ->
          Error
            (Printf.sprintf "In %s: %s (segment path: %s)" source_file message
               path_without_extension)
      | Ok params ->
          Ok
            ({
               route = route_of_segments segments;
               matcher = matcher_of_segments segments;
               conflict_key = conflict_key_of_segments segments;
               params;
             }
              : route_signature))

let route_signature_of_file ~source_file file =
  route_signature_of_path ~source_file (Filename.remove_extension file)

let route_entry_of_file ~source_root ~route_path file kind =
  let source_file = Filename.concat source_root file in
  match route_signature_of_path ~source_file route_path with
  | Error _ as error -> error
  | Ok { route; matcher; conflict_key; params } ->
      Ok
        {
          route;
          matcher;
          conflict_key;
          params;
          layouts = [];
          kind;
          source_file;
          has_metadata = false;
          static = false;
          has_paths = false;
          before_export_origin = None;
          paths_export_origin = None;
          markdown_frontmatter = None;
          markdown_body = None;
          markdown_title = None;
          markdown_description = None;
          route_schema_source = None;
          route_schema_has_params = false;
          route_schema_module = None;
          route_schema_has_query = false;
          route_schema_has_hash = false;
        }

let collect_layouts ~source_root files =
  let layout_by_dir = Hashtbl.create 32 in
  let errors = ref [] in
  files
  |> List.iter (fun file ->
      let extension = Filename.extension file in
      match (kind_of_extension extension, is_layout_file file) with
      | Some Code_page, true -> (
          let dir = relative_directory file in
          let source_file = Filename.concat source_root file in
          let previous = Hashtbl.find_opt layout_by_dir dir in
          match previous with
          | None -> Hashtbl.replace layout_by_dir dir source_file
          | Some existing ->
              errors :=
                Printf.sprintf
                  "Layout conflict in %s/%s: both %s and %s define a layout"
                  source_root dir existing source_file
                :: !errors)
      | _ -> ());
  (layout_by_dir, List.rev !errors)

let layouts_for_file layout_by_dir file =
  let dir = relative_directory file in
  ancestor_directories dir
  |> List.filter_map (fun segment -> Hashtbl.find_opt layout_by_dir segment)

let route_entries_of_files_with ~source_root ~route_path_of_file files =
  let layout_by_dir, layout_errors = collect_layouts ~source_root files in
  files
  |> List.fold_left
       (fun (entries, errors) file ->
         let extension = Filename.extension file in
         match kind_of_extension extension with
         | None -> (entries, errors)
         | Some Code_page when is_layout_file file -> (entries, errors)
         | Some kind -> (
             match
               route_entry_of_file ~source_root
                 ~route_path:(route_path_of_file file) file kind
             with
             | Ok entry ->
                 let entry =
                   { entry with layouts = layouts_for_file layout_by_dir file }
                 in
                 (entry :: entries, errors)
             | Error message -> (entries, message :: errors)))
       ([], layout_errors)
  |> fun (entries, errors) -> (List.rev entries, List.rev errors)

type app_file_collection = {
  page_files : string list;
  api_files : string list;
  not_found_file : string option;
  errors : string list;
}

let app_page_file_conflicts page_files_by_dir =
  page_files_by_dir |> Hashtbl.to_seq |> List.of_seq
  |> List.filter_map (fun (dir, source_files) ->
      match source_files |> List.sort_uniq String.compare with
      | [] | [ _ ] -> None
      | ordered ->
          Some
            (Printf.sprintf "Duplicate page files in %s/%s: %s" app_directory
               dir
               (String.concat ", " ordered)))

let app_route_file_conflicts route_files_by_dir =
  route_files_by_dir |> Hashtbl.to_seq |> List.of_seq
  |> List.filter_map (fun (dir, source_files) ->
      match source_files |> List.sort_uniq String.compare with
      | [] | [ _ ] -> None
      | ordered ->
          Some
            (Printf.sprintf "Duplicate route files in %s/%s: %s" app_directory
               dir
               (String.concat ", " ordered)))

let collect_app_files files =
  let pages = ref [] in
  let api = ref [] in
  let errors = ref [] in
  let not_found = ref None in
  let page_files_by_dir = Hashtbl.create 16 in
  let route_files_by_dir = Hashtbl.create 16 in
  let add_page_file dir source_file relative_file =
    let existing =
      Hashtbl.find_opt page_files_by_dir dir |> Option.value ~default:[]
    in
    Hashtbl.replace page_files_by_dir dir (source_file :: existing);
    pages := relative_file :: !pages
  in
  let add_layout_file relative_file = pages := relative_file :: !pages in
  let add_route_file dir source_file relative_file =
    let existing =
      Hashtbl.find_opt route_files_by_dir dir |> Option.value ~default:[]
    in
    Hashtbl.replace route_files_by_dir dir (source_file :: existing);
    api := relative_file :: !api
  in
  files
  |> List.iter (fun relative_file ->
      let extension = Filename.extension relative_file in
      let basename = basename_without_extension relative_file in
      let dir = relative_directory relative_file in
      let source_file = Filename.concat app_directory relative_file in
      let in_api_namespace = starts_with_directory_prefix dir "api" in
      match (basename, kind_of_extension extension) with
      | "layout", Some Code_page -> add_layout_file relative_file
      | "page", Some Code_page | "page", Some Markdown_page ->
          if in_api_namespace then
            errors :=
              Printf.sprintf
                "Invalid app route declaration: %s is a page file inside %s/**"
                source_file app_api_directory
              :: !errors
          else add_page_file dir source_file relative_file
      | "not-found", Some Code_page -> (
          if in_api_namespace then ()
          else if dir <> "" then
            (* Nested not-found files are treated as app-local support modules
               for now; only root-level not-found is recognized *)
            pages := relative_file :: !pages
          else
            match !not_found with
            | None ->
                not_found := Some source_file;
                pages := relative_file :: !pages
            | Some existing ->
                errors :=
                  Printf.sprintf
                    "Duplicate not-found files in %s/: both %s and %s define \
                     not-found"
                    app_directory existing source_file
                  :: !errors)
      | "route", Some Code_page ->
          if in_api_namespace then add_route_file dir source_file relative_file
          else
            errors :=
              Printf.sprintf
                "Invalid app route declaration: %s is a route file outside \
                 %s/**"
                source_file app_api_directory
              :: !errors
      | "_middleware", Some Code_page when in_api_namespace ->
          api := relative_file :: !api
      | "_middleware", Some Code_page -> ()
      | _, Some Code_page ->
          if in_api_namespace then api := relative_file :: !api
          else pages := relative_file :: !pages
      | _ -> ());
  let api_files =
    !api
    |> List.map (fun relative_file ->
        let prefix = "api/" in
        let prefix_len = String.length prefix in
        if
          String.length relative_file >= prefix_len
          && String.sub relative_file 0 prefix_len = prefix
        then
          String.sub relative_file prefix_len
            (String.length relative_file - prefix_len)
        else relative_file)
  in
  {
    page_files = List.rev !pages;
    api_files = List.rev api_files;
    not_found_file = !not_found;
    errors =
      List.rev !errors
      @ app_page_file_conflicts page_files_by_dir
      @ app_route_file_conflicts route_files_by_dir;
  }

let app_route_entries_of_files files =
  let page_and_layout_files =
    files
    |> List.filter (fun file ->
        let basename = basename_without_extension file in
        match (basename, kind_of_extension (Filename.extension file)) with
        | "layout", Some Code_page -> true
        | "page", Some Code_page | "page", Some Markdown_page -> true
        | _ -> false)
  in
  route_entries_of_files_with ~source_root:app_directory
    ~route_path_of_file:(fun file -> relative_directory file)
    page_and_layout_files

let not_found_layouts_of_app_files files =
  let layout_by_dir, _errors =
    collect_layouts ~source_root:app_directory files
  in
  ancestor_directories ""
  |> List.filter_map (fun segment -> Hashtbl.find_opt layout_by_dir segment)

let attach_markdown_payloads (entries : route_entry list) =
  let attach_entry (entry : route_entry) =
    match entry.kind with
    | Code_page -> (entry, None)
    | Markdown_page -> (
        try
          let markdown =
            In_channel.with_open_bin entry.source_file (fun channel ->
                In_channel.input_all channel)
          in
          let extraction =
            Utopia_markdown.extract_frontmatter ~source_file:entry.source_file
              markdown
          in
          ( {
              entry with
              markdown_frontmatter = extraction.frontmatter;
              markdown_body = Some extraction.markdown_body;
              markdown_title = extraction.title;
              markdown_description = extraction.description;
            },
            extraction.warning )
        with Sys_error message ->
          ( {
              entry with
              markdown_frontmatter = None;
              markdown_body = Some "";
              markdown_title = None;
              markdown_description = None;
            },
            Some
              (Printf.sprintf
                 "markdown frontmatter warning (%s): could not read markdown \
                  (%s); falling back to empty markdown body"
                 entry.source_file message) ))
  in
  entries
  |> List.fold_left
       (fun (acc_entries, acc_warnings) entry ->
         let updated, warning = attach_entry entry in
         let acc_warnings =
           match warning with
           | None -> acc_warnings
           | Some warning -> warning :: acc_warnings
         in
         (updated :: acc_entries, acc_warnings))
       ([], [])
  |> fun (entries, warnings) -> (List.rev entries, List.rev warnings)

let is_api_middleware_file file =
  basename_without_extension file = "_middleware"

let collect_api_middlewares ~source_root files =
  let middleware_by_dir = Hashtbl.create 32 in
  let errors = ref [] in
  files
  |> List.iter (fun file ->
      let extension = Filename.extension file in
      match kind_of_extension extension with
      | Some Code_page when is_api_middleware_file file -> (
          let dir = relative_directory file in
          let source_file = Filename.concat source_root file in
          let previous = Hashtbl.find_opt middleware_by_dir dir in
          match previous with
          | None -> Hashtbl.replace middleware_by_dir dir source_file
          | Some existing ->
              errors :=
                Printf.sprintf
                  "Middleware conflict in %s/%s: both %s and %s define \
                   _middleware"
                  source_root dir existing source_file
                :: !errors)
      | _ -> ());
  (middleware_by_dir, List.rev !errors)

let middlewares_for_file middleware_by_dir file =
  let dir = relative_directory file in
  ancestor_directories dir
  |> List.filter_map (fun segment -> Hashtbl.find_opt middleware_by_dir segment)

let api_route_entry_of_file ~source_root ~route_path file =
  let source_file = Filename.concat source_root file in
  match route_signature_of_path ~source_file route_path with
  | Error _ as error -> error
  | Ok signature ->
      let route =
        if signature.route = "" then "api" else "api/" ^ signature.route
      in
      let matcher =
        if signature.matcher = "" then "api" else "api/" ^ signature.matcher
      in
      let conflict_key =
        if signature.conflict_key = "" then "api"
        else "api/" ^ signature.conflict_key
      in
      Ok
        {
          route;
          matcher;
          conflict_key;
          params = signature.params;
          middlewares = [];
          source_file;
          module_name = "Api__" ^ Names.generated_module_base file;
        }

let api_route_entries_of_files_with ~source_root ~route_path_of_file files =
  let middleware_by_dir, middleware_errors =
    collect_api_middlewares ~source_root files
  in
  files
  |> List.fold_left
       (fun (entries, errors) file ->
         let extension = Filename.extension file in
         match kind_of_extension extension with
         | None | Some Markdown_page -> (entries, errors)
         | Some Code_page when is_api_middleware_file file -> (entries, errors)
         | Some Code_page -> (
             match
               api_route_entry_of_file ~source_root
                 ~route_path:(route_path_of_file file) file
             with
             | Error message -> (entries, message :: errors)
             | Ok entry ->
                 let entry =
                   {
                     entry with
                     middlewares = middlewares_for_file middleware_by_dir file;
                   }
                 in
                 (entry :: entries, errors)))
       ([], middleware_errors)
  |> fun (entries, errors) -> (List.rev entries, List.rev errors)

let app_api_route_entries_of_files files =
  let route_and_middleware_files =
    files
    |> List.filter (fun file ->
        let basename = basename_without_extension file in
        match (basename, kind_of_extension (Filename.extension file)) with
        | "route", Some Code_page | "_middleware", Some Code_page -> true
        | _ -> false)
  in
  api_route_entries_of_files_with ~source_root:app_api_directory
    ~route_path_of_file:(fun file -> relative_directory file)
    route_and_middleware_files

let page_route_in_reserved_api_namespace (entry : route_entry) =
  String.equal entry.route "api"
  || String.starts_with ~prefix:"api/" entry.route

let reserved_api_namespace_errors (route_entries : route_entry list) =
  route_entries
  |> List.filter (fun (entry : route_entry) ->
      page_route_in_reserved_api_namespace entry)
  |> List.map (fun (entry : route_entry) ->
      let route_label = if entry.route = "" then "/" else "/" ^ entry.route in
      Printf.sprintf
        "Page route %s from %s conflicts with reserved /api namespace"
        route_label entry.source_file)

let api_param_kind_conflicts (api_entries : api_route_entry list) =
  let seen = Hashtbl.create 16 in
  let errors = ref [] in
  api_entries
  |> List.iter (fun entry ->
      entry.params
      |> List.iter (fun (name, kind) ->
          match Hashtbl.find_opt seen name with
          | None -> Hashtbl.replace seen name (kind, entry.source_file)
          | Some (existing_kind, _existing_source) when existing_kind = kind ->
              ()
          | Some (existing_kind, existing_source) ->
              errors :=
                Printf.sprintf
                  "API param %s uses multiple shapes (%s in %s vs %s in %s). \
                   Use a consistent param kind so Routes.Api.Params.%s has one \
                   type."
                  name
                  (string_of_param_kind existing_kind)
                  existing_source
                  (string_of_param_kind kind)
                  entry.source_file name
                :: !errors));
  List.rev !errors

let find_api_conflicts (entries : api_route_entry list) =
  let grouped = Hashtbl.create 32 in
  entries
  |> List.iter (fun entry ->
      let current =
        Hashtbl.find_opt grouped entry.conflict_key |> Option.value ~default:[]
      in
      Hashtbl.replace grouped entry.conflict_key (entry :: current));
  grouped |> Hashtbl.to_seq_values |> List.of_seq
  |> List.filter (fun grouped_entries -> List.length grouped_entries > 1)
  |> List.map List.rev

let pp_route route = if route = "" then "/" else "/" ^ route

let strip_pages_prefix source_file =
  let strip_with_prefix prefix source =
    let with_slash = prefix ^ "/" in
    let prefix_len = String.length with_slash in
    if
      String.length source >= prefix_len
      && String.sub source 0 prefix_len = with_slash
    then Some (String.sub source prefix_len (String.length source - prefix_len))
    else None
  in
  match strip_with_prefix app_directory source_file with
  | Some stripped -> stripped
  | None -> (
      match strip_with_prefix pages_directory source_file with
      | Some stripped -> stripped
      | None -> source_file)

let layout_route_path source_file =
  let relative = strip_pages_prefix source_file |> Filename.remove_extension in
  let dir = relative_directory relative in
  let segments =
    dir |> normalize_path_segments
    |> List.map (fun segment ->
        match parse_param_segment segment with
        | Ok parsed -> parsed
        | Error message -> invalid_arg message)
  in
  segments |> matcher_of_segments |> pp_route
