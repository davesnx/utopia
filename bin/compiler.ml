(*

let write_to_file path content =
  Eio.Path.with_open_out ~create:(`Exclusive 0o600) path @@ fun flow ->
  Eio.Flow.copy_string content flow

let rec empty_folder path =
  match Eio.Path.rmtree ~missing_ok:true path with
  | () -> Eio.Path.mkdir ~perm:0o777 path
  | exception Eio.Io _ -> ()

(* match Sys.file_exists path with
   | false -> (
       try Unix.mkdir path 0o777
       with Unix.Unix_error (err, _, _) ->
         Printf.eprintf "Error creating directory '%s': %s\n" path
           (Unix.error_message err))
   | true -> (
       match Sys.is_directory path with
       | true ->
           Sys.readdir path
           |> Array.iter (fun name -> empty_folder (Filename.concat path name))
       | false -> Sys.remove path) *)

let load_pages fname =
  let fname = Dynlink.adapt_filename fname in
  if Sys.file_exists fname then
    try Dynlink.loadfile fname with
    | Dynlink.Error err as e ->
        print_endline
        @@ Printf.sprintf "ERROR loading page: %s\n%s" fname
             (Dynlink.error_message err);
        raise e
    | _ -> failwith "Unknow error while loading plugin"
  else failwith "Plugin file does not exist"

(* There must be a way to point to a module type like
   type layout = Utopia.Loader_page.layout *)
type layout =
  ?key:string ->
  title:string ->
  scripts:React.element list ->
  children:React.element ->
  unit ->
  React.element

let render_html_page ~title ~(layout : layout) children =
  let component : React.element =
    layout ~key:"html" ~title ~scripts:[] ~children ()
  in
  let output = ReactDOM.renderToStaticMarkup component in
  Printf.sprintf "<!DOCTYPE html>%s" output

let split_at n lst =
  let rec aux n lst acc =
    if n <= 0 then (List.rev acc, lst)
    else
      match lst with
      | [] -> (List.rev acc, [])
      | head :: tail -> aux (n - 1) tail (head :: acc)
  in
  aux n lst []

let split_list_into_max_size_lists lst max_size =
  let rec aux lst acc =
    match lst with
    | [] -> List.rev acc
    | _ ->
        let chunk, rest = split_at max_size lst in
        aux rest (chunk :: acc)
  in
  aux lst []

let bootstrap () : unit =
  let ( / ) = Eio.Path.( / ) in
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.Src.set_level Cohttp_eio.src (Some Debug);

  Utopia.page ~path:"index" (fun () ->
      (div ~children:[ React.string "Static page" ] () [@JSX]));

  Utopia.register ~path:"home"
    ~loader:(fun () -> "home")
    (fun data -> (div ~children:[ React.string ("Hello " ^ data) ] () [@JSX]));

  Utopia.register ~path:"users"
    ~loader:(fun () -> ())
    (fun _ -> (div ~children:[ React.string "This page is slow!" ] () [@JSX]));

  Array.make 5_000 "mock_page"
  |> Array.iteri (fun index fixture ->
         Utopia.register
           ~path:(fixture ^ Int.to_string index)
           ~loader:(fun () -> fixture)
           (fun data ->
             (div
                ~children:
                  [
                    React.string data;
                    (h1 ~children:[ React.int index ] () [@JSX]);
                  ]
                () [@JSX])));

  let pages = Utopia.get_pages () in
  Eio_main.run @@ fun env ->
  let cwd = Eio.Stdenv.fs env in
  let utopia_artifacts_folder = cwd / "_utopia" in
  empty_folder utopia_artifacts_folder;

  Eio.Switch.run @@ fun sw ->
  (* let clock = Eio.Stdenv.clock env in *)
  Eio.traceln "Number of pages: %d" (Seq.length pages);

  let generate_page (module Page : Utopia.Loader_page) =
    let file = utopia_artifacts_folder / (Page.path ^ ".html") in
    Eio.traceln "Rendering page: %s" Page.path;
    let data = Page.loader () in
    let content =
      render_html_page ~layout:Page.layout ~title:Page.path (Page.make data)
    in
    write_to_file file content
  in

  let treshold = 1024 in

  (* let fibers = pages |> Seq.map (fun p () -> generate_page p) in *)
  let list_of_pages = List.of_seq pages in
  let fibers = split_list_into_max_size_lists list_of_pages treshold in
  (* let fibers = List.fold_left (fun acc p ->
         if List.length acc >= treshold then
           acc @
     ) [] list_of_pages in *)
  (* Eio.Fiber.all new_fibers *)
  List.iter (fun p -> Eio.Fiber.List.iter (fun p -> generate_page p) p) fibers
*)

let read_files path =
  match Sys.file_exists path with
  | false -> Error (`Page_directory_doesnt_exist path)
  | true ->
      let pages =
        Sys.readdir path
        |> Array.to_list
        |> List.filter (fun page ->
               let full_path = Filename.concat path page in
               not (Sys.is_directory full_path))
        |> Array.of_list
      in
      (* |> Array.iter (fun name -> empty_folder (Filename.concat path name)) *)
      Ok pages

let read_files_recursive path =
  let rec walk current_root current_relative acc =
    let current_path =
      if current_relative = "" then current_root
      else Filename.concat current_root current_relative
    in
    Sys.readdir current_path
    |> Array.to_list
    |> List.sort String.compare
    |> List.fold_left
         (fun acc entry ->
           let relative_entry =
             if current_relative = "" then entry
             else Filename.concat current_relative entry
           in
           let full_entry = Filename.concat current_root relative_entry in
           if Sys.is_directory full_entry then walk current_root relative_entry acc
           else relative_entry :: acc)
         acc
  in
  match Sys.file_exists path with
  | false -> Error (`Page_directory_doesnt_exist path)
  | true -> Ok (walk path "" [] |> List.rev)

let write_to_file file content =
  Out_channel.with_open_bin file (fun channel -> output_string channel content)

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

type page_script = {
  source_path : string;
  generated_module : string;
  generated_file : string;
  asset_path : string;
}

type route_entry = {
  route : string;
  matcher : string;
  conflict_key : string;
  params : (string * param_kind) list;
  layouts : string list;
  scripts : page_script list;
  kind : page_kind;
  source_file : string;
}

let kind_of_extension = function
  | ".ml" | ".mlx" | ".re" -> Some Code_page
  | ".md" -> Some Markdown_page
  | _ -> None

let is_under_lib_directory file =
  match String.split_on_char '/' file with
  | "lib" :: _ -> true
  | _ -> false

let is_identifier_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

let is_valid_identifier name =
  String.length name > 0
  && String.for_all is_identifier_char name
  &&
  match name.[0] with
  | '0' .. '9' -> false
  | _ -> true

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
      else Error (Printf.sprintf "Invalid optional catch-all parameter '%s'" name)
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
           && not (is_group_segment segment)
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
      |> List.filter_map (function Param (name, kind) -> Some (name, kind) | _ -> None)
    in
    let unique_names = names |> List.map fst |> List.sort_uniq String.compare in
    if List.length unique_names <> List.length names then
      Error "Route has duplicated parameter names"
    else Ok names

let route_entry_of_file file kind =
  let without_extension = Filename.remove_extension file in
  let source_file = Filename.concat "pages" file in
  let normalized_segments = normalize_path_segments without_extension in
  let parsed_segments_result =
    normalized_segments
    |> List.map parse_param_segment
    |> List.fold_left
         (fun acc result ->
           match (acc, result) with
           | Error _ as error, _ -> error
           | Ok _, Error message -> Error message
           | Ok segments, Ok segment -> Ok (segment :: segments))
         (Ok [])
    |> Result.map List.rev
  in
  match parsed_segments_result with
  | Error message ->
      Error
        (Printf.sprintf "In %s: %s (segment path: %s)" source_file message
           without_extension)
  | Ok segments -> (
      match validate_segment_rules segments with
      | Error message ->
          Error
            (Printf.sprintf "In %s: %s (segment path: %s)" source_file message
               without_extension)
      | Ok params ->
          Ok
            {
              route = route_of_segments segments;
              matcher = matcher_of_segments segments;
               conflict_key = conflict_key_of_segments segments;
               params;
               layouts = [];
               scripts = [];
               kind;
               source_file;
             })

let relative_directory file =
  let dir = Filename.dirname file in
  if dir = "." then "" else dir

let basename_without_extension file =
  file |> Filename.basename |> Filename.remove_extension

let is_layout_file file = basename_without_extension file = "layout"

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

let collect_layouts files =
  let layout_by_dir = Hashtbl.create 32 in
  let errors = ref [] in
  files
  |> List.iter (fun file ->
         let extension = Filename.extension file in
         match (kind_of_extension extension, is_layout_file file) with
         | Some Code_page, true ->
             let dir = relative_directory file in
             let source_file = Filename.concat "pages" file in
             let previous = Hashtbl.find_opt layout_by_dir dir in
             (match previous with
             | None -> Hashtbl.replace layout_by_dir dir source_file
             | Some existing ->
                 errors :=
                   Printf.sprintf
                     "Layout conflict in pages/%s: both %s and %s define a layout" dir
                     existing source_file
                   :: !errors)
         | _ -> ());
  (layout_by_dir, List.rev !errors)

let layouts_for_file layout_by_dir file =
  let dir = relative_directory file in
  ancestor_directories dir
  |> List.filter_map (fun segment -> Hashtbl.find_opt layout_by_dir segment)

let substring_at text index pattern =
  let pattern_len = String.length pattern in
  index + pattern_len <= String.length text
  && String.sub text index pattern_len = pattern

let find_substring text pattern =
  let rec loop index =
    if index + String.length pattern > String.length text then None
    else if substring_at text index pattern then Some index
    else loop (index + 1)
  in
  if String.length pattern = 0 then Some 0 else loop 0

let trim_trailing_delimiters value =
  let is_delimiter = function
    | ' ' | '\t' | '\r' | '\n' | ';' | ',' | '*' | '/' -> true
    | _ -> false
  in
  let rec last_non_delimiter index =
    if index < 0 then -1
    else if is_delimiter value.[index] then last_non_delimiter (index - 1)
    else index
  in
  let stop = last_non_delimiter (String.length value - 1) in
  if stop < 0 then "" else String.sub value 0 (stop + 1)

let parse_script_directive line =
  let marker = "@utopia.script" in
  match find_substring line marker with
  | None -> Ok None
  | Some index ->
      let start = index + String.length marker in
      let raw_tail =
        String.sub line start (String.length line - start) |> String.trim
      in
      let tail =
        if raw_tail <> "" && raw_tail.[0] = ':' then
          String.sub raw_tail 1 (String.length raw_tail - 1) |> String.trim
        else raw_tail
      in
      let token =
        tail |> String.split_on_char ' '
        |> List.filter (fun item -> item <> "")
        |> function
        | [] -> ""
        | head :: _ -> head
      in
      let declared = token |> trim_trailing_delimiters |> String.trim in
      if declared = "" then Error "Missing script path after @utopia.script"
      else Ok (Some declared)

let normalize_slashes value = String.map (fun c -> if c = '\\' then '/' else c) value

let has_path_traversal value =
  value
  |> normalize_slashes
  |> String.split_on_char '/'
  |> List.exists (fun segment -> segment = "..")

let normalize_relative_path value =
  value |> normalize_slashes |> String.split_on_char '/'
  |> List.filter (fun segment -> segment <> "" && segment <> ".")
  |> String.concat "/"

let script_module_name_from_source source_path =
  let without_extension = Filename.remove_extension source_path in
  let normalized = normalize_relative_path without_extension in
  let buffer = Buffer.create (String.length normalized + 16) in
  Buffer.add_string buffer "Script__";
  String.iter
    (function
      | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9') as c -> Buffer.add_char buffer c
      | _ -> Buffer.add_char buffer '_')
    normalized;
  Buffer.contents buffer

let resolve_script_source ~page_source_file declared_path =
  if declared_path = "" then Error "Script path cannot be empty"
  else if has_path_traversal declared_path then
    Error "Script path cannot contain '..' segments"
  else if Filename.is_relative declared_path then
    let page_dir = Filename.dirname page_source_file in
    Ok
      (Filename.concat page_dir declared_path
      |> normalize_relative_path)
  else Error "Script path must be relative to the page file"

let script_of_declared_path ~page_source_file declared_path =
  match resolve_script_source ~page_source_file declared_path with
  | Error message ->
      Error
        (Printf.sprintf "In %s: %s ('%s')" page_source_file message declared_path)
  | Ok source_path ->
      if not (Sys.file_exists source_path) then
        Error
          (Printf.sprintf "In %s: script file not found '%s'" page_source_file
             source_path)
      else if Sys.is_directory source_path then
        Error
          (Printf.sprintf "In %s: script path points to a directory '%s'"
             page_source_file source_path)
      else
        let extension = Filename.extension source_path in
        match kind_of_extension extension with
        | Some Code_page ->
            let generated_module = script_module_name_from_source source_path in
            Ok
              {
                source_path;
                generated_module;
                generated_file = generated_module ^ extension;
                asset_path = Printf.sprintf "target/%s.js" generated_module;
              }
        | Some Markdown_page | None ->
            Error
              (Printf.sprintf
                 "In %s: script '%s' must point to a .ml/.mlx/.re module"
                 page_source_file source_path)

let scripts_for_page_source page_source_file =
  let source =
    In_channel.with_open_bin page_source_file (fun channel -> In_channel.input_all channel)
  in
  let lines = String.split_on_char '\n' source in
  let seen_script_sources = Hashtbl.create 8 in
  let scripts, errors =
    lines
    |> List.mapi (fun index line -> (index + 1, line))
    |> List.fold_left
         (fun (scripts, errors) (line_number, line) ->
           match parse_script_directive line with
           | Error message -> (scripts, message :: errors)
           | Ok None -> (scripts, errors)
           | Ok (Some declared_path) -> (
               match script_of_declared_path ~page_source_file declared_path with
               | Ok script ->
                   if Hashtbl.mem seen_script_sources script.source_path then
                     let first_line = Hashtbl.find seen_script_sources script.source_path in
                     let error =
                       Printf.sprintf
                         "In %s: duplicate @utopia.script for '%s' at line %d (first seen at line %d)"
                         page_source_file script.source_path line_number first_line
                     in
                     (scripts, error :: errors)
                   else (
                     Hashtbl.replace seen_script_sources script.source_path line_number;
                     (script :: scripts, errors))
               | Error message -> (scripts, message :: errors)))
         ([], [])
  in
  (List.rev scripts, List.rev errors)

let declared_script_sources files =
  let sources = Hashtbl.create 32 in
  let collect_from_file file =
    let source_file = Filename.concat "pages" file in
    if Sys.file_exists source_file && not (Sys.is_directory source_file) then
      let source =
        In_channel.with_open_bin source_file (fun channel -> In_channel.input_all channel)
      in
      source
      |> String.split_on_char '\n'
      |> List.iter (fun line ->
             match parse_script_directive line with
             | Ok None -> ()
             | Ok (Some declared_path) -> (
                 match resolve_script_source ~page_source_file:source_file declared_path with
                  | Ok resolved_source -> Hashtbl.replace sources resolved_source ()
                 | Error _ -> ())
             | Error _ -> ())
  in
  files
  |> List.iter (fun file ->
         if not (is_under_lib_directory file) then
           match kind_of_extension (Filename.extension file) with
           | None -> ()
           | Some _ -> collect_from_file file);
  sources

let route_entries_of_files files =
  let declared_script_sources_set = declared_script_sources files in
  let layout_by_dir, layout_errors = collect_layouts files in
  files
  |> List.fold_left
       (fun (entries, errors) file ->
         let source_file = Filename.concat "pages" file in
         if is_under_lib_directory file then (entries, errors)
         else if Hashtbl.mem declared_script_sources_set source_file then
           (entries, errors)
         else
            let extension = Filename.extension file in
            match kind_of_extension extension with
           | None -> (entries, errors)
           | Some Code_page when is_layout_file file -> (entries, errors)
           | Some kind -> (
               match route_entry_of_file file kind with
                | Ok entry ->
                    let scripts, script_errors =
                      scripts_for_page_source entry.source_file
                    in
                    let entry =
                      {
                        entry with
                        layouts = layouts_for_file layout_by_dir file;
                        scripts;
                      }
                    in
                    (entry :: entries, script_errors @ errors)
                | Error message -> (entries, message :: errors)))
       ([], layout_errors)
  |> fun (entries, errors) -> (List.rev entries, List.rev errors)

let string_of_kind = function
  | Code_page -> "code"
  | Markdown_page -> "markdown"

let string_of_param_kind = function
  | Single -> "single"
  | Catch_all -> "catch_all"
  | Optional_catch_all -> "optional_catch_all"

let string_of_params params =
  params
  |> List.map (fun (name, kind) ->
         Printf.sprintf "%s:%s" name (string_of_param_kind kind))
  |> String.concat ","

let generate_route_manifest entries =
  entries
  |> List.sort (fun left right -> String.compare left.route right.route)
  |> List.map (fun { route; matcher; params; layouts; kind; source_file; _ } ->
         Printf.sprintf "%s\t%s\t%s\t%s\t%s\t%s"
           route
           (string_of_kind kind)
           source_file
           matcher
           (string_of_params params)
           (String.concat ";" layouts))
  |> String.concat "\n"

let find_route_conflicts entries =
  let grouped = Hashtbl.create 32 in
  List.iter
    (fun entry ->
      let current =
        match Hashtbl.find_opt grouped entry.conflict_key with
        | Some entries -> entries
        | None -> []
      in
      Hashtbl.replace grouped entry.conflict_key (entry :: current))
    entries;
  Hashtbl.fold
    (fun _key grouped_entries acc ->
      if List.length grouped_entries > 1 then
        let reversed = List.rev grouped_entries in
        ((List.hd reversed).route, reversed) :: acc
      else acc)
    grouped []

let pp_route route = if route = "" then "/" else "/" ^ route

let source_basename source = Filename.basename source

let preferred_source_for_route route entries =
  let normalized_route = if route = "" then "index" else route in
  let preferred_order =
    [
      Printf.sprintf "pages/%s/index.ml" normalized_route;
      Printf.sprintf "pages/%s/index.re" normalized_route;
      Printf.sprintf "pages/%s.ml" normalized_route;
      Printf.sprintf "pages/%s.re" normalized_route;
      Printf.sprintf "pages/%s.md" normalized_route;
    ]
  in
  let sources = List.map (fun entry -> entry.source_file) entries in
  match List.find_opt (fun candidate -> List.mem candidate sources) preferred_order with
  | Some source -> source
  | None -> List.hd sources

let report_route_conflicts conflicts =
  Printf.eprintf "\n  Route conflicts detected:\n";
  conflicts
  |> List.sort (fun (left, _) (right, _) -> String.compare left right)
  |> List.iter (fun (route, grouped_entries) ->
         let entries = grouped_entries in
         let preferred_source = preferred_source_for_route route entries in
         let ordered_sources =
           entries
           |> List.map (fun entry -> entry.source_file)
           |> List.sort String.compare
         in
          Printf.eprintf "\n    - %s has %d competing page files:\n" (pp_route route)
            (List.length ordered_sources);
          ordered_sources
          |> List.iter (fun source -> Printf.eprintf "        * %s\n" source);
          let alternatives =
            ordered_sources
            |> List.filter (fun source -> source <> preferred_source)
          in
          Printf.eprintf "      Suggested canonical file: %s\n" preferred_source;
          Printf.eprintf "      Rename/remove the others: %s\n"
            (String.concat ", " alternatives);
         let duplicate_names =
           ordered_sources
           |> List.map source_basename
           |> List.sort_uniq String.compare
         in
         if List.length duplicate_names = 1 then
           Printf.eprintf
             "      Note: these files differ only by directory/casing; choose one canonical path.\n");
  Printf.eprintf
    "\n  Rule: exactly one source file must map to each route.\n\n";
  Printf.eprintf
    "  Recommended convention:\n    * folder route: pages/<route>/index.ml\n    * leaf route: pages/<parent>/<name>.ml\n"

let report_route_parse_errors errors =
  Printf.eprintf "\n  Invalid page declarations:\n";
  errors |> List.iter (fun error -> Printf.eprintf "    - %s\n" error);
  Printf.eprintf
    "\n  Supported Next.js-style segments:\n    * [id]\n    * [...slug]\n    * [[...slug]]\n    * route groups: (marketing)\n    * parallel slots: @slot (ignored for URL path)\n"
;
  Printf.eprintf
    "  Script directive format:\n    * @utopia.script ./relative/path/to/module.re\n"

let find_script_module_collisions entries =
  let grouped = Hashtbl.create 32 in
  entries
  |> List.iter (fun entry ->
         entry.scripts
         |> List.iter (fun script ->
                let current =
                  match Hashtbl.find_opt grouped script.generated_module with
                  | Some items -> items
                  | None -> []
                in
                Hashtbl.replace grouped script.generated_module
                  ((entry.route, script.source_path) :: current)));
  Hashtbl.fold
    (fun generated_module refs acc ->
      let unique_sources =
        refs
        |> List.map snd
        |> List.sort_uniq String.compare
      in
      if List.length unique_sources <= 1 then acc
      else
        let ref_text =
          refs
          |> List.sort_uniq (fun (left_route, left_source) (right_route, right_source) ->
                 match String.compare left_source right_source with
                 | 0 -> String.compare left_route right_route
                 | value -> value)
          |> List.map (fun (route, source) ->
                 Printf.sprintf "%s (declared by /%s)" source route)
          |> String.concat ", "
        in
        let message =
          Printf.sprintf
            "Script module collision for %s. Conflicting sources: %s"
            generated_module ref_text
        in
        message :: acc)
    grouped []

let starts_with_at text index prefix =
  let prefix_len = String.length prefix in
  index + prefix_len <= String.length text
  && String.sub text index prefix_len = prefix

let extract_params_accesses source =
  let rec read_ident i =
    if i < String.length source && is_identifier_char source.[i] then
      read_ident (i + 1)
    else i
  in
  let rec loop i acc =
    if i >= String.length source - 6 then List.rev acc
    else if starts_with_at source i "params." then
      let start = i + 7 in
      let stop = read_ident start in
      if stop > start then
        let name = String.sub source start (stop - start) in
        loop stop (name :: acc)
      else loop (i + 1) acc
    else loop (i + 1) acc
  in
  loop 0 [] |> List.sort_uniq String.compare

let unknown_params_for_entry entry =
  match entry.kind with
  | Markdown_page -> []
  | Code_page ->
      let source =
        In_channel.with_open_bin entry.source_file (fun channel ->
            In_channel.input_all channel)
      in
      let used = extract_params_accesses source in
      let declared = entry.params |> List.map fst in
      used |> List.filter (fun name -> not (List.mem name declared))

let report_unknown_param_accesses entries =
  let issues =
    entries
    |> List.filter_map (fun entry ->
           let unknown = unknown_params_for_entry entry in
           if unknown = [] then None else Some (entry, unknown))
  in
  if issues = [] then false
  else (
    Printf.eprintf "\n  Unknown route parameter access detected:\n";
    issues
    |> List.iter (fun (entry, unknown) ->
           let declared =
             entry.params
             |> List.map fst
             |> function
             | [] -> "(none)"
             | values -> String.concat ", " values
           in
           Printf.eprintf
             "    - %s references unknown params [%s]; allowed params for %s are [%s]\n"
             entry.source_file
             (String.concat ", " unknown)
             (pp_route entry.route)
             declared);
    Printf.eprintf
      "\n  Fix: rename the param access or update the filename segment to declare it.\n";
    true)

let unique_scripts_of_entries entries =
  entries
  |> List.concat_map (fun entry -> entry.scripts)
  |> List.sort_uniq (fun left right ->
         String.compare left.generated_module right.generated_module)

(* TODO: Probably we want to use sexp expresions or any abstraction on top *)
let generate_dune_rules files route_entries =
  let escape_for_dune_string value =
    let buffer = Buffer.create (String.length value) in
    String.iter
      (function
        | '\\' -> Buffer.add_string buffer "\\\\"
        | '"' -> Buffer.add_string buffer "\\\""
        | '\n' -> Buffer.add_string buffer "\\n"
        | char -> Buffer.add_char buffer char)
      value;
    Buffer.contents buffer
  in
  let shared_lib_dir = "pages/lib" in
  let shared_lib_files =
    if Sys.file_exists shared_lib_dir && Sys.is_directory shared_lib_dir then
      Sys.readdir shared_lib_dir
      |> Array.to_list
      |> List.sort String.compare
      |> List.filter_map (fun file ->
             let full_path = Filename.concat shared_lib_dir file in
             if Sys.is_directory full_path then None
             else
               let extension = Filename.extension file in
               match kind_of_extension extension with
               | Some Code_page -> Some (file, Filename.remove_extension file, extension)
               | _ -> None)
    else []
  in
  let has_shared_lib =
    Sys.file_exists shared_lib_dir && Sys.is_directory shared_lib_dir
  in
  let pages =
    List.map
      (fun page -> (Filename.remove_extension page, Filename.extension page))
      files
  in
  let ml_pages =
    pages |> List.filter (fun (_, extension) -> kind_of_extension extension = Some Code_page)
  in
  let md_pages =
    pages |> List.filter (fun (_, extension) -> extension = ".md")
  in
  let script_entries = unique_scripts_of_entries route_entries in
  let custom_rules =
    ml_pages
    |> List.map (fun (file, extension) ->
           Printf.sprintf
             "(rule\n\
             \ (deps ../pages/%s%s)\n\
             \ (targets %s_melange%s %s_native%s)\n\
             \ (action\n\
             \  (progn\n\
             \   (run cp %%{deps} %s_melange%s)\n\
             \   (run cp %%{deps} %s_native%s))))\n\n"
             file extension file extension file extension file extension file
             extension)
    |> String.concat ""
  in
  let script_copy_rules =
    script_entries
    |> List.map (fun script ->
           Printf.sprintf
             "(rule\n\
             \ (deps ../%s)\n\
             \ (target %s)\n\
             \ (action\n\
             \  (run cp %%{deps} %s)))\n\n"
             script.source_path script.generated_file script.generated_file)
    |> String.concat ""
  in
  let shared_lib_copy_rules =
    if not has_shared_lib then ""
    else
      shared_lib_files
      |> List.map (fun (file, base, extension) ->
             Printf.sprintf
               "(rule\n\
               \ (deps ../pages/lib/%s)\n\
               \ (targets Lib__%s_melange%s Lib__%s_native%s)\n\
               \ (action\n\
               \  (progn\n\
               \   (run cp %%{deps} Lib__%s_melange%s)\n\
               \   (run cp %%{deps} Lib__%s_native%s))))\n\n"
               file base extension base extension base extension base extension)
      |> String.concat ""
  in
  let shared_lib_namespace_rules =
    if not has_shared_lib then ""
    else
      let melange_aliases =
        shared_lib_files
        |> List.map (fun (_file, base, _extension) ->
               let module_name = String.capitalize_ascii base in
               Printf.sprintf "module %s = Lib__%s_melange" module_name base)
        |> String.concat "\n"
      in
      let native_aliases =
        shared_lib_files
        |> List.map (fun (_file, base, _extension) ->
               let module_name = String.capitalize_ascii base in
               Printf.sprintf "module %s = Lib__%s_native" module_name base)
        |> String.concat "\n"
      in
      Printf.sprintf
        "(rule\n (target Lib_melange.re)\n (action\n  (write-file %%{target} \"%s\")))\n\n(rule\n (target Lib_native.re)\n (action\n  (write-file %%{target} \"%s\")))\n\n"
        (escape_for_dune_string melange_aliases)
        (escape_for_dune_string native_aliases)
  in
  let markdown_rules =
    md_pages
    |> List.map (fun (file, extension) ->
           Printf.sprintf
             {|(rule
 (deps ../pages/%s%s)
 (target %s.html)
 (action
  (with-stdout-to %%{target}
  (with-stdin-from %%{deps}
   (run %%{bin:utopia.markdown})))))

|}
             file extension file)
    |> String.concat ""
  in
  let melange_rule =
    let modules =
      let page_modules =
        ml_pages
        |> List.map (fun (page, _extension) -> Printf.sprintf "%s_melange" page)
      in
      let script_modules =
        script_entries |> List.map (fun script -> script.generated_module)
      in
      if has_shared_lib then
        let lib_modules =
          shared_lib_files
          |> List.map (fun (_file, base, _extension) ->
                 Printf.sprintf "Lib__%s_melange" base)
        in
        String.concat " " ("Lib_melange" :: (lib_modules @ page_modules @ script_modules))
      else String.concat " " (page_modules @ script_modules)
    in
    let open_flag =
      if has_shared_lib then "\n (flags (:standard -open Lib_melange))" else ""
    in
    Printf.sprintf
      "(melange.emit\n\
      \ (target %s)\n\
      \ (modules %s)\n\
      \ (libraries reason-react)%s\n\
      \ (preprocess\n\
      \  (pps reason-react-ppx)))\n\n"
      "target" modules open_flag
  in
  let library_rules =
    let modules =
      let page_modules =
        ml_pages
        |> List.map (fun (page, _extension) -> Printf.sprintf "%s_native" page)
      in
      if has_shared_lib then
        let lib_modules =
          shared_lib_files
          |> List.map (fun (_file, base, _extension) ->
                 Printf.sprintf "Lib__%s_native" base)
        in
        String.concat " " ("Lib_native" :: (lib_modules @ page_modules))
      else String.concat " " page_modules
    in
    let open_flag =
      if has_shared_lib then "\n (flags (:standard -open Lib_native))" else ""
    in

    Printf.sprintf
      "(library\n\
      \ (name pages)\n\
      \ (modules %s)\n\
      \ (public_name utopia)\n\
      \ (libraries server-reason-react.react server-reason-react.reactDom)%s\n\
      \ (preprocess\n\
      \  (pps server-reason-react.ppx)))\n\n"
      modules open_flag
  in
  Printf.sprintf "%s%s%s%s%s%s%s" custom_rules script_copy_rules shared_lib_copy_rules
    shared_lib_namespace_rules melange_rule markdown_rules
    library_rules

let generate_scripts_manifest entries =
  entries
  |> List.sort (fun left right -> String.compare left.route right.route)
  |> List.map (fun entry ->
         let assets =
           entry.scripts
           |> List.map (fun script -> script.asset_path)
           |> String.concat ";"
         in
         Printf.sprintf "%s\t%s" entry.route assets)
  |> String.concat "\n"

let () =
  let utopia_dune_file = "_utopia/dune" in
  let utopia_routes_file = "_utopia/routes.manifest" in
  let utopia_scripts_file = "_utopia/scripts.manifest" in
  print_endline "\n\nUtopia compiler";
  if not (Sys.file_exists "_utopia") then Sys.mkdir "_utopia" 0o755;
  if Sys.file_exists utopia_dune_file then Sys.remove utopia_dune_file;
  if Sys.file_exists utopia_routes_file then Sys.remove utopia_routes_file;
  if Sys.file_exists utopia_scripts_file then Sys.remove utopia_scripts_file;
  match read_files "pages" with
  | Error (`Page_directory_doesnt_exist path) ->
      Printf.eprintf "  Error reading the '%s' directory\n" path
  | Ok pages ->
      Printf.printf "  Pages: %s\n" (String.concat ", " (Array.to_list pages));
      let recursive_pages =
        match read_files_recursive "pages" with
        | Error (`Page_directory_doesnt_exist _path) -> []
        | Ok files -> files
      in
      let route_entries, route_parse_errors = route_entries_of_files recursive_pages in
      let script_module_collisions = find_script_module_collisions route_entries in
      let page_declaration_errors =
        route_parse_errors @ script_module_collisions
      in
      let conflicts = find_route_conflicts route_entries in
      let has_unknown_param_accesses = report_unknown_param_accesses route_entries in
      let has_errors =
        page_declaration_errors <> [] || conflicts <> [] || has_unknown_param_accesses
      in
      if has_errors then (
        if page_declaration_errors <> [] then
          report_route_parse_errors page_declaration_errors;
        if conflicts <> [] then report_route_conflicts conflicts;
        exit 1)
      else (
        print_endline "\n  Generating rules\n";
        let dune_rules = generate_dune_rules (Array.to_list pages) route_entries in
        let route_manifest = generate_route_manifest route_entries in
        let scripts_manifest = generate_scripts_manifest route_entries in
        print_endline dune_rules;
        print_endline "\n  Generating route manifest\n";
        print_endline route_manifest;
        print_endline "\n  Generating scripts manifest\n";
        print_endline scripts_manifest;
        write_to_file utopia_dune_file dune_rules;
        write_to_file utopia_routes_file (route_manifest ^ "\n");
        write_to_file utopia_scripts_file (scripts_manifest ^ "\n"))
