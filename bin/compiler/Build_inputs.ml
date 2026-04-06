open Utopia_types

type compiled_code_file = {
  relative_file : string;
  extension : string;
  base_name : string;
}

type shared_lib_file = {
  source_file : string;
  extension : string;
  module_name : string;
}

type route_schema_file = {
  source_file : string;
  extension : string;
  module_name : string;
}

let shared_lib_directory = Fpath.to_string Utopia_path.shared_lib_directory_name
let has_shared_lib () = Filesystem.directory_exists shared_lib_directory

let shared_lib_files_for_build () : shared_lib_file list =
  if has_shared_lib () then
    Sys.readdir shared_lib_directory
    |> Array.to_list |> List.sort String.compare
    |> List.filter_map (fun file ->
        let full_path = Filename.concat shared_lib_directory file in
        if Sys.is_directory full_path then None
        else
          let extension = Filename.extension file in
          match kind_of_extension extension with
          | Some Code_page ->
              Some
                ({
                   source_file = file;
                   extension;
                   module_name =
                     file |> Filename.remove_extension
                     |> Names.sanitize_module_component;
                 }
                  : shared_lib_file)
          | _ -> None)
  else []

let shared_lib_module_name (file : shared_lib_file) = "Lib__" ^ file.module_name

let shared_lib_target (file : shared_lib_file) =
  Printf.sprintf "%s%s" (shared_lib_module_name file) file.extension

let route_schema_target (file : route_schema_file) =
  Printf.sprintf "%s%s" file.module_name file.extension

let route_schema_files_for_build route_entries : route_schema_file list =
  let seen = Hashtbl.create 16 in
  route_entries
  |> List.filter_map (fun entry ->
      match
        (entry.Routes.route_schema_source, entry.Routes.route_schema_module)
      with
      | Some source_file, Some module_name ->
          if Hashtbl.mem seen source_file then None
          else (
            Hashtbl.replace seen source_file ();
            Some
              ({
                 source_file;
                 extension = Filename.extension source_file;
                 module_name;
               }
                : route_schema_file))
      | _ -> None)
  |> List.sort (fun left right ->
      String.compare left.source_file right.source_file)

let code_files_for_build files =
  files
  |> List.filter_map (fun file ->
      match kind_of_extension (Filename.extension file) with
      | Some Code_page ->
          Some
            {
              relative_file = file;
              extension = Filename.extension file;
              base_name = Names.compiled_page_module_name file;
            }
      | _ -> None)

let markdown_files_for_build files =
  files
  |> List.filter_map (fun file ->
      if Filename.extension file = ".md" then
        Some
          {
            relative_file = file;
            extension = ".md";
            base_name = Names.generated_module_base file;
          }
      else None)
