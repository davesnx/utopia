open Utopia_types

let schema_directory = "routes"

type entry = {
  relative_file : string;
  source_file : string;
  extension : string;
  module_name : string;
  route_key : string;
  has_params : bool;
  has_query : bool;
  has_hash : bool;
}

let contains_substring text needle =
  let text_length = String.length text in
  let needle_length = String.length needle in
  let rec loop index =
    if index + needle_length > text_length then false
    else if String.sub text index needle_length = needle then true
    else loop (index + 1)
  in
  if needle_length = 0 then true else loop 0

let has_named_module source module_name =
  contains_substring source ("module " ^ module_name)

let slice text start stop = String.sub text start (stop - start)

let module_block source module_name =
  let marker = "module " ^ module_name in
  let marker_length = String.length marker in
  let source_length = String.length source in
  let rec find index =
    if index + marker_length > source_length then None
    else if String.sub source index marker_length = marker then Some index
    else find (index + 1)
  in
  match find 0 with
  | None -> None
  | Some start ->
      let rec find_next_module index =
        if index + 8 > source_length then source_length
        else if String.sub source index 8 = "\nmodule " then index + 1
        else find_next_module (index + 1)
      in
      Some (slice source start (find_next_module (start + marker_length)))

let module_has_function source module_name function_name =
  module_block source module_name
  |> Option.map (fun block -> contains_substring block ("let " ^ function_name))
  |> Option.value ~default:false

let module_name_of_relative_file relative_file =
  "Route_schema__" ^ Names.generated_module_base relative_file

let route_key_of_relative_file relative_file =
  let without_extension = Filename.remove_extension relative_file in
  let parsed_segments_result =
    without_extension |> Routes.normalize_path_segments
    |> List.map Routes.parse_param_segment
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
        (Printf.sprintf "In %s/%s: %s" schema_directory relative_file message)
  | Ok segments -> (
      match Routes.validate_segment_rules segments with
      | Error message ->
          Error
            (Printf.sprintf "In %s/%s: %s" schema_directory relative_file
               message)
      | Ok _params -> Ok (Routes.route_of_segments segments))

let load route_entries =
  if not (Filesystem.directory_exists schema_directory) then ([], [])
  else
    let files =
      match Filesystem.read_files_recursive schema_directory with
      | Error (`Page_directory_doesnt_exist _path) -> []
      | Ok files -> files
    in
    let valid_route_keys =
      route_entries
      |> List.map (fun entry -> entry.Routes.route)
      |> List.sort_uniq String.compare
    in
    let parsed_entries, parse_errors =
      files
      |> List.fold_left
           (fun (entries, errors) relative_file ->
             match kind_of_extension (Filename.extension relative_file) with
             | Some Code_page -> (
                 match route_key_of_relative_file relative_file with
                 | Error message -> (entries, message :: errors)
                 | Ok route_key ->
                     let source_file =
                       Filename.concat schema_directory relative_file
                     in
                     let source =
                       In_channel.with_open_bin source_file (fun channel ->
                           In_channel.input_all channel)
                     in
                     let has_params = has_named_module source "Params" in
                     let has_query = has_named_module source "Query" in
                     let has_hash = has_named_module source "Hash" in
                     let errors =
                       if
                         has_params
                         && not (module_has_function source "Params" "encode")
                       then
                         Printf.sprintf
                           "Route schema %s defines module Params but is \
                            missing `let encode = ...`"
                           source_file
                         :: errors
                       else errors
                     in
                     let errors =
                       if
                         has_params
                         && not (module_has_function source "Params" "decode")
                       then
                         Printf.sprintf
                           "Route schema %s defines module Params but is \
                            missing `let decode = ...`"
                           source_file
                         :: errors
                       else errors
                     in
                     let errors =
                       if
                         has_query
                         && not (module_has_function source "Query" "encode")
                       then
                         Printf.sprintf
                           "Route schema %s defines module Query but is \
                            missing `let encode = ...`"
                           source_file
                         :: errors
                       else errors
                     in
                     let errors =
                       if
                         has_query
                         && not (module_has_function source "Query" "decode")
                       then
                         Printf.sprintf
                           "Route schema %s defines module Query but is \
                            missing `let decode = ...`"
                           source_file
                         :: errors
                       else errors
                     in
                     let errors =
                       if
                         has_hash
                         && not (module_has_function source "Hash" "encode")
                       then
                         Printf.sprintf
                           "Route schema %s defines module Hash but is missing \
                            `let encode = ...`"
                           source_file
                         :: errors
                       else errors
                     in
                     let errors =
                       if
                         has_hash
                         && not (module_has_function source "Hash" "decode")
                       then
                         Printf.sprintf
                           "Route schema %s defines module Hash but is missing \
                            `let decode = ...`"
                           source_file
                         :: errors
                       else errors
                     in
                     let errors =
                       if
                         has_params
                         && not
                              (List.exists
                                 (fun entry ->
                                   entry.Routes.route = route_key
                                   && entry.Routes.params <> [])
                                 route_entries)
                       then
                         Printf.sprintf
                           "Route schema %s defines module Params but the \
                            route has no dynamic path params"
                           source_file
                         :: errors
                       else errors
                     in
                     ( {
                         relative_file;
                         source_file;
                         extension = Filename.extension relative_file;
                         module_name =
                           module_name_of_relative_file relative_file;
                         route_key;
                         has_params;
                         has_query;
                         has_hash;
                       }
                       :: entries,
                       errors ))
             | _ -> (entries, errors))
           ([], [])
    in
    let grouped = Hashtbl.create 16 in
    parsed_entries
    |> List.iter (fun entry ->
        let current =
          Hashtbl.find_opt grouped entry.route_key |> Option.value ~default:[]
        in
        Hashtbl.replace grouped entry.route_key (entry :: current));
    let duplicate_errors =
      Hashtbl.fold
        (fun route_key entries errors ->
          if List.length entries <= 1 then errors
          else
            let ordered_sources =
              entries
              |> List.map (fun entry -> entry.source_file)
              |> List.sort String.compare
            in
            let route_label = Routes.pp_route route_key in
            Printf.sprintf
              "Route schema conflict for %s: both %s define a schema"
              route_label
              (String.concat ", " ordered_sources)
            :: errors)
        grouped []
    in
    let entries =
      grouped |> Hashtbl.to_seq_values |> List.of_seq
      |> List.filter_map (function [ entry ] -> Some entry | _ -> None)
    in
    let orphan_errors, entries =
      entries
      |> List.partition (fun entry ->
          not (List.mem entry.route_key valid_route_keys))
    in
    let orphan_errors =
      orphan_errors
      |> List.map (fun entry ->
          Printf.sprintf
            "Route schema %s does not match any collected page route"
            entry.source_file)
    in
    (entries, List.rev parse_errors @ List.rev duplicate_errors @ orphan_errors)

let attach route_entries schema_entries =
  let schema_by_route = Hashtbl.create 16 in
  schema_entries
  |> List.iter (fun entry ->
      Hashtbl.replace schema_by_route entry.route_key entry);
  route_entries
  |> List.map (fun entry ->
      match Hashtbl.find_opt schema_by_route entry.Routes.route with
      | None -> entry
      | Some schema ->
          {
            entry with
            Routes.route_schema_source = Some schema.source_file;
            route_schema_has_params = schema.has_params;
            route_schema_module = Some schema.module_name;
            route_schema_has_query = schema.has_query;
            route_schema_has_hash = schema.has_hash;
          })
