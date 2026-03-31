type read_error = [ `Page_directory_doesnt_exist of string ]

let file_exists path = Sys.file_exists path && not (Sys.is_directory path)
let directory_exists path = Sys.file_exists path && Sys.is_directory path

let read_files path =
  match Sys.file_exists path with
  | false -> Error (`Page_directory_doesnt_exist path)
  | true ->
      let files =
        Sys.readdir path |> Array.to_list
        |> List.filter (fun entry ->
            let full_path = Filename.concat path entry in
            not (Sys.is_directory full_path))
        |> Array.of_list
      in
      Ok files

let read_files_recursive path =
  let rec walk current_root current_relative acc =
    let current_path =
      if current_relative = "" then current_root
      else Filename.concat current_root current_relative
    in
    Sys.readdir current_path |> Array.to_list |> List.sort String.compare
    |> List.fold_left
         (fun acc entry ->
           let relative_entry =
             if current_relative = "" then entry
             else Filename.concat current_relative entry
           in
           let full_entry = Filename.concat current_root relative_entry in
           if Sys.is_directory full_entry then
             walk current_root relative_entry acc
           else relative_entry :: acc)
         acc
  in
  match Sys.file_exists path with
  | false -> Error (`Page_directory_doesnt_exist path)
  | true -> Ok (walk path "" [] |> List.rev)

let write_to_file file content =
  Out_channel.with_open_bin file (fun channel -> output_string channel content)

let ensure_directory path =
  if Sys.file_exists path then (
    if not (Sys.is_directory path) then
      failwith (Printf.sprintf "Expected directory at %s" path))
  else Sys.mkdir path 0o755

let copy_file source_file target_file =
  let contents =
    In_channel.with_open_bin source_file (fun channel ->
        In_channel.input_all channel)
  in
  write_to_file target_file contents

let remove_file_if_exists path = if Sys.file_exists path then Sys.remove path
