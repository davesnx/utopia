let read_file path =
  In_channel.with_open_bin path (fun channel -> In_channel.input_all channel)

let route_count () =
  let routes_source =
    Utopia_path.generated_routes_source (Artifacts.project_paths ())
  in
  if not (Artifacts.artifact_exists routes_source) then 0
  else
    let content = read_file (Artifacts.artifact_path routes_source) in
    content |> String.split_on_char '\n'
    |> List.filter (fun line ->
        String.trim line |> fun line ->
        String.ends_with ~suffix:": Utopia_types.page_route_meta);" line)
    |> List.length
