let read_file path =
  In_channel.with_open_bin path (fun channel -> In_channel.input_all channel)

let route_count () =
  let routes_manifest = Artifacts.routes_manifest_ref () in
  if not (Artifacts.artifact_exists routes_manifest) then 0
  else
    let content = read_file (Artifacts.artifact_path routes_manifest) in
    content |> String.split_on_char '\n'
    |> List.filter (fun line -> String.trim line <> "")
    |> List.length
