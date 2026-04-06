let string_of_params params =
  params
  |> List.map (fun (name, kind) ->
      Printf.sprintf "%s:%s" name (Utopia_types.string_of_param_kind kind))
  |> String.concat ","

let generate entries =
  entries
  |> List.sort (fun left right ->
      String.compare left.Routes.route right.Routes.route)
  |> List.map
       (fun
         {
           Routes.route;
           matcher;
           params;
           layouts;
           kind;
           source_file;
           has_metadata;
           static;
           _;
         }
       ->
         Printf.sprintf "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s" route
           (Utopia_types.string_of_kind kind)
           source_file matcher (string_of_params params)
           (String.concat ";" layouts)
           (string_of_bool has_metadata)
           (string_of_bool static))
  |> String.concat "\n"
