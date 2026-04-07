type frontmatter_value =
  | Null
  | Bool of bool
  | Number of float
  | String of string
  | List of frontmatter_value list
  | Object of (string * frontmatter_value) list

type frontmatter_object = (string * frontmatter_value) list

type parse_error =
  | Parse_error of { message : string; markdown : string }
  | Io_error of { message : string }

type extraction = {
  frontmatter : frontmatter_object option;
  markdown_body : string;
  title : string option;
  description : string option;
  warning : string option;
}

let starts_with_frontmatter markdown =
  String.length markdown >= 4 && String.sub markdown 0 4 = "---\n"

let substring_from markdown start =
  String.sub markdown start (String.length markdown - start)

let find_substring_from ~text ~pattern ~start =
  let pattern_len = String.length pattern in
  let text_len = String.length text in
  let rec loop index =
    if index + pattern_len > text_len then None
    else if String.sub text index pattern_len = pattern then Some index
    else loop (index + 1)
  in
  loop start

let take_frontmatter_block markdown =
  if not (starts_with_frontmatter markdown) then None
  else
    match find_substring_from ~text:markdown ~pattern:"\n---\n" ~start:3 with
    | None -> None
    | Some closing_start ->
        let yaml_start = 4 in
        let yaml_len = max 0 (closing_start - yaml_start) in
        let yaml = String.sub markdown yaml_start yaml_len in
        let body = substring_from markdown (closing_start + 5) in
        Some (yaml, body)

let dedupe_last_wins entries =
  entries
  |> List.fold_left
       (fun acc (key, value) ->
         let acc = List.remove_assoc key acc in
         acc @ [ (key, value) ])
       []

let rec of_yaml_value (value : Yaml.value) : frontmatter_value =
  match value with
  | `Null -> Null
  | `Bool value -> Bool value
  | `Float value -> Number value
  | `String value -> String value
  | `A values -> List (List.map of_yaml_value values)
  | `O values ->
      values
      |> List.map (fun (key, value) -> (key, of_yaml_value value))
      |> dedupe_last_wins
      |> fun values -> Object values

let of_yaml_object (value : Yaml.value) : frontmatter_object option =
  match of_yaml_value value with Object values -> Some values | _ -> None

let find_string key (frontmatter : frontmatter_object) =
  match List.assoc_opt key frontmatter with
  | Some (String value) -> Some value
  | _ -> None

let warning_for_parse_error ~source_file = function
  | Parse_error { message; _ } ->
      Printf.sprintf
        "markdown frontmatter warning (%s): invalid YAML frontmatter (%s); \
         falling back to full markdown body"
        source_file message
  | Io_error { message } ->
      Printf.sprintf
        "markdown frontmatter warning (%s): could not read markdown (%s); \
         falling back to empty markdown body"
        source_file message

let extract ?(source_file = "<markdown>") markdown =
  match take_frontmatter_block markdown with
  | None ->
      {
        frontmatter = None;
        markdown_body = markdown;
        title = None;
        description = None;
        warning = None;
      }
  | Some (yaml_source, stripped_body) -> (
      if String.trim yaml_source = "" then
        {
          frontmatter = Some [];
          markdown_body = stripped_body;
          title = None;
          description = None;
          warning = None;
        }
      else
        match Yaml.of_string yaml_source with
        | Error (`Msg message) ->
            {
              frontmatter = None;
              markdown_body = markdown;
              title = None;
              description = None;
              warning =
                Some
                  (warning_for_parse_error ~source_file
                     (Parse_error { message; markdown = yaml_source }));
            }
        | Ok value -> (
            match of_yaml_object value with
            | Some frontmatter ->
                {
                  frontmatter = Some frontmatter;
                  markdown_body = stripped_body;
                  title = find_string "title" frontmatter;
                  description = find_string "description" frontmatter;
                  warning = None;
                }
            | None ->
                {
                  frontmatter = None;
                  markdown_body = markdown;
                  title = None;
                  description = None;
                  warning =
                    Some
                      (Printf.sprintf
                         "markdown frontmatter warning (%s): YAML frontmatter \
                          root must be an object/map; falling back to full \
                          markdown body"
                         source_file);
                }))
