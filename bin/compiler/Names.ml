let app_directory = "app"
let app_api_directory = "app/api"

let sanitize_module_component value =
  let buffer = Buffer.create (String.length value) in
  String.iter
    (function
      | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9') as char ->
          Buffer.add_char buffer char
      | _ -> Buffer.add_char buffer '_')
    value;
  let raw = Buffer.contents buffer in
  let rec first_non_underscore index =
    if index >= String.length raw then String.length raw
    else if raw.[index] = '_' then first_non_underscore (index + 1)
    else index
  in
  let rec last_non_underscore index =
    if index < 0 then -1
    else if raw.[index] = '_' then last_non_underscore (index - 1)
    else index
  in
  let start = first_non_underscore 0 in
  let stop = last_non_underscore (String.length raw - 1) in
  let trimmed =
    if stop < start then "module" else String.sub raw start (stop - start + 1)
  in
  let prefixed =
    match trimmed.[0] with '0' .. '9' -> "m_" ^ trimmed | _ -> trimmed
  in
  String.capitalize_ascii prefixed

let sanitize_library_component value =
  let buffer = Buffer.create (String.length value) in
  String.iter
    (function
      | ('a' .. 'z' | '0' .. '9') as char -> Buffer.add_char buffer char
      | 'A' .. 'Z' as char -> Buffer.add_char buffer (Char.lowercase_ascii char)
      | _ -> Buffer.add_char buffer '_')
    value;
  let raw = Buffer.contents buffer in
  let rec first_non_underscore index =
    if index >= String.length raw then String.length raw
    else if raw.[index] = '_' then first_non_underscore (index + 1)
    else index
  in
  let rec last_non_underscore index =
    if index < 0 then -1
    else if raw.[index] = '_' then last_non_underscore (index - 1)
    else index
  in
  let start = first_non_underscore 0 in
  let stop = last_non_underscore (String.length raw - 1) in
  if stop < start then "root" else String.sub raw start (stop - start + 1)

let generated_module_base relative_file =
  relative_file |> Filename.remove_extension |> String.split_on_char '/'
  |> List.filter (fun segment -> segment <> "")
  |> List.map sanitize_module_component
  |> String.concat "__"

let compiled_page_module_name relative_file =
  "Pages__" ^ generated_module_base relative_file

let compiled_api_module_name relative_file =
  "Api__" ^ generated_module_base relative_file

let strip_directory_prefix ~directory source_file =
  let prefix = directory ^ "/" in
  let prefix_len = String.length prefix in
  if
    String.length source_file >= prefix_len
    && String.sub source_file 0 prefix_len = prefix
  then String.sub source_file prefix_len (String.length source_file - prefix_len)
  else source_file

let strip_pages_prefix source_file =
  source_file |> strip_directory_prefix ~directory:app_directory

let strip_api_prefix source_file =
  source_file |> strip_directory_prefix ~directory:app_api_directory

let native_module_name_of_source source_file =
  generated_module_base (strip_pages_prefix source_file)

let route_constructor_name_of_source source_file =
  strip_pages_prefix source_file
  |> Filename.remove_extension |> String.split_on_char '/'
  |> List.filter (fun segment -> segment <> "")
  |> List.map sanitize_library_component
  |> String.concat "_" |> String.capitalize_ascii

let compiled_page_module_name_of_source source_file =
  compiled_page_module_name (strip_pages_prefix source_file)

let compiled_api_module_name_of_source source_file =
  compiled_api_module_name (strip_api_prefix source_file)

let generated_route_binding_name source_file suffix =
  let base =
    native_module_name_of_source source_file |> String.lowercase_ascii
  in
  base ^ "_" ^ suffix
