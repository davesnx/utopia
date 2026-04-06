type post = {
  slug : string;
  title : string;
  date : string;
  description : string;
}

type frontmatter = {
  title : string option;
  date : string option;
  description : string option;
}

let empty_frontmatter : frontmatter =
  { title = None; date = None; description = None }

let index_of_substring haystack needle start =
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  let rec loop i =
    if i + needle_len > haystack_len then None
    else if String.sub haystack i needle_len = needle then Some i
    else loop (i + 1)
  in
  loop start

let content_dir () =
  let executable_path =
    if Filename.is_relative Sys.executable_name then
      Filename.concat (Sys.getcwd ()) Sys.executable_name
    else Sys.executable_name
  in
  let build_marker = "/_build/default/" in
  match index_of_substring executable_path build_marker 0 with
  | Some index ->
      let build_root = String.sub executable_path 0 index in
      let remainder_start = index + String.length build_marker in
      let remainder =
        String.sub executable_path remainder_start
          (String.length executable_path - remainder_start)
      in
      let relative_dir = Filename.dirname (Filename.dirname remainder) in
      Filename.concat (Filename.concat build_root relative_dir) "content"
  | None ->
      let executable_dir = Filename.dirname executable_path in
      if Filename.basename executable_dir = "_utopia" then
        Filename.concat (Filename.dirname executable_dir) "content"
      else Filename.concat (Sys.getcwd ()) "content"

let read_raw_content slug =
  [%platform
    match () with
    | Server ->
        let path = Printf.sprintf "%s/%s.md" (content_dir ()) slug in
        In_channel.with_open_text path (fun ch -> In_channel.input_all ch)
    | Client ->
        ignore slug;
        ""]

let strip_quotes value =
  let value_len = String.length value in
  if value_len >= 2 then
    let first = value.[0] in
    let last = value.[value_len - 1] in
    if (first = '"' && last = '"') || (first = '\'' && last = '\'') then
      String.sub value 1 (value_len - 2)
    else value
  else value

let parse_frontmatter_line (frontmatter : frontmatter) line : frontmatter =
  match String.index_opt line ':' with
  | None -> frontmatter
  | Some index -> (
      let key = String.sub line 0 index |> String.trim in
      let value =
        String.sub line (index + 1) (String.length line - index - 1)
        |> String.trim |> strip_quotes
      in
      match key with
      | "title" -> { frontmatter with title = Some value }
      | "date" -> { frontmatter with date = Some value }
      | "description" -> { frontmatter with description = Some value }
      | _ -> frontmatter)

let split_frontmatter markdown =
  match String.split_on_char '\n' markdown with
  | first :: rest when String.trim first = "---" ->
      let rec loop acc = function
        | [] -> (empty_frontmatter, markdown)
        | line :: tail when String.trim line = "---" ->
            let frontmatter =
              List.fold_left parse_frontmatter_line empty_frontmatter
                (List.rev acc)
            in
            (frontmatter, String.concat "\n" tail)
        | line :: tail -> loop (line :: acc) tail
      in
      loop [] rest
  | _ -> (empty_frontmatter, markdown)

let require_frontmatter slug field = function
  | Some value -> value
  | None ->
      invalid_arg
        (Printf.sprintf "Missing `%s` frontmatter in %s.md" field slug)

let read_post slug : post * string =
  let frontmatter, body = read_raw_content slug |> split_frontmatter in
  ( {
      slug;
      title = require_frontmatter slug "title" frontmatter.title;
      date = require_frontmatter slug "date" frontmatter.date;
      description =
        require_frontmatter slug "description" frontmatter.description;
    },
    body )

let slug_of_markdown_filename filename =
  String.sub filename 0 (String.length filename - 3)

let post_slugs () =
  [%platform
    match () with
    | Server ->
        Sys.readdir (content_dir ())
        |> Array.to_list
        |> List.filter (fun filename -> Filename.check_suffix filename ".md")
        |> List.map slug_of_markdown_filename
    | Client -> []]

let posts : post list =
  post_slugs ()
  |> List.map (fun slug -> read_post slug |> fst)
  |> List.sort (fun (left : post) (right : post) ->
      String.compare right.date left.date)

let find_post slug = List.find_opt (fun (p : post) -> p.slug = slug) posts

let read_content slug =
  [%platform
    match () with
    | Server -> read_post slug |> snd
    | Client ->
        ignore slug;
        ""]

let starts_with text prefix =
  let text_len = String.length text in
  let prefix_len = String.length prefix in
  text_len >= prefix_len && String.sub text 0 prefix_len = prefix

let is_absolute_href href =
  starts_with href "http://"
  || starts_with href "https://"
  || starts_with href "//" || starts_with href "mailto:"
  || starts_with href "tel:"

let render_element slug =
  match%platform () with
  | Server ->
      let blog_components =
        Components.make
          ~p:(fun ?className:_ ~children () ->
            React.createElement "p"
              [ React.JSX.string "class" "className" "mt-7" ]
              [ children ])
          ~a:(fun ?title ?className:_ ?visibility:_ ~href ~children () ->
            let className =
              "break-words decoration-from-font underline underline-offset-2 \
               decoration-slate-400 hover:decoration-slate-700"
            in
            if is_absolute_href href then
              React.createElement "a"
                ([
                   React.JSX.string "href" "href" href;
                   React.JSX.string "class" "className" className;
                 ]
                @
                match title with
                | Some value -> [ React.JSX.string "title" "title" value ]
                | None -> [])
                [ children ]
            else
              Utopia_router_link.make
                (Utopia_router_link.makeProps
                   ~to_:(Utopia_route.of_href href)
                   ~className ~children ()))
          ~h1:(fun ?className:_ ?id ~children () ->
            React.createElement "h1"
              ([
                 React.JSX.string "class" "className"
                   "text-xl font-semibold mt-10 mb-4 text-slate-800";
               ]
              @
              match id with
              | Some id -> [ React.JSX.string "id" "id" id ]
              | None -> [])
              [ children ])
          ~h2:(fun ?className:_ ?id ~children () ->
            React.createElement "h2"
              ([
                 React.JSX.string "class" "className"
                   "text-lg font-semibold mt-8 mb-3 text-slate-800";
               ]
              @
              match id with
              | Some id -> [ React.JSX.string "id" "id" id ]
              | None -> [])
              [ children ])
          ~h3:(fun ?className:_ ?id ~children () ->
            React.createElement "h3"
              ([
                 React.JSX.string "class" "className"
                   "text-base font-semibold mt-6 mb-2 text-slate-800";
               ]
              @
              match id with
              | Some id -> [ React.JSX.string "id" "id" id ]
              | None -> [])
              [ children ])
          ~ul:(fun ?className:_ ~children () ->
            React.createElement "ul"
              [
                React.JSX.string "class" "className"
                  "mt-4 ml-6 list-disc space-y-1";
              ]
              [ children ])
          ~ol:(fun ?className:_ ?start:_ ~children () ->
            React.createElement "ol"
              [
                React.JSX.string "class" "className"
                  "mt-4 ml-6 list-decimal space-y-1";
              ]
              [ children ])
          ~blockquote:(fun ?className:_ ~children () ->
            React.createElement "blockquote"
              [
                React.JSX.string "class" "className"
                  "mt-7 border-l-2 border-slate-300 pl-4 text-slate-500 italic";
              ]
              [ children ])
          ~strong:(fun ?className:_ ~children () ->
            React.createElement "strong"
              [
                React.JSX.string "class" "className"
                  "font-semibold text-slate-800";
              ]
              [ children ])
          ~em:(fun ?className:_ ~children () ->
            React.createElement "em"
              [ React.JSX.string "class" "className" "italic" ]
              [ children ])
          ~hr:(fun ?className:_ () ->
            React.createElement "hr"
              [ React.JSX.string "class" "className" "my-8 border-slate-300" ]
              [])
          ()
      in
      let markdown = read_content slug in
      let doc = Utopia_markdown.doc_of_string markdown in
      Utopia_markdown.element_of_doc ~components:blog_components doc
  | Client ->
      ignore slug;
      React.null
