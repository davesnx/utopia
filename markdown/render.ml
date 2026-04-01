(* It's a render to React.element, taked inspiration from https://github.com/dbuenzli/cmarkit/blob/be89f00c3a996bc92b3513464542a3a7009eb2c0/src/cmarkit_html.ml *)

open Cmarkit
module String_set = Set.Make (String)

let add kind value map =
  match value with Some i -> map |> List.cons (kind i) | None -> map

module State = struct
  type safety = Safe | Unsafe

  type t = {
    safety : safety;
    components : Components.t;
    mutable defs : Label.defs;
    mutable ids : String_set.t;
    mutable footnote_count : int;
    mutable footnotes :
      (* Text, id, ref count, footnote *)
      (string * string * int ref * Block.Footnote.t) Label.Map.t;
  }

  let make ~(safety : safety) ~defs ~components _ =
    let ids = String_set.empty and footnotes = Label.Map.empty in
    { safety; ids; footnote_count = 0; footnotes; defs; components }

  let get_defs state = state.defs
end

let unique_id ~(state : State.t) id =
  let rec loop ids id c =
    let id' = if c = 0 then id else String.concat "-" [ id; Int.to_string c ] in
    match String_set.mem id' ids with
    | true -> loop ids id (c + 1)
    | false ->
        state.ids <- String_set.add id' ids;
        id'
  in
  loop state.ids id 0

let html_escaped_string s =
  let len = String.length s in
  let b = Buffer.create len in
  let string = Buffer.add_string in
  let max_idx = len - 1 in
  let flush b start i =
    if start < len then Buffer.add_substring b s start (i - start)
  in
  let rec loop start i =
    if i > max_idx then flush b start i
    else
      let next = i + 1 in
      match String.get s i with
      | '\x00' ->
          flush b start i;
          Buffer.add_utf_8_uchar b Uchar.rep;
          loop next next
      | '&' ->
          flush b start i;
          string b "&amp;";
          loop next next
      | '<' ->
          flush b start i;
          string b "&lt;";
          loop next next
      | '>' ->
          flush b start i;
          string b "&gt;";
          loop next next
      (*    | '\'' -> flush c start i; string c "&apos;"; loop next next *)
      | '\"' ->
          flush b start i;
          string b "&quot;";
          loop next next
      | _ -> loop start next
  in
  loop 0 0;
  Buffer.contents b

let link_dest_and_title ~(state : State.t) ld =
  let dest =
    match Link_definition.dest ld with
    | None -> ""
    | Some (link, _) when state.safety = Safe && Inline.Link.is_unsafe link ->
        ""
    | Some (link, _) -> link
  in
  let title =
    match Link_definition.title ld with
    | None -> ""
    | Some title -> String.concat "\n" (List.map (fun (_, (t, _)) -> t) title)
  in
  (dest, title)

let pct_encoded_string s =
  let size = String.length s in
  let b = Buffer.create size in
  (* Percent encoded + HTML escaped *)
  let byte = Buffer.add_char and string = Buffer.add_string in
  let unsafe_hexdig_of_int i =
    match i < 10 with
    | true -> Char.unsafe_chr (i + 0x30)
    | false -> Char.unsafe_chr (i + 0x37)
  in
  let flush b max start i =
    if start <= max then Buffer.add_substring b s start (i - start)
  in
  let rec loop b s max start i =
    if i > max then flush b max start i
    else
      let next = i + 1 in
      match String.get s i with
      | '%' (* In CommonMark destinations may have percent encoded chars *)
      (* See https://tools.ietf.org/html/rfc3986 *)
      (* unreserved *)
      | 'A' .. 'Z'
      | 'a' .. 'z'
      | '0' .. '9'
      | '-' | '.' | '_' | '~'
      (* sub-delims *)
      | '!' | '$' (*'&' | '\'' | *)
      | '(' | ')' | '*' | '+' | ',' | ';' | '='
      (* gen-delims *)
      | ':' | '/' | '?' | '#' (* '[' | ']' cmark escapes them | *)
      | '@' ->
          loop b s max start next
      | '&' ->
          flush b max start i;
          string b "&amp;";
          loop b s max next next
      | '\'' ->
          flush b max start i;
          string b "&apos;";
          loop b s max next next
      | c ->
          flush b max start i;
          let hi = (Char.code c lsr 4) land 0xF in
          let lo = Char.code c land 0xF in
          byte b '%';
          byte b (unsafe_hexdig_of_int hi);
          byte b (unsafe_hexdig_of_int lo);
          loop b s max next next
  in
  loop b s (String.length s - 1) 0 0;
  Buffer.contents b

(* Rendering functions *)

let rec block_to_element ~(state : State.t) block =
  let open Block in
  match (block : Block.t) with
  | Blocks (blocks, _meta) ->
      let list =
        blocks
        |> List.map (fun block -> block_to_element ~state block)
        |> Array.of_list
      in
      React.fragment (React.array list)
  | Paragraph (paragraph, _meta) ->
      let inline = Paragraph.inline paragraph in
      state.components.p ~children:(inline_to_element ~state inline) ()
  | Heading (heading, _meta) -> (
      let level = Heading.level heading in
      let inline = Heading.inline heading in
      let component =
        match level with
        | 1 -> state.components.h1
        | 2 -> state.components.h2
        | 3 -> state.components.h3
        | 4 -> state.components.h4
        | 5 -> state.components.h5
        | 6 -> state.components.h6
        | _ -> assert false
      in
      match Heading.id heading with
      | None -> component ~children:(inline_to_element ~state inline) ()
      | Some (`Auto id | `Id id) ->
          component ~id:(unique_id ~state id)
            ~children:
              (state.components.a ~className:"anchor"
                 ~visibility:Elements.A.Hidden ~href:("#" ^ id)
                 ~children:(inline_to_element ~state inline)
                 ())
            ())
  | List (list, _meta) -> (
      (* let tight = List'.tight list in *)
      match List'.type' list with
      | `Unordered _ ->
          state.components.ul
            ~children:
              (React.list (List.map (list_item ~state) (List'.items list)))
            ()
      | `Ordered (start, _) -> (
          match start with
          | 1 ->
              state.components.ol
                ~children:
                  (React.list (List.map (list_item ~state) (List'.items list)))
                ()
          | not_one ->
              state.components.ol ~start:not_one
                ~children:
                  (React.list (List.map (list_item ~state) (List'.items list)))
                ()))
  | Block_quote (block_quote, _meta) ->
      state.components.blockquote
        ~children:(block_to_element ~state (Block_quote.block block_quote))
        ()
  | Code_block (code_block, _meta) -> (
      let info_string = Option.map fst (Code_block.info_string code_block) in
      let lang = Option.bind info_string Code_block.language_of_info_string in
      let code =
        Code_block.code code_block |> List.map fst |> String.concat ""
      in
      let contents =
        React.list
          (List.map
             (fun (l, _) -> React.string (html_escaped_string l ^ "\n"))
             (Code_block.code code_block))
      in
      match lang with
      | None ->
          state.components.pre ~className:"utopia-markdown-code-block"
            ~children:
              (state.components.code ~className:"utopia-markdown-code"
                 ~children:contents ())
            ()
      | Some (lang, _env) -> (
          match Markdown_highlight.highlight_html ~lang code with
          | Some html ->
              React.createElement "div"
                [
                  React.JSX.dangerouslyInnerHtml
                    object
                      method __html = html
                    end;
                ]
                []
          | None ->
              state.components.pre ~className:"utopia-markdown-code-block"
                ~children:
                  (state.components.code
                     ~className:("utopia-markdown-code language-" ^ lang)
                     ~children:contents ())
                ()))
  (* TODO: Make sure blank_line goes to null *)
  | Blank_line (_blank_node, _meta) -> React.null
  | Html_block (html, _meta) ->
      (* TODO: Make sure about "safe" *)
      React.createElement "div"
        [
          React.JSX.dangerouslyInnerHtml
            object
              method __html =
                String.concat "\n" (List.map (fun (l, _) -> l) html)
            end;
        ]
        []
  | Thematic_break (_thematic_break, _meta) -> state.components.hr ()
  | Link_reference_definition (_link_def, _meta) ->
      (* TODO: This should not be null *)
      React.null
  | _ -> assert false

(* TODO: Add tight case *)
and list_item ~state (item, _) =
  match Block.List_item.ext_task_marker item with
  | None ->
      state.components.li
        ~children:(block_to_element ~state (Block.List_item.block item))
        ()
  | Some (mark, _) -> (
      match Block.List_item.task_status_of_task_marker mark with
      | `Unchecked ->
          state.components.li ~marker:Elements.Li.Unchecked
            ~children:(block_to_element ~state (Block.List_item.block item))
            ()
      | `Checked | `Other _ ->
          state.components.li ~marker:Elements.Li.Checked
            ~children:(block_to_element ~state (Block.List_item.block item))
            ()
      | `Cancelled ->
          state.components.li ~marker:Elements.Li.Checked
            ~children:
              (state.components.del
                 ~children:
                   (block_to_element ~state (Block.List_item.block item))
                 ())
            ())

and inline_to_element ~state inline =
  let open Inline in
  match inline with
  | Text (text, _meta) -> React.string text
  | Autolink (autolink, _meta) ->
      let pre = if Autolink.is_email autolink then "mailto:" else "" in
      let url = pre ^ fst (Autolink.link autolink) in
      let url = if Link.is_unsafe url then "" else url in
      let content, _meta = Autolink.link autolink in
      state.components.a ~href:url ~children:(React.string content) ()
  | Break (break, _meta) -> (
      match Break.type' break with
      | `Hard -> state.components.br ()
      | `Soft -> (* Unsure about the ouput *) React.null)
  | Code_span (code_span, _meta) ->
      state.components.code ~className:"utopia-inline-code"
        ~children:(React.string (Code_span.code code_span))
        ()
  | Emphasis (emphasis, _meta) ->
      let inline = Emphasis.inline emphasis in
      state.components.em ~children:(inline_to_element ~state inline) ()
  | Strong_emphasis (emphasis, _meta) ->
      let inline = Emphasis.inline emphasis in
      state.components.strong ~children:(inline_to_element ~state inline) ()
  | Inlines (inlines, _meta) ->
      let list =
        inlines
        |> List.map (fun inline -> inline_to_element ~state inline)
        |> Array.of_list
      in
      React.fragment (React.array list)
  | Link (link, _meta) -> (
      match Inline.Link.reference_definition (State.get_defs state) link with
      | Some (Link_definition.Def (ld, _)) -> (
          let href, title = link_dest_and_title ~state ld in
          match title with
          | "" ->
              state.components.a ~href:(pct_encoded_string href)
                ~children:(inline_to_element ~state (Inline.Link.text link))
                ()
          | some_title ->
              state.components.a ~href:(pct_encoded_string href)
                ~title:(html_escaped_string some_title)
                ~children:(inline_to_element ~state (Inline.Link.text link))
                ())
      (* | Some (Block.Footnote.Def (fn, _)) -> link_footnote c l fn *)
      | Some (Block.Footnote.Def (_fn, _)) -> assert false
      | None ->
          inline_to_element ~state (Link.text link)
          (* comment_undefined_label c l *)
      | Some _ ->
          inline_to_element ~state (Link.text link)
          (* comment_unknown_def_type c l) *))
  | Image (link, _meta) -> (
      match Inline.Link.reference_definition (State.get_defs state) link with
      | Some (Link_definition.Def (ld, _)) -> (
          let src, title = link_dest_and_title ~state ld in
          let plain_text i =
            let lines = Inline.to_plain_text ~break_on_soft:false i in
            String.concat "\n" (List.map (String.concat "") lines)
          in
          let alt = Link.text link in
          match title with
          | "" ->
              state.components.img ~src:(pct_encoded_string src)
                ~alt:(plain_text alt)
                ~children:(inline_to_element ~state (Link.text link))
                ()
          | some_title ->
              state.components.img ~src:(pct_encoded_string src)
                ~alt:(plain_text alt)
                ~title:(html_escaped_string some_title)
                ~children:(inline_to_element ~state (Link.text link))
                ())
      (* | Some (Block.Footnote.Def (fn, _)) -> link_footnote c l fn *)
      | Some (Block.Footnote.Def (_fn, _)) -> assert false
      | None ->
          inline_to_element ~state (Link.text link)
          (* comment_undefined_label c l *)
      | Some _ ->
          inline_to_element ~state (Link.text link)
          (* comment_unknown_def_type c l) *))
  | Raw_html (raw_html, _meta) -> (
      match raw_html with
      | [] -> React.null
      | not_empty_html ->
          let html =
            not_empty_html
            (* TODO: What's l? *)
            |> List.map (fun (_l, line) ->
                React.string (Block_line.to_string line))
            |> Array.of_list
          in
          React.fragment (React.array html))
  | Ext_strikethrough (strikethrough, _meta) ->
      let inline = Strikethrough.inline strikethrough in
      state.components.del ~children:(inline_to_element ~state inline) ()
  | Ext_math_span (math_span, _meta) ->
      let content = Math_span.tex math_span in
      state.components.math_span ~children:(React.string content) ()
  | _ -> assert false

let of_doc ~(safety : State.safety) ~(components : Components.t) d =
  let blocks = Doc.block d in
  let defs = Doc.defs d in
  let state = State.make ~safety ~defs ~components () in
  let element = block_to_element ~state blocks in
  element
