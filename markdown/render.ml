open Cmarkit
module String_set = Set.Make (String)

let warn message = prerr_endline ("[utopia.markdown] " ^ message)

module State = struct
  type safety = Safe | Unsafe

  type footnote_entry = {
    index : int;
    id : string;
    ref_count : int ref;
    footnote : Block.Footnote.t;
  }

  type t = {
    safety : safety;
    components : Components.t;
    mutable defs : Label.defs;
    mutable ids : String_set.t;
    mutable footnote_count : int;
    mutable footnotes : footnote_entry Label.Map.t;
  }

  let make ~(safety : safety) ~defs ~components _ =
    let ids = String_set.empty in
    let footnotes = Label.Map.empty in
    { safety; ids; footnote_count = 0; footnotes; defs; components }

  let get_defs state = state.defs
end

let unique_id ~(state : State.t) id =
  let rec loop ids base suffix =
    let candidate =
      if suffix = 0 then base
      else String.concat "-" [ base; Int.to_string suffix ]
    in
    if String_set.mem candidate ids then loop ids base (suffix + 1)
    else (
      state.ids <- String_set.add candidate ids;
      candidate)
  in
  loop state.ids id 0

let footnote_id index = "fn-" ^ Int.to_string index

let footnote_ref_id fnid ref_index =
  "ref-" ^ Int.to_string ref_index ^ "-" ^ fnid

let make_footnote_ref_ids ~(state : State.t) label fn =
  match Label.Map.find_opt label state.footnotes with
  | Some entry ->
      incr entry.State.ref_count;
      let ref_index = !(entry.State.ref_count) in
      ( entry.State.index,
        entry.State.id,
        footnote_ref_id entry.State.id ref_index )
  | None ->
      state.footnote_count <- state.footnote_count + 1;
      let index = state.footnote_count in
      let id = footnote_id index in
      let entry = { State.index; id; ref_count = ref 1; footnote = fn } in
      state.footnotes <- Label.Map.add label entry state.footnotes;
      (index, id, footnote_ref_id id 1)

let html_escaped_string s =
  let len = String.length s in
  let buffer = Buffer.create len in
  let add_string = Buffer.add_string in
  let max_idx = len - 1 in
  let flush start i =
    if start < len then Buffer.add_substring buffer s start (i - start)
  in
  let rec loop start i =
    if i > max_idx then flush start i
    else
      let next = i + 1 in
      match String.get s i with
      | '\x00' ->
          flush start i;
          Buffer.add_utf_8_uchar buffer Uchar.rep;
          loop next next
      | '&' ->
          flush start i;
          add_string buffer "&amp;";
          loop next next
      | '<' ->
          flush start i;
          add_string buffer "&lt;";
          loop next next
      | '>' ->
          flush start i;
          add_string buffer "&gt;";
          loop next next
      | '"' ->
          flush start i;
          add_string buffer "&quot;";
          loop next next
      | _ -> loop start next
  in
  loop 0 0;
  Buffer.contents buffer

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
  let buffer = Buffer.create size in
  let add_char = Buffer.add_char in
  let add_string = Buffer.add_string in
  let unsafe_hexdig_of_int i =
    if i < 10 then Char.unsafe_chr (i + 0x30) else Char.unsafe_chr (i + 0x37)
  in
  let flush max start i =
    if start <= max then Buffer.add_substring buffer s start (i - start)
  in
  let rec loop max start i =
    if i > max then flush max start i
    else
      let next = i + 1 in
      match String.get s i with
      | '%'
      | 'A' .. 'Z'
      | 'a' .. 'z'
      | '0' .. '9'
      | '-' | '.' | '_' | '~' | '!' | '$' | '(' | ')' | '*' | '+' | ',' | ';'
      | '=' | ':' | '/' | '?' | '#' | '@' ->
          loop max start next
      | '&' ->
          flush max start i;
          add_string buffer "&amp;";
          loop max next next
      | '\'' ->
          flush max start i;
          add_string buffer "&apos;";
          loop max next next
      | c ->
          flush max start i;
          let hi = (Char.code c lsr 4) land 0xF in
          let lo = Char.code c land 0xF in
          add_char buffer '%';
          add_char buffer (unsafe_hexdig_of_int hi);
          add_char buffer (unsafe_hexdig_of_int lo);
          loop max next next
  in
  loop (String.length s - 1) 0 0;
  Buffer.contents buffer

let alignment_class = function
  | Some `Left -> Some "utopia-markdown-align-left"
  | Some `Center -> Some "utopia-markdown-align-center"
  | Some `Right -> Some "utopia-markdown-align-right"
  | None -> None

let list_nth_opt list index =
  let rec loop i = function
    | [] -> None
    | head :: tail -> if i = index then Some head else loop (i + 1) tail
  in
  loop 0 list

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
        | _ ->
            warn
              (Printf.sprintf "unsupported heading level %d; rendering as <h6>"
                 level);
            state.components.h6
      in
      match Heading.id heading with
      | None -> component ~children:(inline_to_element ~state inline) ()
      | Some (`Auto id | `Id id) ->
          let unique = unique_id ~state id in
          component ~id:unique
            ~children:
              (state.components.a ~className:"anchor"
                 ~visibility:Elements.A.Hidden ~href:("#" ^ unique)
                 ~children:(inline_to_element ~state inline)
                 ())
            ())
  | List (list, _meta) -> (
      match List'.type' list with
      | `Unordered _ ->
          state.components.ul
            ~children:
              (React.list (List.map (list_item ~state) (List'.items list)))
            ()
      | `Ordered (start, _) ->
          if start = 1 then
            state.components.ol
              ~children:
                (React.list (List.map (list_item ~state) (List'.items list)))
              ()
          else
            state.components.ol ~start
              ~children:
                (React.list (List.map (list_item ~state) (List'.items list)))
              ())
  | Block_quote (block_quote, _meta) ->
      state.components.blockquote
        ~children:(block_to_element ~state (Block_quote.block block_quote))
        ()
  | Code_block (code_block, _meta) -> (
      let info_string = Option.map fst (Code_block.info_string code_block) in
      let lang = Option.bind info_string Code_block.language_of_info_string in
      let code_lines = Code_block.code code_block in
      let code = code_lines |> List.map fst |> String.concat "\n" in
      let contents =
        React.list
          (List.map
             (fun (line, _) -> React.string (html_escaped_string line ^ "\n"))
             code_lines)
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
                    (object
                       method __html = html
                    end);
                ]
                []
          | None ->
              state.components.pre ~className:"utopia-markdown-code-block"
                ~children:
                  (state.components.code
                     ~className:("utopia-markdown-code language-" ^ lang)
                     ~children:contents ())
                ()))
  | Blank_line (_blank_node, _meta) -> React.null
  | Html_block (html, _meta) ->
      React.createElement "div"
        [
          React.JSX.dangerouslyInnerHtml
            (object
               method __html =
                 String.concat "\n" (List.map (fun (line, _) -> line) html)
            end);
        ]
        []
  | Thematic_break (_thematic_break, _meta) -> state.components.hr ()
  | Link_reference_definition (_link_def, _meta) -> React.null
  | Ext_table (table, _meta) -> table_to_element ~state table
  | Ext_footnote_definition (_footnote, _meta) -> React.null
  | Ext_math_block (math_block, _meta) ->
      let lines = Block.Code_block.code math_block in
      let text =
        lines
        |> List.map (fun (line, _) -> React.string (line ^ "\n"))
        |> React.list
      in
      state.components.pre ~className:"utopia-markdown-math-block"
        ~children:(state.components.code ~children:text ())
        ()
  | _ ->
      warn "unsupported markdown block node encountered; rendering nothing";
      React.null

and table_to_element ~(state : State.t) (table : Block.Table.t) =
  let col_count = Block.Table.col_count table in
  let rows = Block.Table.rows table in
  let render_row ~is_header ~aligns cells =
    let rec build index cells acc =
      if index >= col_count then List.rev acc
      else
        let cell_inline, rest_cells =
          match cells with
          | [] -> (React.null, [])
          | (inline, _layout) :: rest -> (inline_to_element ~state inline, rest)
        in
        let className =
          Option.bind (list_nth_opt aligns index)
            (fun ((align_opt, _count), _meta) -> alignment_class align_opt)
        in
        let cell =
          if is_header then
            state.components.th ?className ~children:cell_inline ()
          else state.components.td ?className ~children:cell_inline ()
        in
        build (index + 1) rest_cells (cell :: acc)
    in
    state.components.tr ~children:(React.list (build 0 cells [])) ()
  in
  let rec collect aligns header_rows body_rows = function
    | [] -> (List.rev header_rows, List.rev body_rows)
    | ((`Header cells, _), _) :: rest ->
        let aligns, rest =
          match rest with
          | ((`Sep sep_aligns, _), _) :: tail -> (sep_aligns, tail)
          | _ -> (aligns, rest)
        in
        let row = render_row ~is_header:true ~aligns cells in
        collect aligns (row :: header_rows) body_rows rest
    | ((`Data cells, _), _) :: rest ->
        let row = render_row ~is_header:false ~aligns cells in
        collect aligns header_rows (row :: body_rows) rest
    | ((`Sep sep_aligns, _), _) :: rest ->
        collect sep_aligns header_rows body_rows rest
  in
  let header_rows, body_rows = collect [] [] [] rows in
  let sections =
    (if header_rows = [] then []
     else [ state.components.thead ~children:(React.list header_rows) () ])
    @ [ state.components.tbody ~children:(React.list body_rows) () ]
  in
  state.components.table ~children:(React.list sections) ()

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

and link_footnote ~state link fn =
  match Inline.Link.referenced_label link with
  | None -> inline_to_element ~state (Inline.Link.text link)
  | Some label ->
      let key = Label.key label in
      let index, id, ref_id = make_footnote_ref_ids ~state key fn in
      let label_text = "[" ^ Int.to_string index ^ "]" in
      state.components.footnote_ref ~href:("#" ^ id) ~id:ref_id
        ~children:(React.string label_text) ()

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
      | `Soft -> React.string " ")
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
      | Some (Link_definition.Def (ld, _)) ->
          let href, title = link_dest_and_title ~state ld in
          if title = "" then
            state.components.a ~href:(pct_encoded_string href)
              ~children:(inline_to_element ~state (Inline.Link.text link))
              ()
          else
            state.components.a ~href:(pct_encoded_string href)
              ~title:(html_escaped_string title)
              ~children:(inline_to_element ~state (Inline.Link.text link))
              ()
      | Some (Block.Footnote.Def (fn, _)) -> link_footnote ~state link fn
      | None -> inline_to_element ~state (Link.text link)
      | Some _ ->
          warn "unknown link definition type encountered; rendering link text";
          inline_to_element ~state (Link.text link))
  | Image (link, _meta) -> (
      match Inline.Link.reference_definition (State.get_defs state) link with
      | Some (Link_definition.Def (ld, _)) ->
          let src, title = link_dest_and_title ~state ld in
          let plain_text inline =
            let lines = Inline.to_plain_text ~break_on_soft:false inline in
            String.concat "\n" (List.map (String.concat "") lines)
          in
          let alt = Link.text link in
          if title = "" then
            state.components.img ~src:(pct_encoded_string src)
              ~alt:(plain_text alt)
              ~children:(inline_to_element ~state (Link.text link))
              ()
          else
            state.components.img ~src:(pct_encoded_string src)
              ~alt:(plain_text alt)
              ~title:(html_escaped_string title)
              ~children:(inline_to_element ~state (Link.text link))
              ()
      | Some (Block.Footnote.Def _) ->
          warn "footnote reference used as image; rendering fallback text";
          inline_to_element ~state (Link.text link)
      | None -> inline_to_element ~state (Link.text link)
      | Some _ ->
          warn
            "unknown image definition type encountered; rendering fallback text";
          inline_to_element ~state (Link.text link))
  | Raw_html (raw_html, _meta) -> (
      match raw_html with
      | [] -> React.null
      | lines ->
          let html =
            lines
            |> List.map (fun (_loc, line) ->
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
  | _ ->
      warn "unsupported markdown inline node encountered; rendering nothing";
      React.null

let footnotes_element ~(state : State.t) =
  if Label.Map.is_empty state.footnotes then None
  else
    let entries =
      Label.Map.fold (fun _label entry acc -> entry :: acc) state.footnotes []
      |> List.sort (fun left right ->
          Int.compare left.State.index right.State.index)
    in
    let render_backrefs entry =
      let count = !(entry.State.ref_count) in
      if count <= 0 then React.null
      else
        let refs =
          List.init count (fun i ->
              let ref_index = i + 1 in
              let label =
                if count = 1 then "back" else "back " ^ Int.to_string ref_index
              in
              state.components.footnote_backref
                ~href:("#" ^ footnote_ref_id entry.State.id ref_index)
                ~children:(React.string label) ())
        in
        state.components.div ~className:"utopia-markdown-footnote-backrefs"
          ~children:(React.list refs) ()
    in
    let items =
      entries
      |> List.map (fun entry ->
          let content =
            block_to_element ~state (Block.Footnote.block entry.State.footnote)
          in
          let backlinks = render_backrefs entry in
          state.components.footnotes_item ~id:entry.State.id
            ~children:(React.fragment (React.array [| content; backlinks |]))
            ())
    in
    Some
      (state.components.footnotes_section
         ~children:
           (state.components.footnotes_list ~children:(React.list items) ())
         ())

let of_doc ~(safety : State.safety) ~(components : Components.t) d =
  let blocks = Doc.block d in
  let defs = Doc.defs d in
  let state = State.make ~safety ~defs ~components () in
  let content = block_to_element ~state blocks in
  match footnotes_element ~state with
  | None -> content
  | Some footnotes -> React.fragment (React.array [| content; footnotes |])
