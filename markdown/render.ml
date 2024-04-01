(* It's a render to React.element, taked inspiration from https://github.com/dbuenzli/cmarkit/blob/be89f00c3a996bc92b3513464542a3a7009eb2c0/src/cmarkit_html.ml *)

open Cmarkit
module String_set = Set.Make (String)

module Default_components = struct
  module P = struct
    let make ~children = React.createElement "p" [] children
  end

  module A = struct
    let make ?(title = "") ?(className = "") ?(ariaHidden = "false") ~href
        ~children () =
      React.createElement "a"
        [
          React.JSX.string "class" className;
          React.JSX.string "aria-hidden" ariaHidden;
          React.JSX.string "href" href;
          React.JSX.string "title" title;
        ]
        children
  end

  module Blockquote = struct
    let make ~children = React.createElement "blockquote" [] children
  end

  module Ol = struct
    let make ?start ~children () =
      match start with
      | None -> React.createElement "ol" [] children
      | Some start ->
          React.createElement "ol" [ React.JSX.int "start" start ] children
  end

  module Ul = struct
    let make ~children = React.createElement "ul" [] children
  end

  module Pre = struct
    let make ~children = React.createElement "pre" [] children
  end

  module Hr = struct
    let make () = React.createElement "hr" [] []
  end

  module Br = struct
    let make () = React.createElement "br" [] []
  end

  module Code = struct
    let make ?(className = "") ~children () =
      React.createElement "code" [ React.JSX.string "class" className ] children
  end

  module Em = struct
    let make ~children = React.createElement "em" [] children
  end

  module Strong = struct
    let make ~children = React.createElement "strong" [] children
  end

  module Del = struct
    let make ~children = React.createElement "del" [] children
  end

  module Math_span = struct
    let make ~children = React.createElement "span" [] children
  end

  module Li = struct
    let make ?(disabled = false) ?(checked = false) ~children () =
      match (disabled, checked) with
      | false, false -> React.createElement "li" [] children
      | true, false ->
          React.createElement "li" []
            [
              React.createElement "div"
                [ React.JSX.string "class" "task" ]
                [
                  React.createElement "input"
                    [
                      React.JSX.string "type" "checkbox";
                      React.JSX.bool "disabled" true;
                    ]
                    children;
                ];
            ]
      | false, true ->
          React.createElement "li" []
            [
              React.createElement "div"
                [ React.JSX.string "class" "task" ]
                [
                  React.createElement "input"
                    [
                      React.JSX.string "type" "checkbox";
                      React.JSX.bool "checked" true;
                    ]
                    children;
                ];
            ]
      | true, true ->
          React.createElement "li" []
            [
              React.createElement "div"
                [ React.JSX.string "class" "task" ]
                [
                  React.createElement "input"
                    [
                      React.JSX.string "type" "checkbox";
                      React.JSX.bool "disabled" true;
                      React.JSX.bool "checked" true;
                    ]
                    children;
                ];
            ]
  end

  module Div = struct
    let make ~children = React.createElement "li" [] children
  end

  module Img = struct
    let make ?(title = "") ~src ~alt ~children () =
      React.createElement "img"
        [
          React.JSX.string "src" src;
          React.JSX.string "alt" alt;
          React.JSX.string "title" title;
        ]
        children
  end

  module H1 = struct
    let make ?(id = "") ~children () =
      React.createElement "h1" [ React.JSX.string "id" id ] children
  end

  module H2 = struct
    let make ?(id = "") ~children () =
      React.createElement "h2" [ React.JSX.string "id" id ] children
  end

  module H3 = struct
    let make ?(id = "") ~children () =
      React.createElement "h3" [ React.JSX.string "id" id ] children
  end

  module H4 = struct
    let make ?(id = "") ~children () =
      React.createElement "h4" [ React.JSX.string "id" id ] children
  end

  module H5 = struct
    let make ?(id = "") ~children () =
      React.createElement "h5" [ React.JSX.string "id" id ] children
  end

  module H6 = struct
    let make ?(id = "") ~children () =
      React.createElement "h6" [ React.JSX.string "id" id ] children
  end
end

module Custom_components = struct
  type t = {
    p : children:React.element list -> React.element;
    a :
      ?title:string ->
      ?className:string ->
      ?ariaHidden:string ->
      href:string ->
      children:React.element list ->
      unit ->
      React.element;
    blockquote : children:React.element list -> React.element;
    ol : ?start:int -> children:React.element list -> unit -> React.element;
    ul : children:React.element list -> React.element;
    pre : children:React.element list -> React.element;
    hr : unit -> React.element;
    br : unit -> React.element;
    code :
      ?className:string -> children:React.element list -> unit -> React.element;
    em : children:React.element list -> React.element;
    strong : children:React.element list -> React.element;
    del : children:React.element list -> React.element;
    math_span : children:React.element list -> React.element;
    li :
      ?disabled:bool ->
      ?checked:bool ->
      children:React.element list ->
      unit ->
      React.element;
    div : children:React.element list -> React.element;
    img :
      ?title:string ->
      src:string ->
      alt:string ->
      children:React.element list ->
      unit ->
      React.element;
    h1 : ?id:string -> children:React.element list -> unit -> React.element;
    h2 : ?id:string -> children:React.element list -> unit -> React.element;
    h3 : ?id:string -> children:React.element list -> unit -> React.element;
    h4 : ?id:string -> children:React.element list -> unit -> React.element;
    h5 : ?id:string -> children:React.element list -> unit -> React.element;
    h6 : ?id:string -> children:React.element list -> unit -> React.element;
  }

  let make ?p ?a ?blockquote ?ol ?ul ?pre ?hr ?br ?code ?em ?strong ?del
      ?math_span ?li ?div ?img ?h1 ?h2 ?h3 ?h4 ?h5 ?h6 () =
    {
      p = Option.value ~default:Default_components.P.make p;
      a = Option.value ~default:Default_components.A.make a;
      blockquote =
        Option.value ~default:Default_components.Blockquote.make blockquote;
      ol = Option.value ~default:Default_components.Ol.make ol;
      ul = Option.value ~default:Default_components.Ul.make ul;
      pre = Option.value ~default:Default_components.Pre.make pre;
      hr = Option.value ~default:Default_components.Hr.make hr;
      br = Option.value ~default:Default_components.Br.make br;
      code = Option.value ~default:Default_components.Code.make code;
      em = Option.value ~default:Default_components.Em.make em;
      strong = Option.value ~default:Default_components.Strong.make strong;
      del = Option.value ~default:Default_components.Del.make del;
      math_span =
        Option.value ~default:Default_components.Math_span.make math_span;
      li = Option.value ~default:Default_components.Li.make li;
      div = Option.value ~default:Default_components.Div.make div;
      img = Option.value ~default:Default_components.Img.make img;
      h1 = Option.value ~default:Default_components.H1.make h1;
      h2 = Option.value ~default:Default_components.H2.make h2;
      h3 = Option.value ~default:Default_components.H3.make h3;
      h4 = Option.value ~default:Default_components.H4.make h4;
      h5 = Option.value ~default:Default_components.H5.make h5;
      h6 = Option.value ~default:Default_components.H6.make h6;
    }
end

module State = struct
  type t = {
    safe : bool;
    backend_blocks : bool;
    components : Custom_components.t;
    mutable defs : Label.defs;
    mutable ids : String_set.t;
    mutable footnote_count : int;
    mutable footnotes :
      (* Text, id, ref count, footnote *)
      (string * string * int ref * Block.Footnote.t) Label.Map.t;
  }

  let make ?(backend_blocks = false) ~safe ~defs ~components _ =
    let ids = String_set.empty and footnotes = Label.Map.empty in
    {
      safe;
      backend_blocks;
      ids;
      footnote_count = 0;
      footnotes;
      defs;
      components;
    }

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
    | Some (link, _) when state.safe && Inline.Link.is_unsafe link -> ""
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
      React.Fragment (React.List list)
  | Paragraph (paragraph, _meta) ->
      let inline = Paragraph.inline paragraph in
      state.components.p ~children:[ inline_to_element ~state inline ]
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
      | None -> component ~children:[ inline_to_element ~state inline ] ()
      | Some (`Auto id | `Id id) ->
          component ~id:(unique_id ~state id)
            ~children:
              [
                state.components.a ~className:"anchor" ~ariaHidden:"true"
                  ~href:("#" ^ id)
                  ~children:[ inline_to_element ~state inline ]
                  ();
              ]
            ())
  | List (list, _meta) -> (
      (* let tight = List'.tight list in *)
      match List'.type' list with
      | `Unordered _ ->
          state.components.ul
            ~children:(List.map (list_item ~state) (List'.items list))
      | `Ordered (start, _) -> (
          match start with
          | 1 ->
              state.components.ol
                ~children:(List.map (list_item ~state) (List'.items list))
                ()
          | not_one ->
              state.components.ol ~start:not_one
                ~children:(List.map (list_item ~state) (List'.items list))
                ()))
  | Block_quote (block_quote, _meta) ->
      state.components.blockquote
        ~children:[ block_to_element ~state (Block_quote.block block_quote) ]
  | Code_block (code_block, _meta) -> (
      let info_string = Option.map fst (Code_block.info_string code_block) in
      let lang = Option.bind info_string Code_block.language_of_info_string in
      let contents =
        List.map
          (fun (l, _) -> React.string (html_escaped_string l))
          (Code_block.code code_block)
      in
      match lang with
      | None ->
          state.components.pre
            ~children:[ state.components.code ~children:contents () ]
      | Some (lang, _env) ->
          state.components.pre
            ~children:
              [
                state.components.code ~className:("language-" ^ lang)
                  ~children:contents ();
              ])
  (* TODO: Make sure blank_line goes to null *)
  | Blank_line (_blank_node, _meta) -> React.null
  | Html_block (html, _meta) ->
      (* TODO: Make sure about "safe" *)
      React.InnerHtml (String.concat "\n" (List.map (fun (l, _) -> l) html))
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
        ~children:[ block_to_element ~state (Block.List_item.block item) ]
        ()
  | Some (mark, _) -> (
      match Block.List_item.task_status_of_task_marker mark with
      | `Unchecked ->
          state.components.li ~disabled:true
            ~children:[ block_to_element ~state (Block.List_item.block item) ]
            ()
      | `Checked | `Other _ ->
          state.components.li ~checked:true
            ~children:[ block_to_element ~state (Block.List_item.block item) ]
            ()
      | `Cancelled ->
          state.components.li ~checked:true
            ~children:
              [
                state.components.del
                  ~children:
                    [ block_to_element ~state (Block.List_item.block item) ];
              ]
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
      state.components.a ~href:url ~children:[ React.string content ] ()
  | Break (break, _meta) -> (
      match Break.type' break with
      | `Hard -> state.components.br ()
      | `Soft -> (* Unsure about the ouput *) React.null)
  | Code_span (code_span, _meta) ->
      state.components.code
        ~children:[ React.string (Code_span.code code_span) ]
        ()
  | Emphasis (emphasis, _meta) ->
      let inline = Emphasis.inline emphasis in
      state.components.em ~children:[ inline_to_element ~state inline ]
  | Strong_emphasis (emphasis, _meta) ->
      let inline = Emphasis.inline emphasis in
      state.components.strong ~children:[ inline_to_element ~state inline ]
  | Inlines (inlines, _meta) ->
      let list =
        inlines
        |> List.map (fun inline -> inline_to_element ~state inline)
        |> Array.of_list
      in
      React.Fragment (React.List list)
  | Link (link, _meta) -> (
      match Inline.Link.reference_definition (State.get_defs state) link with
      | Some (Link_definition.Def (ld, _)) -> (
          let href, title = link_dest_and_title ~state ld in
          match title with
          | "" ->
              state.components.a ~href:(pct_encoded_string href)
                ~children:[ inline_to_element ~state (Inline.Link.text link) ]
                ()
          | some_title ->
              state.components.a ~href:(pct_encoded_string href)
                ~title:(html_escaped_string some_title)
                ~children:[ inline_to_element ~state (Inline.Link.text link) ]
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
                ~children:[ inline_to_element ~state (Link.text link) ]
                ()
          | some_title ->
              state.components.img ~src:(pct_encoded_string src)
                ~alt:(plain_text alt)
                ~title:(html_escaped_string some_title)
                ~children:[ inline_to_element ~state (Link.text link) ]
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
          React.Fragment (React.List html))
  | Ext_strikethrough (strikethrough, _meta) ->
      let inline = Strikethrough.inline strikethrough in
      state.components.del ~children:[ inline_to_element ~state inline ]
  | Ext_math_span (math_span, _meta) ->
      let content = Math_span.tex math_span in
      state.components.math_span ~children:[ React.string content ]
  | _ -> assert false

let of_doc ~safe:_ ~(components : Custom_components.t) d =
  let blocks = Doc.block d in
  let defs = Doc.defs d in
  let state =
    State.make ~backend_blocks:false ~safe:true ~defs ~components ()
  in
  let element = block_to_element ~state blocks in
  element
