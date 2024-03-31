(* Adapted from https://github.com/dbuenzli/cmarkit/blob/be89f00c3a996bc92b3513464542a3a7009eb2c0/src/cmarkit_html.ml
   Original license: *)

(*---------------------------------------------------------------------------
   Copyright (c) 2021 The cmarkit programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmarkit
module String_set = Set.Make (String)

(* Renderer state *)

module State = struct
  type t = {
    safe : bool;
    backend_blocks : bool;
    mutable defs : Label.defs;
    mutable ids : String_set.t;
    mutable footnote_count : int;
    mutable footnotes :
      (* Text, id, ref count, footnote *)
      (string * string * int ref * Block.Footnote.t) Label.Map.t;
  }

  let make ?(backend_blocks = false) ~safe ~defs _ =
    let ids = String_set.empty and footnotes = Label.Map.empty in
    { safe; backend_blocks; ids; footnote_count = 0; footnotes; defs }

  let get_defs state = state.defs
end

let unique_id (state : State.t) id =
  let rec loop ids id c =
    let id' = if c = 0 then id else String.concat "-" [ id; Int.to_string c ] in
    match String_set.mem id' ids with
    | true -> loop ids id (c + 1)
    | false ->
        state.ids <- String_set.add id' ids;
        id'
  in
  loop state.ids id 0

let footnote_id label =
  let make_label l = String.map (function ' ' | '\t' -> '-' | c -> c) l in
  "fn-" ^ make_label (String.sub label 1 (String.length label - 1))

let footnote_ref_id fnid c = String.concat "-" [ "ref"; Int.to_string c; fnid ]

let make_footnote_ref_ids (state : State.t) label fn =
  match Label.Map.find_opt label state.footnotes with
  | Some (text, id, refc, _) ->
      incr refc;
      (text, id, footnote_ref_id id !refc)
  | None ->
      state.footnote_count <- state.footnote_count + 1;
      let text =
        String.concat "" [ "["; Int.to_string state.footnote_count; "]" ]
      in
      let id = footnote_id label in
      state.footnotes <-
        Label.Map.add label (text, id, ref 1, fn) state.footnotes;
      (text, id, footnote_ref_id id 1)

(* Escaping *)

let buffer_add_html_escaped_uchar b u =
  match Uchar.to_int u with
  | 0x0000 -> Buffer.add_utf_8_uchar b Uchar.rep
  | 0x0026 (* & *) -> Buffer.add_string b "&amp;"
  | 0x003C (* < *) -> Buffer.add_string b "&lt;"
  | 0x003E (* > *) -> Buffer.add_string b "&gt;"
  (* | 0x0027 (* ' *) -> Buffer.add_string b "&apos;" *)
  | 0x0022 (* '\"' *) -> Buffer.add_string b "&quot;"
  | _ -> Buffer.add_utf_8_uchar b u

(* let html_escaped_uchar c s = buffer_add_html_escaped_uchar (C.buffer c) s *)

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

let buffer_add_pct_encoded_string s =
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

let pct_encoded_string s = buffer_add_pct_encoded_string s

(* Rendering functions *)

(* let comment c s =
     C.string c "<!-- ";
     html_escaped_string c s;
     C.string c " -->"

   let comment_undefined_label c l =
     match Inline.Link.referenced_label l with
     | None -> ()
     | Some def -> comment c ("Undefined label " ^ Label.key def)

   let comment_unknown_def_type c l =
     match Inline.Link.referenced_label l with
     | None -> ()
     | Some def -> comment c ("Unknown label definition type for " ^ Label.key def)

   let comment_foonote_image c l =
     match Inline.Link.referenced_label l with
     | None -> ()
     | Some def -> comment c ("Footnote " ^ Label.key def ^ " referenced as image")

   let block_lines c = function
     (* newlines only between lines *)
     | [] -> ()
     | (l, _) :: ls ->
         let line c (l, _) =
           C.byte c '\n';
           C.string c l
         in
         C.string c l;
         List.iter (line c) ls *)

(* Inline rendering *)

(* let autolink c a =
     let pre = if Inline.Autolink.is_email a then "mailto:" else "" in
     let url = pre ^ fst (Inline.Autolink.link a) in
     let url = if Inline.Link.is_unsafe url then "" else url in
     C.string c "<a href=\"";
     pct_encoded_string c url;
     C.string c "\">";
     html_escaped_string c (fst (Inline.Autolink.link a));
     C.string c "</a>"

   let break c b =
     match Inline.Break.type' b with
     | `Hard -> C.string c "<br>\n"
     | `Soft -> C.byte c '\n'

   let code_span c cs =
     C.string c "<code>";
     html_escaped_string c (Inline.Code_span.code cs);
     C.string c "</code>"

   let emphasis c e =
     C.string c "<em>";
     C.inline c (Inline.Emphasis.inline e);
     C.string c "</em>"

   let strong_emphasis c e =
     C.string c "<strong>";
     C.inline c (Inline.Emphasis.inline e);
     C.string c "</strong>"

   let image ?(close = " >") c i =
     match Inline.Link.reference_definition (C.get_defs c) i with
     | Some (Link_definition.Def (ld, _)) ->
         let plain_text i =
           let lines = Inline.to_plain_text ~break_on_soft:false i in
           String.concat "\n" (List.map (String.concat "") lines)
         in
         let link, title = link_dest_and_title c ld in
         C.string c "<img src=\"";
         pct_encoded_string c link;
         C.string c "\" alt=\"";
         html_escaped_string c (plain_text (Inline.Link.text i));
         C.byte c '\"';
         if title <> "" then (
           C.string c " title=\"";
           html_escaped_string c title;
           C.byte c '\"');
         C.string c close
     | Some (Block.Footnote.Def _) -> comment_foonote_image c i
     | None -> comment_undefined_label c i
     | Some _ -> comment_unknown_def_type c i

   let link_footnote c l fn =
     let key = Label.key (Option.get (Inline.Link.referenced_label l)) in
     let text, label, ref = make_footnote_ref_ids c key fn in
     let is_full_ref =
       match Inline.Link.reference l with `Ref (`Full, _, _) -> true | _ -> false
     in
     if is_full_ref then (
       C.string c "<a href=\"#";
       pct_encoded_string c label;
       C.string c "\" id=\"";
       html_escaped_string c ref;
       C.string c "\" role=\"doc-noteref\">";
       C.inline c (Inline.Link.text l);
       C.string c "</a>")
     else (
       C.string c "<sup><a href=\"#";
       pct_encoded_string c label;
       C.string c "\" id=\"";
       html_escaped_string c ref;
       C.string c "\" role=\"doc-noteref\" className=\"fn-label\">";
       C.string c text;
       C.string c "</a></sup>")

   let link c l =
     match Inline.Link.reference_definition (C.get_defs c) l with
     | Some (Link_definition.Def (ld, _)) ->
         let link, title = link_dest_and_title c ld in
         C.string c "<a href=\"";
         pct_encoded_string c link;
         if title <> "" then (
           C.string c "\" title=\"";
           html_escaped_string c title);
         C.string c "\">";
         C.inline c (Inline.Link.text l);
         C.string c "</a>"
     | Some (Block.Footnote.Def (fn, _)) -> link_footnote c l fn
     | None ->
         C.inline c (Inline.Link.text l);
         comment_undefined_label c l
     | Some _ ->
         C.inline c (Inline.Link.text l);
         comment_unknown_def_type c l

   let raw_html c h =
     if safe c then comment c "CommonMark raw HTML omitted"
     else
       let line c (_, (h, _)) =
         C.byte c '\n';
         C.string c h
       in
       if h <> [] then (
         C.string c (fst (snd (List.hd h)));
         List.iter (line c) (List.tl h))

   let strikethrough c s =
     C.string c "<del>";
     C.inline c (Inline.Strikethrough.inline s);
     C.string c "</del>"

   let math_span c ms =
     let tex_line c l = html_escaped_string c (Block_line.tight_to_string l) in
     let tex_lines c = function
       (* newlines only between lines *)
       | [] -> ()
       | l :: ls ->
           let line c l =
             C.byte c '\n';
             tex_line c l
           in
           tex_line c l;
           List.iter (line c) ls
     in
     let tex = Inline.Math_span.tex_layout ms in
     if tex = [] then ()
     else (
       C.string c (if Inline.Math_span.display ms then "\\[" else "\\(");
       tex_lines c tex;
       C.string c (if Inline.Math_span.display ms then "\\]" else "\\)"))

   let inline c = function
     | Inline.Autolink (a, _) ->
         autolink c a;
         true
     | Inline.Break (b, _) ->
         break c b;
         true
     | Inline.Code_span (cs, _) ->
         code_span c cs;
         true
     | Inline.Emphasis (e, _) ->
         emphasis c e;
         true
     | Inline.Image (i, _) ->
         image c i;
         true
     | Inline.Inlines (is, _) ->
         List.iter (C.inline c) is;
         true
     | Inline.Link (l, _) ->
         link c l;
         true
     | Inline.Raw_html (html, _) ->
         raw_html c html;
         true
     | Inline.Strong_emphasis (e, _) ->
         strong_emphasis c e;
         true
     | Inline.Text (t, _) ->
         html_escaped_string c t;
         true
     | Inline.Ext_strikethrough (s, _) ->
         strikethrough c s;
         true
     | Inline.Ext_math_span (ms, _) ->
         math_span c ms;
         true
     | _ ->
         comment c "<!-- Unknown Cmarkit inline -->";
         true

   (* Block rendering *)

   let block_quote c bq =
     C.string c "<blockquote>\n";
     C.block c (Block.Block_quote.block bq);
     C.string c "</blockquote>\n"

   let code_block c cb =
     let i = Option.map fst (Block.Code_block.info_string cb) in
     let lang = Option.bind i Block.Code_block.language_of_info_string in
     let line (l, _) =
       html_escaped_string c l;
       C.byte c '\n'
     in
     match lang with
     | Some (lang, _env) when backend_blocks c && lang.[0] = '=' ->
         if lang = "=html" && not (safe c) then
           block_lines c (Block.Code_block.code cb)
         else ()
     | _ ->
         C.string c "<pre><code";
         (match lang with
         | None -> ()
         | Some (lang, _env) ->
             C.string c " className=\"language-";
             html_escaped_string c lang;
             C.byte c '\"');
         C.byte c '>';
         List.iter line (Block.Code_block.code cb);
         C.string c "</code></pre>\n"

   let heading c h =
     let level = string_of_int (Block.Heading.level h) in
     C.string c "<h";
     C.string c level;
     (match Block.Heading.id h with
     | None -> C.byte c '>'
     | Some (`Auto id | `Id id) ->
         let id = unique_id c id in
         C.string c " id=\"";
         C.string c id;
         C.string c "\"><a className=\"anchor\" aria-hidden=\"true\" href=\"#";
         C.string c id;
         C.string c "\"></a>");
     C.inline c (Block.Heading.inline h);
     C.string c "</h";
     C.string c level;
     C.string c ">\n"

   let paragraph c p =
     C.string c "<p>";
     C.inline c (Block.Paragraph.inline p);
     C.string c "</p>\n"

   let item_block ~tight c = function
     | Block.Blank_line _ -> ()
     | Block.Paragraph (p, _) when tight -> C.inline c (Block.Paragraph.inline p)
     | Block.Blocks (bs, _) ->
         let rec loop c add_nl = function
           | Block.Blank_line _ :: bs -> loop c add_nl bs
           | Block.Paragraph (p, _) :: bs when tight ->
               C.inline c (Block.Paragraph.inline p);
               loop c true bs
           | b :: bs ->
               if add_nl then C.byte c '\n';
               C.block c b;
               loop c false bs
           | [] -> ()
         in
         loop c true bs
     | b ->
         C.byte c '\n';
         C.block c b

   let list_item ~tight c (i, _) =
     match Block.List_item.ext_task_marker i with
     | None ->
         C.string c "<li>";
         item_block ~tight c (Block.List_item.block i);
         C.string c "</li>\n"
     | Some (mark, _) ->
         C.string c "<li>";
         let close =
           match Block.List_item.task_status_of_task_marker mark with
           | `Unchecked ->
               C.string c
                 "<div className=\"task\"><input type=\"checkbox\" disabled><div>";
               "</div></div></li>\n"
           | `Checked | `Other _ ->
               C.string c
                 "<div className=\"task\"><input type=\"checkbox\" disabled \
                  checked><div>";
               "</div></div></li>\n"
           | `Cancelled ->
               C.string c
                 "<div className=\"task\"><input type=\"checkbox\" disabled><del>";
               "</del></div></li>\n"
         in
         item_block ~tight c (Block.List_item.block i);
         C.string c close

   let list c l =
     let tight = Block.List'.tight l in
     match Block.List'.type' l with
     | `Unordered _ ->
         C.string c "<ul>\n";
         List.iter (list_item ~tight c) (Block.List'.items l);
         C.string c "</ul>\n"
     | `Ordered (start, _) ->
         C.string c "<ol";
         if start = 1 then C.string c ">\n"
         else (
           C.string c " start=\"";
           C.string c (string_of_int start);
           C.string c "\">\n");
         List.iter (list_item ~tight c) (Block.List'.items l);
         C.string c "</ol>\n"

   let html_block c lines =
     let line (l, _) =
       C.string c l;
       C.byte c '\n'
     in
     if safe c then (
       comment c "CommonMark HTML block omitted";
       C.byte c '\n')
     else List.iter line lines

   let thematic_break c = C.string c "<hr>\n"

   let math_block c cb =
     let line l =
       html_escaped_string c (Block_line.to_string l);
       C.byte c '\n'
     in
     C.string c "\\[\n";
     List.iter line (Block.Code_block.code cb);
     C.string c "\\]\n"

   let table c t =
     let start c align tag =
       C.byte c '<';
       C.string c tag;
       match align with
       | None -> C.byte c '>'
       | Some `Left -> C.string c " className=\"left\">"
       | Some `Center -> C.string c " className=\"center\">"
       | Some `Right -> C.string c " className=\"right\">"
     in
     let close c tag =
       C.string c "</";
       C.string c tag;
       C.string c ">\n"
     in
     let rec cols c tag ~align count cs =
       match (align, cs) with
       | (a, _) :: align, (col, _) :: cs ->
           start c (fst a) tag;
           C.inline c col;
           close c tag;
           cols c tag ~align (count - 1) cs
       | (a, _) :: align, [] ->
           start c (fst a) tag;
           close c tag;
           cols c tag ~align (count - 1) []
       | [], (col, _) :: cs ->
           start c None tag;
           C.inline c col;
           close c tag;
           cols c tag ~align:[] (count - 1) cs
       | [], [] ->
           for _i = count downto 1 do
             start c None tag;
             close c tag
           done
     in
     let row c tag ~align count cs =
       C.string c "<tr>\n";
       cols c tag ~align count cs;
       C.string c "</tr>\n"
     in
     let header c count ~align cols = row c "th" ~align count cols in
     let data c count ~align cols = row c "td" ~align count cols in
     let rec rows c col_count ~align = function
       | ((`Header cols, _), _) :: rs ->
           let align, rs =
             match rs with
             | ((`Sep align, _), _) :: rs -> (align, rs)
             | _ -> (align, rs)
           in
           header c col_count ~align cols;
           rows c col_count ~align rs
       | ((`Data cols, _), _) :: rs ->
           data c col_count ~align cols;
           rows c col_count ~align rs
       | ((`Sep align, _), _) :: rs -> rows c col_count ~align rs
       | [] -> ()
     in
     C.string c "<div role=\"region\"><table>\n";
     rows c (Block.Table.col_count t) ~align:[] (Block.Table.rows t);
     C.string c "</table></div>"

   let block c = function
     | Block.Block_quote (bq, _) ->
         block_quote c bq;
         true
     | Block.Blocks (bs, _) ->
         List.iter (C.block c) bs;
         true
     | Block.Code_block (cb, _) ->
         code_block c cb;
         true
     | Block.Heading (h, _) ->
         heading c h;
         true
     | Block.Html_block (h, _) ->
         html_block c h;
         true
     | Block.List (l, _) ->
         list c l;
         true
     | Block.Paragraph (p, _) ->
         paragraph c p;
         true
     | Block.Thematic_break (_, _) ->
         thematic_break c;
         true
     | Block.Ext_math_block (cb, _) ->
         math_block c cb;
         true
     | Block.Ext_table (t, _) ->
         table c t;
         true
     | Block.Blank_line _ | Block.Link_reference_definition _
     | Block.Ext_footnote_definition _ ->
         true
     | _ ->
         comment c "Unknown Cmarkit block";
         C.byte c '\n';
         true

   (* XHTML rendering *)

   let xhtml_block c = function
     | Block.Thematic_break _ ->
         C.string c "<hr />\n";
         true
     | b -> block c b

   let xhtml_inline c = function
     | Inline.Break (b, _) when Inline.Break.type' b = `Hard ->
         C.string c "<br />\n";
         true
     | Inline.Image (i, _) ->
         image ~close:" />" c i;
         true
     | i -> inline c i

   (* Document rendering *)

   let footnotes c fns =
     (* XXX we could do something about recursive footnotes and footnotes in
        footnotes here. *)
     let fns = Label.Map.fold (fun _ fn acc -> fn :: acc) fns [] in
     let fns = List.sort Stdlib.compare fns in
     let footnote c (_, id, refc, fn) =
       C.string c "<li id=\"";
       html_escaped_string c id;
       C.string c "\">\n";
       C.block c (Block.Footnote.block fn);
       C.string c "<span>";
       for r = 1 to !refc do
         C.string c "<a href=\"#";
         pct_encoded_string c (footnote_ref_id id r);
         C.string c "\" role=\"doc-backlink\" className=\"fn-label\">↩︎︎";
         if !refc > 1 then (
           C.string c "<sup>";
           C.string c (Int.to_string r);
           C.string c "</sup>");
         C.string c "</a>"
       done;
       C.string c "</span>";
       C.string c "</li>"
     in
     C.string c "<section role=\"doc-endnotes\"><ol>\n";
     List.iter (footnote c) fns;
     C.string c "</ol></section>\n"

   let doc c d =
     C.block c (Doc.block d);
     let st = C.State.get c state in
     if Label.Map.is_empty st.footnotes then () else footnotes c st.footnotes;
     true *)

(* Renderer *)

(*
  TODO: Add error handling with Textloc.t from Cmarkit

  type output =
  | Ok of React.element
  | Error of (Textloc.t * string)
  | Warning of (Textloc.t * string) *)

let rec block_to_element ~state block =
  let open Cmarkit in
  let open Block in
  match (block : Block.t) with
  | Blank_line (_blank_node, _meta) -> React.createElement "div" [] []
  | Block_quote (_block_quote, _meta) -> React.createElement "div" [] []
  | Blocks (blocks, _meta) ->
      let list =
        blocks
        |> List.map (fun block -> block_to_element ~state block)
        |> Array.of_list
      in
      React.Fragment (React.List list)
  | List _list -> React.createElement "div" [] []
  | Code_block (_code, _meta) -> React.createElement "code" [] []
  | Html_block (_html, _meta) -> React.createElement "div" [] []
  | Link_reference_definition (_link_def, _meta) ->
      React.createElement "div" [] []
  | Paragraph (paragraph, _meta) ->
      let inline = Paragraph.inline paragraph in
      React.createElement "p" [] [ inline_to_element ~state inline ]
  | Thematic_break (_thematic_break, _meta) -> React.createElement "div" [] []
  | Heading (heading, _meta) -> (
      let level = Heading.level heading in
      let inline = Heading.inline heading in
      let tag = "h" ^ Int.to_string level in
      match Heading.id heading with
      | None -> React.createElement tag [] [ inline_to_element ~state inline ]
      | Some (`Auto id | `Id id) ->
          let id = unique_id state id in
          React.createElement tag
            [ React.JSX.string "id" id ]
            [
              React.createElement "a"
                [
                  React.JSX.string "class" "anchor";
                  React.JSX.string "aria-hidden" "true";
                  React.JSX.string "href" ("#" ^ id);
                ]
                [];
            ])
  | _ -> assert false

and inline_to_element ~state inline =
  let open Cmarkit in
  let open Inline in
  match inline with
  | Text (text, _meta) -> React.string text
  | Autolink (autolink, _meta) ->
      let pre = if Autolink.is_email autolink then "mailto:" else "" in
      let url = pre ^ fst (Autolink.link autolink) in
      let url = if Link.is_unsafe url then "" else url in
      let content, _meta = Autolink.link autolink in
      React.createElement "a"
        [ React.JSX.string "href" (pct_encoded_string url) ]
        [ React.string (html_escaped_string content) ]
  | Break (break, _meta) -> (
      match Break.type' break with
      | `Hard -> React.createElement "br" [] []
      | `Soft -> (* Unsure about the ouput *) React.null)
  | Code_span (code_span, _meta) ->
      React.createElement "code" [] [ React.string (Code_span.code code_span) ]
  | Emphasis (emphasis, _meta) ->
      let inline = Emphasis.inline emphasis in
      React.createElement "em" [] [ inline_to_element ~state inline ]
  | Strong_emphasis (emphasis, _meta) ->
      let inline = Emphasis.inline emphasis in
      React.createElement "strong" [] [ inline_to_element ~state inline ]
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
              React.createElement "a"
                [ React.JSX.string "href" (pct_encoded_string href) ]
                [ inline_to_element ~state (Inline.Link.text link) ]
          | some_title ->
              React.createElement "a"
                [
                  React.JSX.string "href" (pct_encoded_string href);
                  React.JSX.string "title" (html_escaped_string some_title);
                ]
                [ inline_to_element ~state (Link.text link) ])
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
              React.createElement "img"
                [
                  React.JSX.string "src" (pct_encoded_string src);
                  React.JSX.string "alt" (plain_text alt);
                ]
                [ inline_to_element ~state (Inline.Link.text link) ]
          | some_title ->
              React.createElement "img"
                [
                  React.JSX.string "src" (pct_encoded_string src);
                  React.JSX.string "alt" (plain_text alt);
                  React.JSX.string "title" (html_escaped_string some_title);
                ]
                [ inline_to_element ~state (Link.text link) ])
      (* | Some (Block.Footnote.Def (fn, _)) -> link_footnote c l fn *)
      | Some (Block.Footnote.Def (_fn, _)) -> assert false
      | None ->
          inline_to_element ~state (Link.text link)
          (* comment_undefined_label c l *)
      | Some _ ->
          inline_to_element ~state (Link.text link)
          (* comment_unknown_def_type c l) *))
  | Raw_html (_raw_html, _meta) -> React.createElement "div" [] []
  | Ext_strikethrough (strikethrough, _meta) ->
      let inline = Strikethrough.inline strikethrough in
      React.createElement "del" [] [ inline_to_element ~state inline ]
  | Ext_math_span (math_span, _meta) ->
      let content = Math_span.tex math_span in
      React.createElement "span" [] [ React.string content ]
  | _ -> assert false

let of_doc ~safe:_ d =
  let blocks = Doc.block d in
  let defs = Doc.defs d in
  let state = State.make ~backend_blocks:false ~safe:true ~defs () in
  let element = block_to_element ~state blocks in
  element
