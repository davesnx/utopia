type t = {
  p : ?className:string -> children:React.element -> unit -> React.element;
  a :
    ?title:string ->
    ?className:string ->
    ?visibility:Elements.A.visibility ->
    href:string ->
    children:React.element ->
    unit ->
    React.element;
  blockquote :
    ?className:string -> children:React.element -> unit -> React.element;
  ol :
    ?className:string ->
    ?start:int ->
    children:React.element ->
    unit ->
    React.element;
  ul : ?className:string -> children:React.element -> unit -> React.element;
  pre : ?className:string -> children:React.element -> unit -> React.element;
  hr : ?className:string -> unit -> React.element;
  br : ?className:string -> unit -> React.element;
  code : ?className:string -> children:React.element -> unit -> React.element;
  em : ?className:string -> children:React.element -> unit -> React.element;
  strong : ?className:string -> children:React.element -> unit -> React.element;
  del : ?className:string -> children:React.element -> unit -> React.element;
  math_span :
    ?className:string -> children:React.element -> unit -> React.element;
  li :
    ?className:string ->
    ?marker:Elements.Li.marker ->
    children:React.element ->
    unit ->
    React.element;
  div : ?className:string -> children:React.element -> unit -> React.element;
  img :
    ?className:string ->
    ?title:string ->
    src:string ->
    alt:string ->
    children:React.element ->
    unit ->
    React.element;
  h1 :
    ?className:string ->
    ?id:string ->
    children:React.element ->
    unit ->
    React.element;
  h2 :
    ?className:string ->
    ?id:string ->
    children:React.element ->
    unit ->
    React.element;
  h3 :
    ?className:string ->
    ?id:string ->
    children:React.element ->
    unit ->
    React.element;
  h4 :
    ?className:string ->
    ?id:string ->
    children:React.element ->
    unit ->
    React.element;
  h5 :
    ?className:string ->
    ?id:string ->
    children:React.element ->
    unit ->
    React.element;
  h6 :
    ?className:string ->
    ?id:string ->
    children:React.element ->
    unit ->
    React.element;
  table : ?className:string -> children:React.element -> unit -> React.element;
  thead : ?className:string -> children:React.element -> unit -> React.element;
  tbody : ?className:string -> children:React.element -> unit -> React.element;
  tr : ?className:string -> children:React.element -> unit -> React.element;
  th : ?className:string -> children:React.element -> unit -> React.element;
  td : ?className:string -> children:React.element -> unit -> React.element;
  footnotes_section :
    ?className:string -> children:React.element -> unit -> React.element;
  footnotes_list :
    ?className:string -> children:React.element -> unit -> React.element;
  footnotes_item :
    ?className:string ->
    id:string ->
    children:React.element ->
    unit ->
    React.element;
  footnote_ref :
    ?className:string ->
    href:string ->
    id:string ->
    children:React.element ->
    unit ->
    React.element;
  footnote_backref :
    ?className:string ->
    href:string ->
    children:React.element ->
    unit ->
    React.element;
}

let make ?(p = Elements.P.make) ?(a = Elements.A.make)
    ?(blockquote = Elements.Blockquote.make)
    ?(math_span = Elements.Math_span.make) ?(ol = Elements.Ol.make)
    ?(ul = Elements.Ul.make) ?(pre = Elements.Pre.make) ?(hr = Elements.Hr.make)
    ?(br = Elements.Br.make) ?(code = Elements.Code.make)
    ?(em = Elements.Em.make) ?(strong = Elements.Strong.make)
    ?(del = Elements.Del.make) ?(li = Elements.Li.make)
    ?(div = Elements.Div.make) ?(img = Elements.Img.make)
    ?(h1 = Elements.H1.make) ?(h2 = Elements.H2.make) ?(h3 = Elements.H3.make)
    ?(h4 = Elements.H4.make) ?(h5 = Elements.H5.make) ?(h6 = Elements.H6.make)
    ?(table = Elements.Table.make) ?(thead = Elements.Thead.make)
    ?(tbody = Elements.Tbody.make) ?(tr = Elements.Tr.make)
    ?(th = Elements.Th.make) ?(td = Elements.Td.make)
    ?(footnotes_section = Elements.Footnotes_section.make)
    ?(footnotes_list = Elements.Footnotes_list.make)
    ?(footnotes_item = Elements.Footnotes_item.make)
    ?(footnote_ref = Elements.Footnote_ref.make)
    ?(footnote_backref = Elements.Footnote_backref.make) () =
  {
    p;
    a;
    blockquote;
    math_span;
    ol;
    ul;
    pre;
    hr;
    br;
    code;
    em;
    strong;
    del;
    li;
    div;
    img;
    h1;
    h2;
    h3;
    h4;
    h5;
    h6;
    table;
    thead;
    tbody;
    tr;
    th;
    td;
    footnotes_section;
    footnotes_list;
    footnotes_item;
    footnote_ref;
    footnote_backref;
  }
