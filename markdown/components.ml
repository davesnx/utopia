type t = {
  p : ?className:string -> children:React.element -> unit -> React.element;
  a :
    ?title:string ->
    ?className:string ->
    ?ariaHidden:bool ->
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
    ?disabled:bool ->
    ?checked:bool ->
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
    () =
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
  }
