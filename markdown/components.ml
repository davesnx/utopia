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

let make ?p ?a ?blockquote ?ol ?ul ?pre ?hr ?br ?code ?em ?strong ?del
    ?math_span ?li ?div ?img ?h1 ?h2 ?h3 ?h4 ?h5 ?h6 () =
  {
    p = Option.value ~default:Elements.P.make p;
    a = Option.value ~default:Elements.A.make a;
    blockquote = Option.value ~default:Elements.Blockquote.make blockquote;
    ol = Option.value ~default:Elements.Ol.make ol;
    ul = Option.value ~default:Elements.Ul.make ul;
    pre = Option.value ~default:Elements.Pre.make pre;
    hr = Option.value ~default:Elements.Hr.make hr;
    br = Option.value ~default:Elements.Br.make br;
    code = Option.value ~default:Elements.Code.make code;
    em = Option.value ~default:Elements.Em.make em;
    strong = Option.value ~default:Elements.Strong.make strong;
    del = Option.value ~default:Elements.Del.make del;
    math_span = Option.value ~default:Elements.Math_span.make math_span;
    li = Option.value ~default:Elements.Li.make li;
    div = Option.value ~default:Elements.Div.make div;
    img = Option.value ~default:Elements.Img.make img;
    h1 = Option.value ~default:Elements.H1.make h1;
    h2 = Option.value ~default:Elements.H2.make h2;
    h3 = Option.value ~default:Elements.H3.make h3;
    h4 = Option.value ~default:Elements.H4.make h4;
    h5 = Option.value ~default:Elements.H5.make h5;
    h6 = Option.value ~default:Elements.H6.make h6;
  }
