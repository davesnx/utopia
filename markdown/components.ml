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

module String = struct
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
