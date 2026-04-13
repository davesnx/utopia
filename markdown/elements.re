module P = {
  let make = (~className=?, ~children, ()) => <p ?className> children </p>;
};

module A = {
  type visibility =
    | Visible
    | Hidden;

  let make =
      (~title=?, ~className=?, ~visibility=Visible, ~href, ~children, ()) =>
    <a ariaHidden={visibility == Hidden} href ?className ?title> children </a>;
};

module Blockquote = {
  let make = (~className=?, ~children, ()) =>
    <blockquote ?className> children </blockquote>;
};

module Ol = {
  let make = (~className=?, ~start=?, ~children, ()) =>
    switch (start) {
    | None => <ol ?className> children </ol>
    | Some(start) => <ol ?className start> children </ol>
    };
};

module Ul = {
  let make = (~className=?, ~children, ()) => <ul ?className> children </ul>;
};

module Pre = {
  let make = (~className=?, ~children, ()) =>
    <pre ?className> children </pre>;
};

module Hr = {
  let make = (~className=?, ()) => <hr ?className />;
};

module Br = {
  let make = (~className=?, ()) => <br ?className />;
};

module Code = {
  let make = (~className=?, ~children, ()) =>
    <code ?className> children </code>;
};

module Em = {
  let make = (~className=?, ~children, ()) => <em ?className> children </em>;
};

module Strong = {
  let make = (~className=?, ~children, ()) =>
    <strong ?className> children </strong>;
};

module Del = {
  let make = (~className=?, ~children, ()) =>
    <del ?className> children </del>;
};

module Math_span = {
  let make = (~className=?, ~children, ()) =>
    <span ?className> children </span>;
};

module Li = {
  type marker =
    | Plain
    | Unchecked
    | Checked;

  let make = (~className=?, ~marker=Plain, ~children, ()) =>
    switch (marker) {
    | Plain => <li ?className> children </li>
    | Unchecked =>
      <li ?className>
        <div className="task"> <input type_="checkbox" disabled=true /> </div>
        children
      </li>
    | Checked =>
      <li ?className>
        <div className="task"> <input type_="checkbox" checked=true /> </div>
        children
      </li>
    };
};

module Div = {
  let make = (~className=?, ~children, ()) =>
    <div ?className> children </div>;
};

module Img = {
  let make = (~className=?, ~title=?, ~src, ~alt, ~children, ()) => {
    ignore(children);
    <img src alt ?className ?title />;
  };
};

module H1 = {
  let make = (~className=?, ~id=?, ~children, ()) =>
    <h1 ?id ?className> children </h1>;
};

module H2 = {
  let make = (~className=?, ~id=?, ~children, ()) =>
    <h2 ?id ?className> children </h2>;
};

module H3 = {
  let make = (~className=?, ~id=?, ~children, ()) =>
    <h3 ?id ?className> children </h3>;
};

module H4 = {
  let make = (~className=?, ~id=?, ~children, ()) =>
    <h4 ?id ?className> children </h4>;
};

module H5 = {
  let make = (~className=?, ~id=?, ~children, ()) =>
    <h5 ?id ?className> children </h5>;
};

module H6 = {
  let make = (~className=?, ~id=?, ~children, ()) =>
    <h6 ?id ?className> children </h6>;
};

module Table = {
  let make = (~className=?, ~children, ()) =>
    <table ?className> children </table>;
};

module Thead = {
  let make = (~className=?, ~children, ()) =>
    <thead ?className> children </thead>;
};

module Tbody = {
  let make = (~className=?, ~children, ()) =>
    <tbody ?className> children </tbody>;
};

module Tr = {
  let make = (~className=?, ~children, ()) => <tr ?className> children </tr>;
};

module Th = {
  let make = (~className=?, ~children, ()) => <th ?className> children </th>;
};

module Td = {
  let make = (~className=?, ~children, ()) => <td ?className> children </td>;
};

module Footnotes_section = {
  let make = (~className=?, ~children, ()) => {
    let className =
      switch (className) {
      | Some(value) => value
      | None => "footnotes"
      };
    <section className> children </section>;
  };
};

module Footnotes_list = {
  let make = (~className=?, ~children, ()) => <ol ?className> children </ol>;
};

module Footnotes_item = {
  let make = (~className=?, ~id, ~children, ()) =>
    <li id ?className> children </li>;
};

module Footnote_ref = {
  let make = (~className=?, ~href, ~id, ~children, ()) =>
    <sup> <a href id ?className role="doc-noteref"> children </a> </sup>;
};

module Footnote_backref = {
  let make = (~className=?, ~href, ~children, ()) =>
    <a href ?className role="doc-backlink"> children </a>;
};
