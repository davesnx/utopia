module type Page = {
  let path: string;
  let make: (~key: string=?, unit) => React.element;
};

module Index = {
  let path = "index";

  [@react.component]
  let make = () => {
    <div> {React.string("index")} </div>;
  };
};

module Blog = {
  let path = "blog";

  [@react.component]
  let make = () => {
    <div> {React.string("Blog")} </div>;
  };
};
let list_of_pages: list(module Page) = [(module Index), (module Blog)];
