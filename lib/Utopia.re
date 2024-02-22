module type Page = {
  let path: string;
  let make: (~key: string=?, unit) => React.element;
};

module Loader = Loader;
