module Page = {
  let path = "blog";

  [@react.component]
  let make = () => {
    <div> {React.string("Blog")} </div>;
  };
};

Utopia.Loader.p := Some((module Page): (module Utopia.Loader.PLUG));
