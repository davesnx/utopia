module Page = {
  let path = "index";

  [@react.component]
  let make = () => {
    <div> {React.string("index")} </div>;
  };
};

Utopia.Loader.p := Some((module Page): (module Utopia.Loader.PLUG));
