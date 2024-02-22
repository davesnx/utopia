module Page = {
  let path = "index";

  [@react.component]
  let make = () => {
    <div> {React.string("index")} </div>;
  };
};

Utopia.Loader.push((module Page));
