module Page = {
  let path = "index";

  [@react.component]
  let make = () => {
    <div> {React.string("Hello")} </div>;
  };
};

Utopia.Router.register((module Page));
