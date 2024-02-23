module Page = {
  let path = "blog";

  [@react.component]
  let make = () => {
    <div> {React.string("Blog")} </div>;
  };
};

Utopia.Router.register((module Page));
