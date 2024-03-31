Utopia.register(
  ~path="home",
  ~loader=() => "World",
  data => <div> {React.string("Hello " ++ data)} </div>,
);

Utopia.page(~path="index", () => <div> {React.string("Static page")} </div>);
