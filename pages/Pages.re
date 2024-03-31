Utopia.register(
  ~path="home",
  ~loader=() => "World",
  data => <div> {React.string("Hello " ++ data)} </div>,
);

Utopia.page(~path="index", () => <div> {React.string("Static page")} </div>);

Array.make(10, "mock_page")
|> Array.iteri((index, fixture) => {
     Utopia.register(
       ~path=fixture ++ Int.to_string(index),
       ~loader=() => fixture,
       data =>
         <div> {React.string(data)} <h1> {React.int(index)} </h1> </div>,
     )
   });
