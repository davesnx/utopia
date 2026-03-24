let path = "about";

type data = string;

let loader: unit => data = () => "0";

let component = (~data: data) =>
  <div> <h1> {React.string("sanchooo: " ++ data)} </h1> </div>;
