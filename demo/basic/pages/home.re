let path = "index";
let loader = () => 0;
let component = (~data) =>
  <div> <h1> {React.string("sanchooo: " ++ data)} </h1> </div>;
