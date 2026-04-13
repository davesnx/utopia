[@react.component]
let make = () => {
  let label = switch%platform () {
  | Client => "client"
  | Server => "server"
  };
  <div> {React.string(label ++ string_of_int(Utils.value))} </div>;
};
