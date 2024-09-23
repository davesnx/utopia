/* Static API */

/* Utopia.Page.Make(
  {
    let loader = () => 3;
    let component = (~data) =>
      div(~children=[Html.h1(~children=[React.string("Home")], ())], ());
    ();
  },
);
*/

/* How nextJS works in page router + SSG */

let getInitialProps = () => DB.get(); /* -- SSG */
let getServerProps = () => DB.get(); /* -- app.get() express router */

let initialProps = getInitialProps();
<script id="__initialProps" type="application/json">
  {React.string(Js.Json.stringifyAny(initialProps))}
</script>;

let component = (~data) => {
  let (state, setState) = React.useState();
  let data = Js.Array.map(x => x + 1, data);
  div(~children=[Html.h1(~children=[React.string("Home")], ())], ());
};

let data = read_dara_from_dom();
REactDom.render(component(data), document.getElementById("root"));

/* remix */

let loader = () => DB.get();

let component = (~data) => {
  let (state, setState) = useState();
  let data = Js.Array.map(x => x + 1, data);
  div(~children=[Html.h1(~children=[React.string("Home")], ())], ());
};

/* next app router */

let onSubmit = () => {
  let data = Form.getValues(form);
};

let component = (~data) => {
  let data = RPC.get(DB.get());
  // timeouts ??
  // cache ??
  // Success(data) | Error `Timeout | Error `Cache | Error `Network
  let data = Js.Array.map(x => x + 1, data);
  <div>
  <Form form />
</div>;
};

module Form = {
  let%client make = (~form) => {
    <form>
      <input type="text" name="name" />
      <button type="submit">Submit</button>
    </form>;
  };
};
