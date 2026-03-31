open! Melange_json.Primitives;

let context = React.createContext(React.null);

module PageConsumer = {
  [@react.client.component]
  let make = () => React.useContext(context);
};

module Provider = {
  let provider = React.Context.provider(context);

  [@react.client.component]
  let make = (~value: React.element, ~children: React.element) =>
    switch%platform (Runtime.platform) {
    | Client =>
      React.createElement(
        provider,
        {
          "value": value,
          "children": children,
        },
      )
    | Server => provider(~value, ~children, ())
    };
};

[@react.client.component]
let make =
    (
      ~path: string,
      ~layout: React.element,
      ~pageconsumer: option(React.element),
    ) => {
  let (pageconsumer, setPageConsumer) =
    React.useState(() => pageconsumer |> Option.value(~default=React.null));
  let isFirstRender = React.useRef(true);
  let (cachedNodeKey, setCachedNodeKey) = React.useState(() => path);

  let%browser_only renderPage = pageElement => {
    setPageConsumer(_ => pageElement);
    setCachedNodeKey(_ => Js.Date.now() |> string_of_float);
  };

  switch%platform (Runtime.platform) {
  | Client =>
    if (isFirstRender.current) {
      isFirstRender.current = false;
      Utopia_router.VirtualHistory.push(~path, ~renderPage);
    }
  | Server => ()
  };

  <Provider
    value={<React.Fragment key=cachedNodeKey> pageconsumer </React.Fragment>}>
    layout
  </Provider>;
};
