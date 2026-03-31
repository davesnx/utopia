open! Melange_json.Primitives;

[@react.client.component]
let make =
    (
      ~to_: Utopia_route.t,
      ~replace: bool=false,
      ~className: option(string)=?,
      ~children: React.element,
    ) => {
  let router = Utopia_router.useRouter();
  let navigate = router.navigate;
  let href = Utopia_route.href(to_);

  let onClick = event => {
    let isPlainLeftClick =
      React.Event.Mouse.button(event) == 0
      && !React.Event.Mouse.metaKey(event)
      && !React.Event.Mouse.ctrlKey(event)
      && !React.Event.Mouse.shiftKey(event)
      && !React.Event.Mouse.altKey(event);

    if (isPlainLeftClick) {
      React.Event.Mouse.preventDefault(event);
      navigate(~replace, to_);
    };
  };

  switch (className) {
  | Some(className) => <a href className onClick> children </a>
  | None => <a href onClick> children </a>
  };
};
