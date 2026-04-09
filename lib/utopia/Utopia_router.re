open! Melange_json.Primitives;

exception NoProvider(string);

module DOM = Webapi.Dom;
module Location = DOM.Location;
module History = DOM.History;

[@platform js]
module HistoryState = {
  type t = History.state;
  let empty: t = Obj.magic(Js.Dict.empty());
};

module HistoryCache = {
  type page =
    | FullPage(React.element)
    | DiffPage(string, React.element);

  type t = {
    cache: Hashtbl.t(string, page),
    keyQueue: Queue.t(string),
    maxSize: int,
  };

  let create = (~maxSize=16, ()) => {
    cache: Hashtbl.create(maxSize),
    keyQueue: Queue.create(),
    maxSize,
  };

  let set = (t, ~key, ~page) => {
    if (!Hashtbl.mem(t.cache, key)) {
      if (Queue.length(t.keyQueue) >= t.maxSize) {
        let oldestKey = Queue.take(t.keyQueue);
        Hashtbl.remove(t.cache, oldestKey);
      };
      Queue.add(key, t.keyQueue);
    };
    Hashtbl.replace(t.cache, key, page);
  };

  let get = (t, ~key) => Hashtbl.find_opt(t.cache, key);
};

let historyCache = HistoryCache.create();

module VirtualHistory = {
  type route = {
    path: string,
    renderPage: React.element => unit,
  };

  let state = ref([]);

  let push = (~path, ~renderPage) => {
    let filteredRoutes = List.filter(route => route.path != path, state^);
    state :=
      filteredRoutes
      @ [
        {
          path,
          renderPage,
        },
      ];
  };

  let find = path => List.find_opt(route => route.path == path, state^);

  let cleanup = () => {
    state := [];
  };

  let cleanPathState = path => {
    state :=
      List.filter(
        route => String.length(route.path) <= String.length(path),
        state^,
      );
  };
};

type navigation_history =
  | Push
  | Replace;

type navigation_freshness =
  | Use_cache
  | Revalidate;

let navigation_history_to_json = history =>
  switch (history) {
  | Push => string_to_json("Push")
  | Replace => string_to_json("Replace")
  };

let navigation_history_of_json = json =>
  switch (string_of_json(json)) {
  | "Push" => Push
  | "Replace" => Replace
  | _ => failwith("Invalid navigation_history JSON")
  };

let navigation_freshness_to_json = freshness =>
  switch (freshness) {
  | Use_cache => string_to_json("Use_cache")
  | Revalidate => string_to_json("Revalidate")
  };

let navigation_freshness_of_json = json =>
  switch (string_of_json(json)) {
  | "Use_cache" => Use_cache
  | "Revalidate" => Revalidate
  | _ => failwith("Invalid navigation_freshness JSON")
  };

type t = {
  path: string,
  route: Utopia_route.t,
  navigate:
    (
      ~history: navigation_history=?,
      ~freshness: navigation_freshness=?,
      Utopia_route.t
    ) =>
    unit,
};

let context: React.Context.t(option(t)) = React.createContext(None);
let provider = React.Context.provider(context);

let useRouter = () =>
  switch (React.useContext(context)) {
  | Some(router) => router
  | None => raise(NoProvider("Utopia.useRouter() requires <Utopia />"))
  };

let browserWindow = () =>
  switch%platform () {
  | Client => [%mel.raw "window"]
  | Server => failwith("window isn't supported on the server")
  };

let browserHistory = () =>
  switch%platform () {
  | Client => DOM.Window.history(browserWindow())
  | Server => failwith("history isn't supported on the server")
  };

let browserEventTarget = () =>
  switch%platform () {
  | Client => DOM.Window.asEventTarget(browserWindow())
  | Server => failwith("window events aren't supported on the server")
  };

let currentUrl = () =>
  switch%platform () {
  | Client =>
    URL.makeExn(Location.href(DOM.Window.location(browserWindow())))
  | Server => failwith("currentUrl isn't supported on the server")
  };

[@warning "-27"]
let findRouteAnchor = event =>
  switch%platform () {
  | Client => [%mel.raw
     {js|
      (function(event) {
        if (
          event.defaultPrevented ||
          event.button !== 0 ||
          event.metaKey ||
          event.ctrlKey ||
          event.shiftKey ||
          event.altKey
        ) {
          return undefined;
        }

        var target = event.target;
        if (!target || typeof target.closest !== "function") {
          return undefined;
        }

        var anchor = target.closest("a.js-route-link");
        if (!anchor) {
          return undefined;
        }

        if (anchor.target && anchor.target !== "_self") {
          return undefined;
        }

        if (anchor.hasAttribute("download")) {
          return undefined;
        }

        return anchor;
      })(event)
    |js}
    ]
  | Server => None
  };

let requestPath = url =>
  URL.pathname(url) ++ (URL.search(url) |> Option.value(~default=""));

let browserPath = url =>
  requestPath(url) ++ (URL.hash(url) |> Option.value(~default=""));

let callServer = Utopia_call_server.callServer;

module PassThroughLayout = {
  [@react.component]
  let make = (~children) => children;
};

type navigationResult = (string, string, React.element);

[@react.client.component]
let make = (~initialPath: string, ~children: React.element) => {
  let initialRoute = Utopia_route.of_href(initialPath);
  let (element, setElement) = React.useState(() => children);
  let (route, setRoute) = React.useState(() => initialRoute);
  let (path, setPath) =
    React.useState(() => Utopia_route.request_path(initialRoute));
  let (cachedNodeKey, setCachedNodeKey) =
    React.useState(() => Utopia_route.request_path(initialRoute));

  let renderFullPage = page => {
    setCachedNodeKey(_ => Js.Date.now() |> string_of_float);
    setElement(_ => page);
    VirtualHistory.cleanup();
    true;
  };

  let renderDiffPage = (~parentRoute, page) =>
    switch (VirtualHistory.find(parentRoute)) {
    | Some(route) =>
      VirtualHistory.cleanPathState(route.path);
      route.renderPage(page);
      true;
    | None => false
    };

  let%browser_only reportNavigationError = (targetPath, message) => {
    ignore(
      [%mel.raw
        {|
      (function() {
        if (typeof window !== 'undefined' && window.__utopia_dev_report_error) {
          window.__utopia_dev_report_error({
            operation: 'navigation',
            message: message,
            stack: null,
            context: 'to: ' + targetPath
          });
        }
      })()
    |}
      ],
    );
  };

  let%browser_only fetchNavigation = (~currentPath: option(string), nextPath) => {
    let headers =
      switch (currentPath) {
      | Some(currentPath) =>
        Fetch.HeadersInit.makeWithArray([|
          ("Accept", "application/react.component"),
          ("X-Utopia-Current-Path", currentPath),
        |])
      | None =>
        Fetch.HeadersInit.makeWithArray([|
          ("Accept", "application/react.component"),
        |])
      };
    let promise =
      Fetch.fetchWithInit(
        nextPath,
        Fetch.RequestInit.make(~method_=Get, ~headers, ()),
      );
    ReactServerDOMEsbuild.createFromFetch(~callServer, promise);
  };

  let%browser_only rec navigate =
                       (
                         ~history as navigationHistory=Push,
                         ~freshness=Use_cache,
                         to_,
                       ) => {
    let shouldReplace =
      switch (navigationHistory) {
      | Replace => true
      | Push => false
      };
    let shouldRevalidate =
      switch (freshness) {
      | Revalidate => true
      | Use_cache => false
      };
    let current = currentUrl();
    let currentBrowserPath = browserPath(current);
    let currentRequestPath = requestPath(current);
    let nextRequestPath = Utopia_route.request_path(to_);
    let nextBrowserPath = Utopia_route.href(to_);
    let shouldRequestDiff =
      !shouldRevalidate
      && Utopia_route.pathname(to_) != URL.pathname(current);

    if (nextBrowserPath == currentBrowserPath && !shouldRevalidate) {
      ();
    } else if (nextRequestPath == currentRequestPath && !shouldRevalidate) {
      if (shouldReplace) {
        History.replaceState(
          HistoryState.empty,
          "",
          nextBrowserPath,
          browserHistory(),
        );
      } else {
        History.pushState(
          HistoryState.empty,
          "",
          nextBrowserPath,
          browserHistory(),
        );
      };
      setRoute(_ => to_);
      setPath(_ => nextRequestPath);
    } else {
      let diffFrom = shouldRequestDiff ? Some(currentRequestPath) : None;
      let promise = fetchNavigation(~currentPath=diffFrom, nextRequestPath);
      let _ =
        promise
        |> Js.Promise.then_(
             ((mode, parentRoute, nextElement): navigationResult) => {
             switch (mode) {
             | "diff" =>
               if (renderDiffPage(~parentRoute, nextElement)) {
                 HistoryCache.set(
                   historyCache,
                   ~key=nextRequestPath,
                   ~page=HistoryCache.DiffPage(parentRoute, nextElement),
                 );
                 if (shouldReplace) {
                   History.replaceState(
                     HistoryState.empty,
                     "",
                     nextBrowserPath,
                     browserHistory(),
                   );
                 } else {
                   History.pushState(
                     HistoryState.empty,
                     "",
                     nextBrowserPath,
                     browserHistory(),
                   );
                 };
                 setRoute(_ => to_);
                 setPath(_ => nextRequestPath);
                 Js.Promise.resolve();
               } else {
                 navigate(~history=Replace, ~freshness=Revalidate, to_);
                 Js.Promise.resolve();
               }
             | _ =>
               HistoryCache.set(
                 historyCache,
                 ~key=nextRequestPath,
                 ~page=HistoryCache.FullPage(nextElement),
               );
               ignore(renderFullPage(nextElement));
               if (shouldReplace) {
                 History.replaceState(
                   HistoryState.empty,
                   "",
                   nextBrowserPath,
                   browserHistory(),
                 );
               } else {
                 History.pushState(
                   HistoryState.empty,
                   "",
                   nextBrowserPath,
                   browserHistory(),
                 );
               };
               setRoute(_ => to_);
               setPath(_ => nextRequestPath);
               Js.Promise.resolve();
             }
           })
        |> Js.Promise.catch(err => {
             let msg = [%mel.raw {| String(err && err.message ? err.message : err) |}];
             reportNavigationError(nextRequestPath, msg);
             Js.Promise.resolve();
           });
      ();
    };
  };

  React.useEffect0(() => {
    let initialRoute = currentUrl() |> browserPath |> Utopia_route.of_href;
    let initialRequestPath = Utopia_route.request_path(initialRoute);
    HistoryCache.set(
      historyCache,
      ~key=initialRequestPath,
      ~page=HistoryCache.FullPage(element),
    );
    History.replaceState(
      HistoryState.empty,
      "",
      Utopia_route.href(initialRoute),
      browserHistory(),
    );
    setRoute(_ => initialRoute);
    setPath(_ => initialRequestPath);
    None;
  });

  React.useEffect0(() => {
    let watcherId = _event => {
      let nextRoute = currentUrl() |> browserPath |> Utopia_route.of_href;
      let nextRequestPath = Utopia_route.request_path(nextRoute);
      setRoute(_ => nextRoute);
      setPath(_ => nextRequestPath);
      switch (HistoryCache.get(historyCache, ~key=nextRequestPath)) {
      | Some(HistoryCache.FullPage(page)) => ignore(renderFullPage(page))
      | Some(HistoryCache.DiffPage(parentRoute, page)) =>
        if (!renderDiffPage(~parentRoute, page)) {
          navigate(~history=Replace, ~freshness=Revalidate, nextRoute);
        }
      | None => navigate(~history=Replace, ~freshness=Revalidate, nextRoute)
      };
    };

    DOM.EventTarget.addEventListener(
      "popstate",
      watcherId,
      browserEventTarget(),
    );

    Some(
      () =>
        DOM.EventTarget.removeEventListener(
          "popstate",
          watcherId,
          browserEventTarget(),
        ),
    );
  });

  React.useEffect0(() => {
    let clickWatcher = event => {
      switch (findRouteAnchor(event)) {
      | Some(anchor) =>
        switch (DOM.Element.getAttribute("href", anchor)) {
        | Some(href) =>
          let current = currentUrl();
          let target = URL.makeWith(href, ~base=URL.toString(current));
          if (URL.origin(target) == URL.origin(current)) {
            DOM.Event.preventDefault(event);
            navigate(Utopia_route.of_href(browserPath(target)));
          };
        | None => ()
        }
      | None => ()
      };
    };

    DOM.EventTarget.addEventListener(
      "click",
      clickWatcher,
      browserEventTarget(),
    );

    Some(
      () =>
        DOM.EventTarget.removeEventListener(
          "click",
          clickWatcher,
          browserEventTarget(),
        ),
    );
  });

  <React.Fragment key=cachedNodeKey>
    {switch%platform (Runtime.platform) {
     | Client =>
       React.createElement(
         provider,
         {
           "value":
             Some({
               path,
               route,
               navigate,
             }),
           "children": element,
         },
       )
     | Server =>
       provider({
         "value":
           Some({
             path: Utopia_route.request_path(initialRoute),
             route: initialRoute,
             navigate: (~history=?, ~freshness=?, _) =>
               failwith("navigate isn't supported on the server"),
           }),
         "children": element,
       })
     }}
  </React.Fragment>;
};
