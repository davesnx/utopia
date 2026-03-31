open! Melange_json.Primitives;

let make = Utopia_router.make;
let callServer = Utopia_router.callServer;
let currentUrl = Utopia_router.currentUrl;
let browserPath = Utopia_router.browserPath;

module Route = {
  type t = Utopia_route.t;

  module Query = {
    type t = Utopia_route.Query.t;
  };

  module Params = {
    type value =
      Utopia_route.Params.value = | One(string) | Many(list(string));
    type t = Utopia_route.Params.t;
    let one = Utopia_route.Params.one;
    let many = Utopia_route.Params.many;
    let find = Utopia_route.Params.find;
    let find_one = Utopia_route.Params.find_one;
    let find_many = Utopia_route.Params.find_many;
  };

  module Hash = {
    type t = Utopia_route.Hash.t;
  };

  module Nonempty = {
    type t('a) = Utopia_route.Nonempty.t('a);
    let make = Utopia_route.Nonempty.make;
    let of_list = Utopia_route.Nonempty.of_list;
    let to_list = Utopia_route.Nonempty.to_list;
  };

  let href = Utopia_route.href;
  let pathname = Utopia_route.pathname;
  let request_path = Utopia_route.request_path;
  let query_entries = Utopia_route.query_entries;
  let hash = Utopia_route.hash;
  let equal = Utopia_route.equal;
  let same_pathname = Utopia_route.same_pathname;
  let same_request_path = Utopia_route.same_request_path;
  let to_json = Utopia_route.to_json;
  let of_json = Utopia_route.of_json;
};

module Routes = Utopia_routes;

type router = {
  path: string,
  route: Route.t,
  current: option(Routes.Current.t),
  navigate: (~replace: bool=?, ~revalidate: bool=?, Route.t) => unit,
};

let useRouter = () => {
  let router = Utopia_router.useRouter();
  {
    path: router.path,
    route: router.route,
    current: Routes.current(router.route),
    navigate: router.navigate,
  };
};

module Router = {
  let currentUrl = Utopia_router.currentUrl;
  let browserPath = Utopia_router.browserPath;
  let useRouter = useRouter;

  module Boundary = Utopia_router_route;

  module Navigate = {
    [@react.client.component]
    let make = (~to_: Route.t, ~className: string, ~children: React.element) =>
      <Utopia_router_link to_ className> children </Utopia_router_link>;
  };
};

module PassThroughLayout = Utopia_router.PassThroughLayout;
