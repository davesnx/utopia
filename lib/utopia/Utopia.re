open! Melange_json.Primitives;

let make = Utopia_router.make;
let callServer = Utopia_call_server.callServer;
let currentUrl = Utopia_router.currentUrl;
let browserPath = Utopia_router.browserPath;
let respond = Utopia_server.respond;

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

type navigation_history = Utopia_router.navigation_history = | Push | Replace;

type navigation_freshness =
  Utopia_router.navigation_freshness = | Use_cache | Revalidate;

let navigation_history_to_json = Utopia_router.navigation_history_to_json;
let navigation_history_of_json = Utopia_router.navigation_history_of_json;
let navigation_freshness_to_json = Utopia_router.navigation_freshness_to_json;
let navigation_freshness_of_json = Utopia_router.navigation_freshness_of_json;

type router = {
  path: string,
  route: Route.t,
  navigate:
    (
      ~history: navigation_history=?,
      ~freshness: navigation_freshness=?,
      Route.t
    ) =>
    unit,
};

let useRouter = () => {
  let router = Utopia_router.useRouter();
  {
    path: router.path,
    route: router.route,
    navigate: router.navigate,
  };
};

module Router = {
  let currentUrl = Utopia_router.currentUrl;
  let browserPath = Utopia_router.browserPath;
  let useRouter = useRouter;

  module Boundary = Utopia_router_route;

  module Link = {
    [@react.client.component]
    let make =
        (
          ~to_: Route.t,
          ~history: navigation_history=Push,
          ~className: option(string)=?,
          ~children: React.element,
        ) =>
      <Utopia_router_link to_ history ?className>
        children
      </Utopia_router_link>;
  };
};

module PassThroughLayout = Utopia_router.PassThroughLayout;

module Markdown = Utopia_markdown_api;

module Metadata = {
  type t = Utopia_types.metadata;

  let make = Utopia_types.make_metadata;

  module Og_image = {
    type t = Utopia_types.og_image;
    let make = Utopia_types.make_og_image;
  };

  module Open_graph = {
    type t = Utopia_types.open_graph;
    let make = Utopia_types.make_open_graph;
  };

  module Twitter = {
    type t = Utopia_types.twitter;
    let make = Utopia_types.make_twitter;
  };

  module Robots = {
    type t = Utopia_types.robots;
    let make = Utopia_types.make_robots;
  };

  module Icon = {
    type t = Utopia_types.icon;
    let make = Utopia_types.make_icon;
  };
};

module Types = {
  type page_kind = Utopia_types.page_kind = | Code_page | Markdown_page;
  type param_kind =
    Utopia_types.param_kind = | Single | Catch_all | Optional_catch_all;
  type route_segment =
    Utopia_types.route_segment =
      | Static(string) | Param(string, param_kind);
  type metadata = Utopia_types.metadata;
  type og_image = Utopia_types.og_image;
  type open_graph = Utopia_types.open_graph;
  type twitter = Utopia_types.twitter;
  type robots = Utopia_types.robots;
  type icon = Utopia_types.icon;

  let kind_of_extension = Utopia_types.kind_of_extension;
  let string_of_kind = Utopia_types.string_of_kind;
  let parse_kind = Utopia_types.parse_kind;
  let string_of_param_kind = Utopia_types.string_of_param_kind;
  let parse_param_kind = Utopia_types.parse_param_kind;
  let empty_metadata = Utopia_types.empty_metadata;
};
