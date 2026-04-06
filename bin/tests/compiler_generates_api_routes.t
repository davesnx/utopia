  $ mkdir -p pages api/users _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ printf "let page = ()\n" > pages/Home.re
  $ printf "let middleware next request = next request\n" > api/_middleware.ml
  $ printf "let middleware next request = next request\n" > api/users/_middleware.ml
  $ printf "let handler (_request : Dream.request) = Utopia_server.respond \"{\\\"ok\\\":true}\"\n" > api/users/[id].ml
  $ utopia.compiler > /dev/null
  $ grep -F 'let get_all () : Utopia_types.page_route_meta list =' _utopia/Routes.ml
  let get_all () : Utopia_types.page_route_meta list =
  $ grep -F 'let get_all () : Utopia_types.api_route_meta list =' _utopia/Routes.ml
    let get_all () : Utopia_types.api_route_meta list =
  $ grep -F 'route = "api/users/[id]"; matcher = "api/users/:id"; conflict_key = "api/users/:";' _utopia/Routes.ml
      ({ route = "api/users/[id]"; matcher = "api/users/:id"; conflict_key = "api/users/:"; params = [("id", Utopia_types.Single)]; middlewares = ["api/_middleware.ml"; "api/users/_middleware.ml"]; source_file = "api/users/[id].ml"; module_name = "Api__Users__Id" } : Utopia_types.api_route_meta);
  $ grep -F 'let id (request : Dream.request) =' _utopia/Routes.ml
        let id (request : Dream.request) =
  $ grep -qF '(rule (deps ../../api/users/[id].ml) (target Api__Users__Id.ml)' _utopia/dune
  $ grep -qE '\(name api_' _utopia/dune
  $ ! test -f _utopia/routes.manifest
  $ ! test -f _utopia/api.manifest
