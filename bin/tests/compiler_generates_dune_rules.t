  $ mkdir pages _utopia
  $ touch _utopia/dune
  $ printf "let page = ()\n" > pages/Home.re
  $ printf "# Hello\n" > pages/Guide.md
  $ utopia.compiler > /dev/null
  $ test -f _utopia/ReactServerDOMEsbuild.re
$ test -f _utopia/Utopia.re
$ test -f _utopia/Utopia_route.ml
$ test -f _utopia/Utopia_routes.ml
$ test -f _utopia/Utopia_server.ml
$ test -f _utopia/Utopia_types.ml
$ test -f _utopia/Utopia_router.re
$ test -f _utopia/Utopia_router_route.re
$ test -f _utopia/Utopia_router_link.re
$ test -f _utopia/client_entry.re
  $ test -f _utopia/native/FunctionReferences.re
  $ test -f _utopia/native/Utopia.re
  $ test -f _utopia/native/Utopia_route.ml
  $ test -f _utopia/native/Utopia_types.ml
  $ test -f _utopia/native/Utopia_router.re
  $ test -f _utopia/native/Utopia_router_route.re
  $ test -f _utopia/native/Utopia_router_link.re
  $ grep -qF '(rule (deps ../pages/Home.re) (target Utopia_page__Home.re)' _utopia/dune
  $ grep -qF '(rule (deps client_entry.re) (target client_entry_melange.re)' _utopia/dune
  $ grep -qF '(melange.emit (target target) (module_systems es6)' _utopia/dune
  $ grep -qF '(rule (deps ../pages/Guide.md) (target Guide.html)' _utopia/dune
  $ grep -qF '(subdir native' _utopia/dune
  $ grep -qF '(rule (deps ../Utopia_routes.ml) (target Utopia_routes.ml)' _utopia/dune
  $ grep -qF '(wrapped false)' _utopia/dune
  $ grep -qF '(modules server_main Utopia_server)' _utopia/dune
  $ grep -qF -- '-shared-folder-prefix=_utopia/' _utopia/dune
  $ grep -qF -- '-shared-folder-prefix=_utopia/native/' _utopia/dune
  $ ! grep -qF 'target ReactServerDOMEsbuild.re' _utopia/dune
  $ ! grep -qF 'target FunctionReferences.re' _utopia/dune
  $ ! grep -qF 'target Utopia.re' _utopia/dune
  $ ! grep -qF 'target Utopia_route.ml' _utopia/dune
  $ ! grep -qF 'target Utopia_server.ml' _utopia/dune
  $ ! grep -qF 'target Utopia_types.ml' _utopia/dune
  $ ! grep -qF 'target Utopia_router.re' _utopia/dune
  $ ! grep -qF 'target Utopia_router_route.re' _utopia/dune
  $ ! grep -qF 'target Utopia_router_link.re' _utopia/dune
