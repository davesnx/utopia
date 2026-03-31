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
$ python3 - <<'PY'
> from pathlib import Path
> text = Path('_utopia/dune').read_text()
> for needle in [
>     '(rule (deps ../pages/Home.re) (target Utopia_page__Home.re)',
>     '(rule (deps client_entry.re) (target client_entry_melange.re)',
>     '(melange.emit (target target) (module_systems es6)',
>     '(rule (deps ../pages/Guide.md) (target Guide.html)',
>     '(subdir native',
>     '(rule (deps ../Utopia_routes.ml) (target Utopia_routes.ml)',
>     '(wrapped false)',
>     '(modules server_main Utopia_server Utopia_types)',
>     '-shared-folder-prefix=_utopia/',
>     '-shared-folder-prefix=_utopia/native/',
> ]:
>     assert needle in text, needle
> for needle in [
>     'target ReactServerDOMEsbuild.re',
>     'target FunctionReferences.re',
>     'target Utopia.re',
>     'target Utopia_route.ml',
>     'target Utopia_server.ml',
>     'target Utopia_types.ml',
>     'target Utopia_router.re',
>     'target Utopia_router_route.re',
>     'target Utopia_router_link.re',
> ]:
>     assert needle not in text, needle
> PY
