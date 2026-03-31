  $ mkdir -p pages/about/boo _utopia
  $ touch _utopia/dune
  $ printf "let layout = ()\n" > pages/layout.re
  $ printf "let page = ()\n" > pages/about/Team.re
  $ printf "let page = ()\n" > pages/about/boo/index.re
  $ utopia.compiler > /dev/null
  $ python3 - <<'PY'
  > from pathlib import Path
  > text = " ".join(Path("_utopia/dune").read_text().split())
  > checks = [
  >     "target Utopia_page__About__Team.re",
  >     "target Utopia_page__About__Boo__Index.re",
  >     "target Utopia_page__Layout.re",
  >     "modules Utopia_page__About__Team Utopia_page__About__Boo__Index Utopia_page__Layout Utopia_routes Utopia Utopia_route Utopia_types ReactServerDOMEsbuild Utopia_router Utopia_router_route Utopia_router_link client_entry_melange",
  >     "modules FunctionReferences Utopia Utopia_route Utopia_types Utopia_router Utopia_router_route Utopia_router_link Utopia_routes Utopia_page__About__Team Utopia_page__About__Boo__Index Utopia_page__Layout",
  > ]
  > missing = [needle for needle in checks if needle not in text]
  > if missing:
  >     raise SystemExit("\n".join(missing))
  > PY
