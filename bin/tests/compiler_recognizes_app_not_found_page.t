  $ mkdir -p app/about _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ printf "let page = ()\n" > app/page.re
  $ printf "let page = ()\n" > app/about/page.re
  $ cat > app/not-found.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("custom 404")} </div>;
  > EOF
  $ utopia.compiler > /dev/null

The not-found file should NOT be in Routes.ml as a page route:
  $ ! grep -qF 'source_file = "app/not-found.re";' _utopia/Routes.ml

Regular pages should still be in Routes.ml:
  $ grep -qF 'source_file = "app/page.re";' _utopia/Routes.ml
  $ grep -qF 'source_file = "app/about/page.re";' _utopia/Routes.ml

The not-found module should appear in the native library:
  $ grep -qF 'Pages__Not_found' _utopia/dune

The not-found page should NOT appear as an app-local alias:
  $ ! grep -qF 'module Not_found = Pages__Not_found' _utopia/dune

Routes_server should wire the not-found render function, and server_main should use Routes_server:
  $ grep -qF 'let not_found_page =' _utopia/Routes_server.ml
  $ grep -qF 'Generated_not_found_registry.make' _utopia/Routes_server.ml
  $ grep -qF 'Pages__Not_found.make' _utopia/Routes_server.ml
  $ grep -qF 'Routes_server' _utopia/server_main.ml
