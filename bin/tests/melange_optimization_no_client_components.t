Projects with zero client components still have melange.emit (for runtime):

  $ mkdir -p app _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > app/page.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("home")} </div>;
  > EOF
  $ utopia.compiler > /dev/null

melange.emit is present with the current runtime/router modules,
but no page modules:

  $ grep -qF 'melange.emit' _utopia/dune
  $ cat _utopia/dune | tr -s ' \n' ' ' | grep -oP 'melange\.emit.*?modules \K[^)]+' | tr ' ' '\n' | sort
  React_server_dom_esbuild
  Routes
  Routes_client
  Utopia
  Utopia_call_server
  Utopia_dev_client
  Utopia_route
  Utopia_router
  Utopia_router_link
  Utopia_router_route
  Utopia_types
  client_entry_melange

No melange copy rules for pages (only native has the page):

  $ grep -c '(target Pages__Page.re)' _utopia/dune
  1
