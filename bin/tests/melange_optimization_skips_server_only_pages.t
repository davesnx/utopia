Server-only app routes (no [@react.client.component]) are excluded from melange:

  $ mkdir -p app/about app/settings _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > app/page.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("home")} </div>;
  > EOF
  $ cat > app/about/page.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("about")} </div>;
  > EOF
  $ cat > app/settings/page.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("settings")} </div>;
  > EOF
  $ utopia.compiler > /dev/null

Melange modules should NOT include any page modules (no client components):

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

No melange copy rules for pages:

  $ ! grep -qF '(deps ../app/page.re) (target Pages__Page.re)' _utopia/dune
  $ ! grep -qF '(deps ../app/about/page.re) (target Pages__About__Page.re)' _utopia/dune

Native library still includes all pages:

  $ grep -qF 'Pages__Page' _utopia/dune
  $ grep -qF 'Pages__About__Page' _utopia/dune
  $ grep -qF 'Pages__Settings__Page' _utopia/dune
