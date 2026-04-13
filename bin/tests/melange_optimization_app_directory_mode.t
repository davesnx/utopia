Optimization works with app/ directory mode:

  $ mkdir -p app/about app/dashboard lib _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > lib/Ui.re <<'EOF'
  > let button_class = "px-4 py-2 bg-blue";
  > EOF
  $ cat > app/page.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("home")} </div>;
  > EOF
  $ cat > app/about/page.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("about")} </div>;
  > EOF
  $ cat > app/dashboard/page.re <<'EOF'
  > module Toggle = {
  >   [@react.client.component]
  >   [@react.component]
  >   let make = (~on: bool) => {
  >     <button className={Ui.button_class}>
  >       {React.string(on ? "ON" : "OFF")}
  >     </button>;
  >   };
  > };
  > [@react.component]
  > let make = () => <Toggle on=true />;
  > EOF
  $ utopia.compiler > /dev/null

Only dashboard page (with client component) is in melange,
alongside the current runtime/router support modules:

  $ cat _utopia/dune | tr -s ' \n' ' ' | grep -oP 'melange\.emit.*?modules \K[^)]+' | tr ' ' '\n' | sort
  Lib
  Lib__Ui
  Pages__Dashboard__Page
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

Only one extraction rule (for dashboard page with client component):

  $ grep -c 'extract-client' _utopia/dune
  1

Lib module Ui is included because client component references it:

  $ grep -qF 'Lib__Ui' _utopia/dune
