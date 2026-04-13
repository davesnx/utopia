App routes with [@react.client.component] are included in melange via extraction:

  $ mkdir -p app/about _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > app/page.re <<'EOF'
  > module Widget = {
  >   [@react.client.component]
  >   [@react.component]
  >   let make = (~label: string) => {
  >     <div> {React.string(label)} </div>;
  >   };
  > };
  > [@react.component]
  > let make = () => <Widget label="hello" />;
  > EOF
  $ cat > app/about/page.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("about")} </div>;
  > EOF
  $ utopia.compiler > /dev/null

Melange modules include the client component page but not server-only page:

  $ cat _utopia/dune | tr -s ' \n' ' ' | grep -oP 'melange\.emit.*?modules \K[^)]+' | tr ' ' '\n' | sort
  Pages__Page
  ReactServerDOMEsbuild
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

Extraction rule uses --extract-client instead of cat:

  $ grep -qF 'run %{bin:utopia.compiler} --extract-client' _utopia/dune

Native library still includes all pages:

  $ cat _utopia/dune | tr -s ' \n' ' ' | grep -oP 'library.*?name pages_.*?modules \K[^)]+' | tr ' ' '\n' | sort
  Pages__About__Page
  Pages__Page
