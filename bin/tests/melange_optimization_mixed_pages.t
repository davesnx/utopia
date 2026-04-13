Mixed project: some pages with client components, some without.
Only client-component pages go to melange:

  $ mkdir -p app/about app/contact app/dashboard _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > app/page.re <<'EOF'
  > module Counter = {
  >   [@react.client.component]
  >   [@react.component]
  >   let make = (~initial: int) => {
  >     let (count, setCount) = React.useState(() => initial);
  >     <button onClick={_ => setCount(c => c + 1)}>
  >       {React.string(string_of_int(count))}
  >     </button>;
  >   };
  > };
  > [@react.component]
  > let make = () => <Counter initial=0 />;
  > EOF
  $ cat > app/about/page.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("about")} </div>;
  > EOF
  $ cat > app/contact/page.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("contact")} </div>;
  > EOF
  $ cat > app/dashboard/page.re <<'EOF'
  > module Toggle = {
  >   [@react.client.component]
  >   [@react.component]
  >   let make = (~on: bool) => {
  >     <span> {React.string(on ? "ON" : "OFF")} </span>;
  >   };
  > };
  > [@react.component]
  > let make = () => <Toggle on=true />;
  > EOF
  $ utopia.compiler > /dev/null

Only the root page and dashboard page (with client components) are in melange,
alongside the current runtime/router support modules:

  $ cat _utopia/dune | tr -s ' \n' ' ' | grep -oP 'melange\.emit.*?modules \K[^)]+' | tr ' ' '\n' | sort
  Pages__Dashboard__Page
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

About and Contact only appear in native subdir (not melange copy rules).
Native has all 4, melange has only 2 extraction rules:

  $ grep -c 'extract-client' _utopia/dune
  2

But all pages are in native library:

  $ cat _utopia/dune | tr -s ' \n' ' ' | grep -oP 'library.*?name pages_.*?modules \K[^)]+' | tr ' ' '\n' | sort
  Pages__About__Page
  Pages__Contact__Page
  Pages__Dashboard__Page
  Pages__Page

Extraction rules use --extract-client:

  $ grep -c 'extract-client' _utopia/dune
  2
