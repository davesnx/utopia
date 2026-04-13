Lib modules are included transitively: if client code uses A and A uses B, both are included:

  $ mkdir -p app lib _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > lib/Helpers.re <<'EOF'
  > let greet = (name) => "Hello " ++ name;
  > EOF
  $ cat > lib/Formatter.re <<'EOF'
  > let format_greeting = (name) => Helpers.greet(name) ++ "!";
  > EOF
  $ cat > lib/Db.re <<'EOF'
  > let query = () => "server only db call";
  > EOF
  $ cat > app/page.re <<'EOF'
  > module Widget = {
  >   [@react.client.component]
  >   [@react.component]
  >   let make = (~name: string) => {
  >     <div> {React.string(Formatter.format_greeting(name))} </div>;
  >   };
  > };
  > let data = Db.query();
  > [@react.component]
  > let make = () => <Widget name=data />;
  > EOF
  $ utopia.compiler > /dev/null

Client code uses Formatter, which uses Helpers. Both should be in melange.
Db is only used by server code and should be excluded:

  $ cat _utopia/dune | tr -s ' \n' ' ' | grep -oP 'melange\.emit.*?modules \K[^)]+' | tr ' ' '\n' | sort
  Lib
  Lib__Formatter
  Lib__Helpers
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
