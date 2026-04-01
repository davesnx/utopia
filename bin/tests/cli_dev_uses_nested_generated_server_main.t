  $ mkdir -p demo/notes/pages demo/notes/lib
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(dirs :standard demo)\n" > dune
  $ printf "(dirs :standard _utopia)\n" > demo/notes/dune
  $ cat > demo/notes/lib/Greeting.re <<'EOF'
  > let label = "nested lib ready";
  > let message = name => "Hello nested dev " ++ name;
  > EOF
  $ cat > demo/notes/pages/Home.re <<'EOF'
  > [@react.server.function]
  > let greet = (~name: string): Js.Promise.t(string) =>
  >   Js.Promise.resolve(Greeting.message(name));
  > 
  > [@react.component]
  > let make = () => <div> {React.string(Greeting.label)} </div>;
  > EOF
  $ (cd demo/notes && utopia.compiler > /dev/null)
  $ dune describe pp demo/notes/_utopia/native/Utopia_page__Home.re > native.pp
  $ action_id=$(grep -oP 'Runtime\.id: "\K[^"]+' native.pp | head -1)
  $ cd demo/notes
  $ PORT=8114 HOST=127.0.0.1 NO_LOG=1 utopia dev --no-watch > dev.log 2>&1 &
  $ dev_pid=$!
  $ curl -s --retry 10 --retry-connrefused --retry-delay 1 http://127.0.0.1:8114/home | grep -qF 'nested lib ready'
  $ curl -i -s --retry 10 --retry-connrefused --retry-delay 1 -X POST -H 'Accept: application/react.action' -H 'Content-Type: text/plain;charset=utf-8' -H "X-Action-ID: $action_id" --data '["Alice"]' http://127.0.0.1:8114/home | rg 'HTTP/1.1 200 OK|Content-Type: application/react.action|^0:"Hello nested dev Alice"$'
  HTTP/1.1 200 OK
  Content-Type: application/react.action
  0:"Hello nested dev Alice"
  $ kill $dev_pid
  $ wait $dev_pid || true
