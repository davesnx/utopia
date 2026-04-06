  $ mkdir pages _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > pages/Home.re <<'EOF'
  > [@react.server.function]
  > let greet = (~name: string): Js.Promise.t(string) =>
  >   Js.Promise.resolve("Hello " ++ name);
  > 
  > [@react.component]
  > let make = () => <div> {React.string("home")} </div>;
  > EOF
  $ utopia.compiler > /dev/null
  $ dune describe pp _utopia/native/Pages__Home.re > native.pp
  $ action_id=$(grep -oP 'Runtime\.id: "\K[^"]+' native.pp | head -1)
  $ PORT=8111 HOST=127.0.0.1 NO_LOG=1 utopia dev --no-watch > dev.log 2>&1 &
  $ dev_pid=$!
  $ curl -i -s --retry 10 --retry-connrefused --retry-delay 1 -X POST -H 'Accept: application/react.action' -H 'Content-Type: text/plain;charset=utf-8' -H "X-Action-ID: $action_id" --data '["Dev Alice"]' http://127.0.0.1:8111/home | rg 'HTTP/1.1 200 OK|Content-Type: application/react.action|^0:"Hello Dev Alice"$'
  HTTP/1.1 200 OK
  Content-Type: application/react.action
  0:"Hello Dev Alice"
  $ kill $dev_pid
  $ wait $dev_pid || true
