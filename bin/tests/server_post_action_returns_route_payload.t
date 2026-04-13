  $ mkdir -p app/home app/about _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > app/home/page.re <<'EOF'
  > let before = _request => ();
  > [@react.server.function]
  > let nextRoute = (): Js.Promise.t(Utopia.Route.t) =>
  >   Js.Promise.resolve(Routes.About.route);
  > 
  > [@react.component]
  > let make = () => <div> {React.string("home")} </div>;
  > EOF
  $ cat > app/about/page.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("about")} </div>;
  > EOF
  $ utopia.compiler > /dev/null
  $ dune build @melange _utopia/server_main.exe > /dev/null
  $ dune describe pp _utopia/native/Pages__Home__Page.re > native.pp
  $ action_id=$(grep -oP 'Runtime\.id: "\K[^"]+' native.pp | head -1)
  $ PORT=8112 HOST=127.0.0.1 NO_LOG=1 _build/default/_utopia/server_main.exe > server.log 2>&1 &
  $ server_pid=$!
  $ curl -i -s --retry 10 --retry-connrefused --retry-delay 1 -X POST -H 'Accept: application/react.action' -H 'Content-Type: text/plain;charset=utf-8' -H "X-Action-ID: $action_id" --data '[]' http://127.0.0.1:8112/home | rg 'HTTP/1.1 200 OK|Content-Type: application/react.action|^0:\{"pathname":"/about","request_path":"/about","href":"/about"\}$'
  HTTP/1.1 200 OK
  Content-Type: application/react.action
  0:{"pathname":"/about","request_path":"/about","href":"/about"}
  $ kill $server_pid 2>/dev/null || true
  $ wait $server_pid 2>/dev/null || true
