  $ mkdir pages _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(dirs :standard _utopia)\n" > dune
  $ touch _utopia/dune
  $ cat > pages/Home.re <<'EOF'
  > [@react.server.function]
  > let nextRoute = (): Js.Promise.t(Utopia.Route.t) =>
  >   Js.Promise.resolve(Utopia.Routes.About.route);
  > 
  > [@react.component]
  > let make = () => <div> {React.string("home")} </div>;
  > EOF
  $ cat > pages/About.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("about")} </div>;
  > EOF
  $ utopia.compiler > /dev/null
  $ dune build @melange _utopia/server_main.exe > /dev/null
  $ dune describe pp _utopia/native/Utopia_page__Home.re > native.pp
  $ eval "$(python3 - <<'PY'
  > from pathlib import Path
  > import re
  > action_id = re.findall(r'Runtime.id: "([^"]+)"', Path('native.pp').read_text())[0]
  > print(f'action_id={action_id}')
  > PY
  > )"
  $ PORT=8112 HOST=127.0.0.1 NO_LOG=1 _build/default/_utopia/server_main.exe > server.log 2>&1 &
  $ server_pid=$!
  $ curl -i -s --retry 10 --retry-connrefused --retry-delay 1 -X POST -H 'Accept: application/react.action' -H 'Content-Type: text/plain;charset=utf-8' -H "X-Action-ID: $action_id" --data '[]' http://127.0.0.1:8112/home | rg 'HTTP/1.1 200 OK|Content-Type: application/react.action|^0:\{"pathname":"/about","request_path":"/about","href":"/about"\}$'
  HTTP/1.1 200 OK
  Content-Type: application/react.action
  0:{"pathname":"/about","request_path":"/about","href":"/about"}
  $ kill $server_pid
  $ wait $server_pid || true
  Terminated
