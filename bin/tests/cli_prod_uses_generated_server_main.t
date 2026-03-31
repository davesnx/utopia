  $ mkdir pages _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(dirs :standard _utopia)\n" > dune
  $ touch _utopia/dune
  $ cat > pages/Home.re <<'EOF'
  > [@react.server.function]
  > let greet = (~name: string): Js.Promise.t(string) =>
  >   Js.Promise.resolve("Hello " ++ name);
  > 
  > [@react.component]
  > let make = () => <div> {React.string("home")} </div>;
  > EOF
  $ utopia build > /dev/null 2>&1
  $ dune describe pp _utopia/native/Utopia_page__Home.re > native.pp
  $ eval "$(python3 - <<'PY'
  > from pathlib import Path
  > import re
  > action_id = re.findall(r'Runtime.id: "([^"]+)"', Path('native.pp').read_text())[0]
  > print(f'action_id={action_id}')
  > PY
  > )"
  $ PORT=8110 HOST=127.0.0.1 NO_LOG=1 utopia prod > prod.log 2>&1 &
  $ prod_pid=$!
  $ curl -i -s --retry 10 --retry-connrefused --retry-delay 1 -X POST -H 'Accept: application/react.action' -H 'Content-Type: text/plain;charset=utf-8' -H "X-Action-ID: $action_id" --data '["Prod Alice"]' http://127.0.0.1:8110/home | rg 'HTTP/1.1 200 OK|Content-Type: application/react.action|^0:"Hello Prod Alice"$'
  HTTP/1.1 200 OK
  Content-Type: application/react.action
  0:"Hello Prod Alice"
  $ kill $prod_pid
  $ wait $prod_pid || true
