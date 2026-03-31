  $ mkdir pages _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(dirs :standard _utopia)\n" > dune
  $ touch _utopia/dune
  $ cat > pages/Home.re <<'EOF'
  > [@react.server.function]
  > let greet = (~name: string): Js.Promise.t(string) =>
  >   Js.Promise.resolve("Hello " ++ name);
  > 
  > [@react.server.function]
  > let greetForm = (~formData: Js.FormData.t): Js.Promise.t(string) => {
  >   let name =
  >     Js.FormData.get(formData, "name")
  >     |> (
  >       fun
  >       | `String(value) => value
  >     );
  >   Js.Promise.resolve("Hello " ++ name);
  > };
  > 
  > [@react.component]
  > let make = () => <div> {React.string("home")} </div>;
  > EOF
  $ utopia.compiler > /dev/null
  $ dune build @melange _utopia/server_main.exe > /dev/null
  $ dune describe pp _utopia/native/Utopia_page__Home.re > native.pp
  $ eval "$(python3 - <<'PY'
  > from pathlib import Path
  > import re
  > ids = re.findall(r'Runtime.id: "([^"]+)"', Path('native.pp').read_text())
  > print(f'body_id={ids[0]}')
  > print(f'form_id={ids[1]}')
  > PY
  > )"
  $ PORT=8104 HOST=127.0.0.1 NO_LOG=1 _build/default/_utopia/server_main.exe > server.log 2>&1 &
  $ server_pid=$!
  $ curl -i -s --retry 5 --retry-connrefused --retry-delay 1 -X POST -H 'Accept: application/react.action' -H 'Content-Type: text/plain;charset=utf-8' -H "X-Action-ID: $body_id" --data '["Alice"]' http://127.0.0.1:8104/home | rg 'HTTP/1.1 200 OK|Content-Type: application/react.action|^0:"Hello Alice"$'
  HTTP/1.1 200 OK
  Content-Type: application/react.action
  0:"Hello Alice"
  $ curl -i -s --retry 5 --retry-connrefused --retry-delay 1 -X POST -H 'Accept: application/react.action' -H "X-Action-ID: $form_id" -F '0=["$K1"]' -F '1_name=Form Alice' http://127.0.0.1:8104/home | rg 'HTTP/1.1 200 OK|Content-Type: application/react.action|^0:"Hello Form Alice"$'
  HTTP/1.1 200 OK
  Content-Type: application/react.action
  0:"Hello Form Alice"
  $ kill $server_pid
  $ wait $server_pid || true
  Terminated
