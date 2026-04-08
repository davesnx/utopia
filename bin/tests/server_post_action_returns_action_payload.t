  $ mkdir pages _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > pages/Home.re <<'EOF'
  > let before = _request => ();
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
  $ dune describe pp _utopia/native/Pages__Home.re > native.pp
  $ body_line=$(grep -n 'let greet = {' native.pp | sed -n '1p' | cut -d: -f1)
  $ body_id=$(sed -n "$((body_line + 1))p" native.pp | grep -oP 'Runtime\.id: "\K[^"]+')
  $ form_line=$(grep -n 'let greetForm = {' native.pp | sed -n '1p' | cut -d: -f1)
  $ form_id=$(sed -n "$((form_line + 1))p" native.pp | grep -oP 'Runtime\.id: "\K[^"]+')
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
