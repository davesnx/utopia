  $ mkdir -p demo/notes/pages demo/notes/lib
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(dirs :standard demo)\n" > dune
  $ printf "(dirs :standard _utopia)\n" > demo/notes/dune
  $ cat > demo/notes/lib/Greeting.re <<'EOF'
  > let label = "nested lib ready";
  > let message = name => "Hello nested prod " ++ name;
  > EOF
  $ cat > demo/notes/pages/Home.re <<'EOF'
  > [@react.server.function]
  > let greet = (~name: string): Js.Promise.t(string) =>
  >   Js.Promise.resolve(Greeting.message(name));
  > 
  > [@react.component]
  > let make = () => <div> {React.string(Greeting.label)} </div>;
  > EOF
  $ (cd demo/notes && utopia build > build.log 2>&1)
  $ ! rg -q 'Entering directory|Leaving directory' demo/notes/build.log
  $ dune describe pp demo/notes/_utopia/native/Utopia_page__Home.re > native.pp
  $ eval "$(python3 - <<'PY'
  > from pathlib import Path
  > import re
  > action_id = re.findall(r'Runtime.id: "([^"]+)"', Path('native.pp').read_text())[0]
  > print(f'action_id={action_id}')
  > PY
  > )"
  $ cd demo/notes
  $ PORT=8113 HOST=127.0.0.1 NO_LOG=1 utopia prod > prod.log 2>&1 &
  $ prod_pid=$!
  $ python3 - <<'PY'
  > import subprocess
  > html = subprocess.check_output([
  >   'curl', '-s', '--retry', '10', '--retry-connrefused', '--retry-delay', '1',
  >   'http://127.0.0.1:8113/home',
  > ], text=True)
  > assert 'nested lib ready' in html
  > print('nested lib ready')
  > PY
  nested lib ready
  $ curl -i -s --retry 10 --retry-connrefused --retry-delay 1 -X POST -H 'Accept: application/react.action' -H 'Content-Type: text/plain;charset=utf-8' -H "X-Action-ID: $action_id" --data '["Alice"]' http://127.0.0.1:8113/home | rg 'HTTP/1.1 200 OK|Content-Type: application/react.action|^0:"Hello nested prod Alice"$'
  HTTP/1.1 200 OK
  Content-Type: application/react.action
  0:"Hello nested prod Alice"
  $ kill $prod_pid
  $ wait $prod_pid || true
