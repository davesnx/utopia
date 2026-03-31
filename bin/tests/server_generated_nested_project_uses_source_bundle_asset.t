  $ mkdir -p demo/app/pages demo/app/_utopia/dist
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(dirs :standard demo)\n" > dune
  $ printf "(dirs :standard _utopia)\n" > demo/app/dune
  $ touch demo/app/_utopia/dune
$ cat > demo/app/pages/Home.re <<'EOF'
> [@react.component]
> let make = () => <div> <h1> {React.string("Nested demo")} </h1> </div>;
> EOF
$ printf "console.log('nested demo bundle');\n" > demo/app/_utopia/dist/client_entry_melange.js
$ (cd demo/app && utopia.compiler > /dev/null)
$ dune build demo/app/_utopia/server_main.exe > /dev/null
$ printf "body { color: rebeccapurple; }\n" > _build/default/demo/app/output.css
$ mkdir -p _build/default/demo/app/_utopia/dist
$ printf "console.log('stale build bundle');\n" > _build/default/demo/app/_utopia/dist/client_entry_melange.js
$ PORT=8107 HOST=127.0.0.1 NO_LOG=1 _build/default/demo/app/_utopia/server_main.exe > server.log 2>&1 &
$ server_pid=$!
$ curl -s --retry 5 --retry-connrefused --retry-delay 1 http://127.0.0.1:8107/home | rg -o 'output\.css|modulepreload|client_entry_melange\.js'
  output.css
  modulepreload
  client_entry_melange.js
  client_entry_melange.js
$ curl -i -s --retry 5 --retry-connrefused --retry-delay 1 http://127.0.0.1:8107/output.css | rg "HTTP/1.1 200 OK|Content-Type: text/css; charset=utf-8|rebeccapurple"
  HTTP/1.1 200 OK
  Content-Type: text/css; charset=utf-8
  body { color: rebeccapurple; }
$ curl -s --retry 5 --retry-connrefused --retry-delay 1 http://127.0.0.1:8107/dist/client_entry_melange.js
  console.log('nested demo bundle');
$ kill $server_pid
$ wait $server_pid || true
  Terminated
