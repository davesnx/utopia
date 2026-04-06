  $ mkdir pages _utopia _utopia/dist
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ printf "let page = ()\n" > pages/Home.re
  $ utopia.compiler > /dev/null
  $ printf "console.log('demo bootstrap');\n" > _utopia/dist/client_entry_melange.js
  $ printf "body { color: black; }\n" > output.css
  $ PORT=8105 HOST=127.0.0.1 NO_LOG=1 utopia.server > server.log 2>&1 &
  $ server_pid=$!
  $ curl -s --retry 5 --retry-connrefused --retry-delay 1 http://127.0.0.1:8105/home | rg -o 'output\.css|modulepreload|client_entry_melange\.js'
  output.css
  modulepreload
  client_entry_melange.js
  output.css
  client_entry_melange.js
  $ curl -i -s --retry 5 --retry-connrefused --retry-delay 1 http://127.0.0.1:8105/output.css | rg "HTTP/1.1 200 OK|Content-Type: text/css; charset=utf-8|body \{ color: black; \}"
  HTTP/1.1 200 OK
  Content-Type: text/css; charset=utf-8
  body { color: black; }
  $ curl -i -s --retry 5 --retry-connrefused --retry-delay 1 http://127.0.0.1:8105/dist/client_entry_melange.js | rg "HTTP/1.1 200 OK|Content-Type: application/javascript; charset=utf-8|demo bootstrap"
  HTTP/1.1 200 OK
  Content-Type: application/javascript; charset=utf-8
  console.log('demo bootstrap');
  $ kill $server_pid
  $ wait $server_pid || true
  Terminated
