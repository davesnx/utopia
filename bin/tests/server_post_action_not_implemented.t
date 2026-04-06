  $ mkdir pages _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ printf "let page = ()\n" > pages/Home.re
  $ utopia.compiler > /dev/null
  $ PORT=8103 HOST=127.0.0.1 NO_LOG=1 utopia.server > server.log 2>&1 &
  $ server_pid=$!
  $ curl -i -s --retry 5 --retry-connrefused --retry-delay 1 -X POST http://127.0.0.1:8103/home
  HTTP/1.1 400 Bad Request
  Content-Type: text/plain; charset=utf-8
  Content-Length: 26
  
  Missing X-Action-ID header
  $ kill $server_pid
  $ wait $server_pid || true
  Terminated
