  $ mkdir pages _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > pages/Home.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("hello")} </div>;
  > EOF
  $ utopia.compiler > /dev/null
  $ dune build _utopia/server_main.exe > /dev/null
  $ PORT=8103 HOST=127.0.0.1 NO_LOG=1 _build/default/_utopia/server_main.exe > server.log 2>&1 &
  $ server_pid=$!
  $ curl -i -s --retry 5 --retry-connrefused --retry-delay 1 -X POST http://127.0.0.1:8103/home
  HTTP/1.1 400 Bad Request
  Content-Type: text/plain; charset=utf-8
  Content-Length: 26
  
  Missing X-Action-ID header
  $ kill $server_pid
  $ wait $server_pid || true
  Terminated
