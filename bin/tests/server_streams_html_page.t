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
  $ PORT=8101 HOST=127.0.0.1 NO_LOG=1 _build/default/_utopia/server_main.exe > server.log 2>&1 &
  $ server_pid=$!
  $ curl -i -s --retry 5 --retry-connrefused --retry-delay 1 http://127.0.0.1:8101/home | rg "HTTP/1.1 200 OK|Content-Type: text/html; charset=utf-8"
  HTTP/1.1 200 OK
  Content-Type: text/html; charset=utf-8
  $ curl -s --retry 5 --retry-connrefused --retry-delay 1 http://127.0.0.1:8101/home | rg -o '<div>hello</div>'
  <div>hello</div>
  $ curl -s --retry 5 --retry-connrefused --retry-delay 1 http://127.0.0.1:8101/home | rg 'window.srr_stream.close\(\)'
  '>window.srr_stream.push()</script><script>window.srr_stream.close()</script>
  $ curl -s --retry 5 --retry-connrefused --retry-delay 1 http://127.0.0.1:8101/home | rg "client_entry_melange\.js"
  [1]
  $ kill $server_pid
  $ wait $server_pid || true
  Terminated
