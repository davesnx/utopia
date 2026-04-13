  $ mkdir -p app/home _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > app/home/page.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("hello")} </div>;
  > EOF
  $ utopia.compiler > /dev/null
  $ dune build _utopia/server_main.exe > /dev/null
  $ PORT=8102 HOST=127.0.0.1 NO_LOG=1 _build/default/_utopia/server_main.exe > server.log 2>&1 &
  $ server_pid=$!
  $ curl -i -s --retry 5 --retry-connrefused --retry-delay 1 -H 'Accept: application/react.component' http://127.0.0.1:8102/home | rg "HTTP/1.1 200 OK|Content-Type: application/react.component|X-Content-Type-Options: nosniff|X-Location: /home|^0:\["
  HTTP/1.1 200 OK
  Content-Type: application/react.component
  X-Content-Type-Options: nosniff
  X-Location: /home
  0:["full","",["$","$1",null,{"path":"/","layout":["$","$2",null,{},null,[],1],"pageconsumer":["$","$1",null,{"path":"/home","layout":["$","div",null,{"children":"hello"},null,[],1],"pageconsumer":null},null,[],1]},null,[],1]]
  $ kill $server_pid 2>/dev/null || true
  $ wait $server_pid 2>/dev/null || true
