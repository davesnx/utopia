  $ mkdir pages _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(dirs :standard _utopia)\n" > dune
  $ touch _utopia/dune
  $ cat > pages/Home.re <<'EOF'
  > [@react.component]
  > let make = () => <div> <h1> {React.string("Hello")} </h1> </div>;
  > EOF
  $ utopia.compiler > /dev/null
  $ dune build @melange _utopia/server_main.exe > /dev/null
  $ PORT=8104 HOST=127.0.0.1 NO_LOG=1 _build/default/_utopia/server_main.exe > server.log 2>&1 &
  $ server_pid=$!
  $ curl -i -s --retry 5 --retry-connrefused --retry-delay 1 -H 'Accept: application/react.component' http://127.0.0.1:8104/home > response.txt
  $ rg -o 'HTTP/1.1 200 OK|Content-Type: application/react.component|X-Location: /home|dangerouslySetInnerHTML' response.txt
  HTTP/1.1 200 OK
  Content-Type: application/react.component
  X-Location: /home
  dangerouslySetInnerHTML
  $ ! rg -q 'Async exception|InnerHtml does not exist in RSC' server.log
  $ kill $server_pid
  $ wait $server_pid || true
  Terminated
