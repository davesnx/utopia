  $ mkdir pages _utopia
  $ touch _utopia/dune
  $ printf "let page = ()\n" > pages/Home.re
  $ utopia.compiler > /dev/null
  $ PORT=8102 HOST=127.0.0.1 NO_LOG=1 utopia.server > server.log 2>&1 &
  $ server_pid=$!
  $ curl -i -s --retry 5 --retry-connrefused --retry-delay 1 -H 'Accept: application/react.component' http://127.0.0.1:8102/home | rg "HTTP/1.1 200 OK|Content-Type: application/react.component|X-Content-Type-Options: nosniff|X-Location: /home|^0:\["
  HTTP/1.1 200 OK
  Content-Type: application/react.component
  X-Content-Type-Options: nosniff
  X-Location: /home
  0:["$","html",null,{"children":[["$","head",null,{"children":[["$","meta",null,{"charSet":"utf-8"},null,[],{}],["$","meta",null,{"name":"viewport","content":"width=device-width, initial-scale=1"},null,[],{}],["$","title",null,{"children":"home"},null,[],{}]]},null,[],{}],["$","body",null,{"children":["$","div",null,{"children":["$","main",null,{"children":[["$","h1",null,{"children":"/home"},null,[],{}],["$","p",null,{"children":["Code page from ",["$","code",null,{"children":"pages/Home.re"},null,[],{}],"."]},null,[],{}],["$","pre",null,{"children":"let page = ()\n"},null,[],{}]]},null,[],{}],"id":"root"},null,[],{}]},null,[],{}]]},null,[],{}]
  $ kill $server_pid
  $ wait $server_pid || true
  Terminated
