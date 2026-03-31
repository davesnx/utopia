  $ mkdir pages _utopia
  $ touch _utopia/dune
  $ printf "let page = ()\n" > pages/Home.re
  $ utopia.compiler > /dev/null
  $ PORT=8101 HOST=127.0.0.1 NO_LOG=1 utopia.server > server.log 2>&1 &
  $ server_pid=$!
  $ curl -i -s --retry 5 --retry-connrefused --retry-delay 1 http://127.0.0.1:8101/home | rg "HTTP/1.1 200 OK|Content-Type: text/html; charset=utf-8|window.srr_stream.push\(\)|data-payload"
  HTTP/1.1 200 OK
  Content-Type: text/html; charset=utf-8
  </script><script data-payload='0:["$","html",null,{"children":[["$","head",null,{"children":[["$","meta",null,{"charSet":"utf-8"},null,[],{}],["$","meta",null,{"name":"viewport","content":"width=device-width, initial-scale=1"},null,[],{}],["$","title",null,{"children":["home"]},null,[],{}]]},null,[],{}],["$","body",null,{"children":[["$","div",null,{"children":[["$","main",null,{"children":[["$","h1",null,{"children":["/home"]},null,[],{}],["$","p",null,{"children":["Code page from ",["$","code",null,{"children":["pages/Home.re"]},null,[],{}],"."]},null,[],{}],["$","pre",null,{"children":["let page = ()\n"]},null,[],{}]]},null,[],{}]],"id":"root"},null,[],{}]]},null,[],{}]]},null,[],{}]
  '>window.srr_stream.push()</script></html><script>window.srr_stream.close()</script>
  $ curl -s --retry 5 --retry-connrefused --retry-delay 1 http://127.0.0.1:8101/home | rg "client_entry_melange\.js"
  [1]
  $ kill $server_pid
  $ wait $server_pid || true
  Terminated
