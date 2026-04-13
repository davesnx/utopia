  $ mkdir -p app app/api/users/[id] app/api/fail _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > app/page.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("hello")} </div>;
  > EOF
  $ cat > app/api/_middleware.ml <<'EOF'
  > let middleware next request =
  >   let open Lwt.Syntax in
  >   let* response = next request in
  >   Dream.add_header response "X-Middleware-Root" "1";
  >   Lwt.return response
  > EOF
  $ cat > app/api/users/_middleware.ml <<'EOF'
  > let middleware next request =
  >   let open Lwt.Syntax in
  >   let* response = next request in
  >   Dream.add_header response "X-Middleware-Users" "1";
  >   Lwt.return response
  > EOF
  $ cat > app/api/users/[id]/route.ml <<'EOF'
  > let handler request =
  >   let id = Routes.Api.Params.id request in
  >   let method_name =
  >     match Dream.method_ request with
  >     | `POST -> "POST"
  >     | _ -> "GET"
  >   in
  >   Utopia_server.respond ~headers:[ ("X-Handler", "users") ]
  >     (Printf.sprintf "{\"id\":\"%s\",\"method\":\"%s\"}" id method_name)
  > EOF
  $ printf 'let handler _request = failwith "boom"\n' > app/api/fail/route.ml
  $ utopia.compiler > /dev/null
  $ dune build _utopia/server_main.exe > /dev/null
  $ PORT=8116 HOST=127.0.0.1 NO_LOG=1 _build/default/_utopia/server_main.exe > server.log 2>&1 &
  $ server_pid=$!
  $ curl -i -s --retry 5 --retry-connrefused --retry-delay 1 http://127.0.0.1:8116/api/users/42 | rg 'HTTP/1.1 200 OK|Content-Type: application/json; charset=utf-8|X-Middleware-Root: 1|X-Middleware-Users: 1|X-Handler: users|\{"id":"42","method":"GET"\}'
  HTTP/1.1 200 OK
  Content-Type: application/json; charset=utf-8
  X-Handler: users
  X-Middleware-Users: 1
  X-Middleware-Root: 1
  {"id":"42","method":"GET"}
  $ curl -i -s --retry 5 --retry-connrefused --retry-delay 1 -X POST http://127.0.0.1:8116/api/users/42 | rg 'HTTP/1.1 200 OK|\{"id":"42","method":"POST"\}'
  HTTP/1.1 200 OK
  {"id":"42","method":"POST"}
  $ curl -i -s --retry 5 --retry-connrefused --retry-delay 1 http://127.0.0.1:8116/api/missing | rg 'HTTP/1.1 404 Not Found|\{"error":"API route not found","code":"api_not_found","path":"/api/missing"\}'
  HTTP/1.1 404 Not Found
  {"error":"API route not found","code":"api_not_found","path":"/api/missing"}
  $ curl -i -s http://127.0.0.1:8116/api/fail | rg 'HTTP/1.1 500 Internal Server Error|\{"error":"Internal API error","code":"api_internal_error","path":"/api/fail"\}'
  HTTP/1.1 500 Internal Server Error
  {"error":"Internal API error","code":"api_internal_error","path":"/api/fail"}
  $ kill $server_pid 2>/dev/null || true
  $ wait $server_pid 2>/dev/null || true
