  $ mkdir -p app/search routes _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > app/search/page.re <<'EOF'
  > [@react.component]
  > let make = () => {
  >   let route =
  >     Routes.Search.make(
  >       ~query={Routes.Search.Route_query.q: "notes"},
  >       ~hash=Routes.Search.Route_hash.Details,
  >       (),
  >     );
  >   let label =
  >     switch (Routes.of_route(route)) {
  >     | Some(Routes.Search_page {query: Some(query), hash: Some(hash)}) =>
  >         query.q
  >         ++ ":"
  >         ++ switch (hash) {
  >            | Details => "details"
  >            | Overview => "overview"
  >            }
  >     | _ => "invalid"
  >     };
  >   <Utopia.Router.Link to_=route className="js-route-link">
  >     {React.string(label)}
  >   </Utopia.Router.Link>;
  > };
  > EOF
  $ cat > routes/search.re <<'EOF'
  > module Query = {
  >   type t = {q: string};
  >   let encode = value => [("q", value.q)];
  >   let decode = entries =>
  >     entries
  >     |> List.find_opt(((key, _value)) => key == "q")
  >     |> Option.map(((_key, value)) => {q: value});
  > };
  > 
  > module Hash = {
  >   type t = Details | Overview;
  >   let encode = value =>
  >     switch (value) {
  >     | Details => "details"
  >     | Overview => "overview"
  >     };
  >   let decode = value =>
  >     switch (value) {
  >     | "details" => Some(Details)
  >     | "overview" => Some(Overview)
  >     | _ => None
  >     };
  > };
  > EOF
  $ utopia.compiler > /dev/null
  $ grep -qF 'module Route_query = Route_schema__Search.Query' _utopia/Routes_client.ml
  $ grep -qF 'module Route_hash = Route_schema__Search.Hash' _utopia/Routes_client.ml
  $ grep -qF 'let make ?query =' _utopia/Routes_client.ml
  $ grep -qF 'fun ?hash ->' _utopia/Routes_client.ml
  $ grep -qF 'let route = make ()' _utopia/Routes_client.ml
  $ grep -qF 'let of_route route =' _utopia/Routes_client.ml
  $ grep -qF 'Search_page of' _utopia/Routes_client.ml
  $ export UTOPIA_ROOT="$(dirname "$OPAM_SWITCH_PREFIX")"
  $ export OCAMLPATH="$UTOPIA_ROOT/_build/install/default/lib"
  $ dune build --root . @melange _utopia/server_main.exe > /dev/null
