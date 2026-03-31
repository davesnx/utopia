  $ mkdir pages routes _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(dirs :standard _utopia)\n" > dune
  $ touch _utopia/dune
  $ cat > pages/Search.re <<'EOF'
  > [@react.component]
  > let make = () => {
  >   let route =
  >     Utopia.Routes.Search.make(
  >       ~query={Utopia.Routes.Search.Route_query.q: "notes"},
  >       ~hash=Utopia.Routes.Search.Route_hash.Details,
  >       (),
  >     );
  >   let label =
  >     switch (Utopia.Routes.current(route)) {
  >     | Some(Utopia.Routes.Current.Search {query: Some(query), hash: Some(hash)}) =>
  >         query.q
  >         ++ ":"
  >         ++ switch (hash) {
  >            | Details => "details"
  >            | Overview => "overview"
  >            }
  >     | _ => "invalid"
  >     };
  >   <Utopia.Router.Navigate to_=route className="js-route-link">
  >     {React.string(label)}
  >   </Utopia.Router.Navigate>;
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
  $ grep -E "module Search = struct|module Route_query = Route_schema__Search.Query|module Route_hash = Route_schema__Search.Hash|let make \?query \?hash \(\) =|module Current = struct|Search \{ query = query; hash = hash \}|let current = Current.of_route|let route = make \(\)" _utopia/Utopia_routes.ml
  module Search = struct
    module Route_query = Route_schema__Search.Query
    module Route_hash = Route_schema__Search.Hash
    let make ?query ?hash () =
    let route = make ()
  module Current = struct
        | Some query, Some hash -> Some (Search { query = query; hash = hash })
  let current = Current.of_route
  $ dune build @melange _utopia/server_main.exe > /dev/null
