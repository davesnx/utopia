  $ mkdir -p pages/users routes/users _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(dirs :standard _utopia)\n" > dune
  $ touch _utopia/dune
  $ cat > pages/users/[id].ml <<'EOF'
  > let make () =
  >   let route =
  >     Utopia.Routes.Users.Param_id.make
  >       ~params:{ Utopia.Routes.Users.Param_id.Route_params.id = 42 }
  >       ()
  >   in
  >   match Utopia.Routes.current route with
  >   | Some (Utopia.Routes.Current.Users__Id { params }) ->
  >       React.string (string_of_int params.id)
  >   | _ -> React.string "invalid"
  > EOF
  $ cat > routes/users/[id].ml <<'EOF'
  > module Params = struct
  >   type t = { id : int }
  > 
  >   let encode value =
  >     [ ("id", Utopia_route.Params.one (string_of_int value.id)) ]
  > 
  >   let decode values =
  >     match Utopia_route.Params.find_one "id" values with
  >     | Some value -> Option.map (fun id -> { id }) (int_of_string_opt value)
  >     | None -> None
  > end
  > EOF
  $ utopia.compiler > /dev/null
  $ grep -E "module Users = struct|module Param_id = struct|module Route_params = Route_schema__Users__Id.Params|let make ~params \(\) =|Users__Id of \{ params : Users.Param_id.Route_params.t \}|Route_params.decode \[\(" _utopia/Utopia_routes.ml
  module Users = struct
    module Param_id = struct
      module Route_params = Route_schema__Users__Id.Params
      let make ~params () =
    | Users__Id of { params : Users.Param_id.Route_params.t }
        let params = Users.Param_id.Route_params.decode [("id", Utopia_route.Params.one id)] in
  $ dune build @melange _utopia/server_main.exe > /dev/null
