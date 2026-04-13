  $ mkdir -p app/users/[id] routes/users _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > app/users/[id]/page.ml <<'EOF'
  > let before _request = ()
  > let[@react.component] make () =
  >   let route =
  >     Routes.Users.Param_id.make
  >       ~params:{ Routes.Users.Param_id.Route_params.id = 42 }
  >       ()
  >   in
  >   match Routes.of_route route with
  >   | Some (Routes.Users_id_page { params }) ->
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
  $ grep -qF 'module Route_params = Route_schema__Users__Id.Params' _utopia/Routes_client.ml
  $ grep -qF 'let make ~params =' _utopia/Routes_client.ml
  $ grep -qF 'Users_id_page of {' _utopia/Routes_client.ml
  $ grep -qF 'params: Users.Param_id.Route_params.t' _utopia/Routes_client.ml
  $ grep -qF 'Users.Param_id.Route_params.decode' _utopia/Routes_client.ml
  $ export UTOPIA_ROOT="$(dirname "$OPAM_SWITCH_PREFIX")"
  $ export OCAMLPATH="$UTOPIA_ROOT/_build/install/default/lib"
  $ dune build --root . @melange _utopia/server_main.exe > /dev/null
