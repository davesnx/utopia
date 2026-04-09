Extraction works with OCaml/mlx struct...end module syntax:

  $ cat > input.mlx <<'EOF'
  > let server_helper () = "server"
  > let shared_helper x = x ^ " world"
  > 
  > module Widget = struct
  >   let[@react.client.component] make ~(label : string) () =
  >     <div>(React.string (shared_helper label))</div>
  > end
  > 
  > let[@react.component] make () =
  >   let data = server_helper () in
  >   <div><Widget label=data /></div>
  > EOF
  $ utopia.compiler --extract-client input.mlx > output.mlx

Client component module is extracted:

  $ grep -qF 'module Widget = struct' output.mlx
  $ grep -qF 'react.client.component' output.mlx

Shared helper is included:

  $ grep -qF 'shared_helper' output.mlx

Server code is excluded:

  $ ! grep -qF 'server_helper' output.mlx

The server make with let-in expression is excluded:

  $ ! grep -qF 'let data' output.mlx
