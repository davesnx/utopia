  $ mkdir -p app/users/[id] routes/users _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ printf "let page = ()\n" > app/users/[id]/page.ml
  $ cat > routes/users/[id].ml <<'EOF'
  > module Params = struct
  >   type t = { id : int }
  > 
  >   let encode value =
  >     [ ("id", Utopia_route.Params.one (string_of_int value.id)) ]
  > end
  > EOF
  $ utopia.compiler > compiler.log 2>&1 ; test $? -eq 1
  $ rg 'Route schema routes/users/\[id\]\.ml defines module Params but is missing `let decode = \.\.\.`' compiler.log
      - Route schema routes/users/[id].ml defines module Params but is missing `let decode = ...`
