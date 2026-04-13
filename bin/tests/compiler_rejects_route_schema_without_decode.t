  $ mkdir -p app/search routes _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ printf "let page = ()\n" > app/search/page.re
  $ cat > routes/search.re <<'EOF'
  > module Query = {
  >   type t = {q: string};
  >   let encode = value => [("q", value.q)];
  > };
  > EOF
  $ utopia.compiler > compiler.log 2>&1 ; test $? -eq 1
  $ rg 'Route schema routes/search.re defines module Query but is missing `let decode = \.\.\.`' compiler.log
      - Route schema routes/search.re defines module Query but is missing `let decode = ...`
