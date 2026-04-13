  $ mkdir -p app/api/users _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ printf "let page = ()\n" > app/api/users/page.re
  $ utopia.compiler > compiler.log 2>&1 || true
  $ rg -qF 'Invalid app route declaration: app/api/users/page.re is a page file inside app/api/**' compiler.log
