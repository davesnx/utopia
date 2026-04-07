  $ mkdir -p app/api _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ printf "let page = ()\n" > app/api/page.re
  $ utopia.compiler 2>&1 | rg -F 'Invalid app route declaration: app/api/page.re is a page file inside app/api/**'
      - Invalid app route declaration: app/api/page.re is a page file inside app/api/**
