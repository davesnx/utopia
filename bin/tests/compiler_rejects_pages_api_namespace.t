  $ mkdir -p pages/api _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ printf "let page = ()\n" > pages/api/users.re
  $ utopia.compiler 2>&1 | rg 'reserved /api namespace|Page route /api/users from pages/api/users.re'
    Page routes cannot use the reserved /api namespace:
      - Page route /api/users from pages/api/users.re conflicts with reserved /api namespace
