  $ tmp=$(mktemp -d)
  $ mkdir -p "$tmp/work/demo/app/_utopia/dist" "$tmp/work/demo/app/_utopia/static" "$tmp/work/demo/app/target/demo/app/_utopia"
  $ touch "$tmp/work/dune-project"
  $ (cd "$tmp/work/demo/app" && utopia clean --build-outputs 2>&1)
  
  utopia clean
  
    ✓ Removed _utopia/dist
    ✓ Removed _utopia/static
    ✓ Removed target/demo/app/_utopia
    ✓ Clean complete
  
  $ test -d "$tmp/work/demo/app/_utopia" && echo kept-generated-scaffold
  kept-generated-scaffold
  $ test ! -e "$tmp/work/demo/app/_utopia/dist" && echo removed-dist
  removed-dist
  $ test ! -e "$tmp/work/demo/app/_utopia/static" && echo removed-static
  removed-static
  $ test ! -e "$tmp/work/demo/app/target/demo/app/_utopia" && echo removed-target
  removed-target
