  $ mkdir -p pages/blog _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > 'pages/blog/[slug].re' <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("post")} </div>;
  > EOF
  $ utopia.compiler > out.log 2>&1; echo $?
  1
  $ rg 'Static pages with dynamic segments require a paths export' out.log
    Static pages with dynamic segments require a paths export:
  $ rg 'pages/blog/\[slug\]\.re is static but has params \[slug\] without a paths export' out.log
      - pages/blog/[slug].re is static but has params [slug] without a paths export
