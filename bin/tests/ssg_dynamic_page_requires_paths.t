  $ mkdir -p app/blog/[slug] _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > 'app/blog/[slug]/page.re' <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("post")} </div>;
  > EOF
  $ utopia.compiler > out.log 2>&1; echo $?
  1
  $ rg 'Static pages with dynamic segments require a paths export' out.log
    Static pages with dynamic segments require a paths export:
  $ rg 'app/blog/\[slug\]/page\.re is static but has params \[slug\] without a paths export' out.log
      - app/blog/[slug]/page.re is static but has params [slug] without a paths export
