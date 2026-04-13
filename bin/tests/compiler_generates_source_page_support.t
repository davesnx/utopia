  $ mkdir -p app/blog/[slug] _utopia
  $ cat > dune-project <<'EOF'
  > (lang dune 3.9)
  > (using melange 0.1)
  > 
  > (dialect
  >  (name mlx)
  >  (implementation
  >   (extension mlx)
  >   (preprocess
  >    (run mlx-pp %{input-file}))))
  > EOF
  $ cat > dune <<'EOF'
  > (data_only_dirs _utopia)
  > (include _utopia/dune)
  > EOF
  $ touch _utopia/dune
  $ cat > app/page.mlx <<'EOF'
  > let[@react.component] make () = <div> (React.string "home") </div>
  > EOF
  $ cat > app/layout.mlx <<'EOF'
  > let[@react.component] make ~children () = children
  > EOF
  $ cat > app/blog/page.mlx <<'EOF'
  > let[@react.server.function] action () : string Js.Promise.t =
  >   Js.Promise.resolve "ok"
  > 
  > let[@react.component] make () = <div> (React.string "blog") </div>
  > EOF
  $ cat > app/blog/[slug]/page.mlx <<'EOF'
  > let before _request = ()
  > let[@react.component] make () = <div> (React.string "slug") </div>
  > EOF
  $ utopia.compiler > /dev/null
  $ grep -qF '(subdir app' _utopia/dune
  $ grep -qF '(subdir app/blog' _utopia/dune
  $ grep -qF '(subdir app/blog/[slug]' _utopia/dune
  $ grep -qE '\(modules (Page Layout|Layout Page)\)' _utopia/dune
  $ grep -qF '(modules Page)' _utopia/dune
  $ export UTOPIA_ROOT="$(dirname "$OPAM_SWITCH_PREFIX")"
  $ export OCAMLPATH="$UTOPIA_ROOT/_build/install/default/lib"
  $ dune build --root . @app/all @app/blog/all '@app/blog/[slug]/all' > /dev/null
  $ ls _build/default/app/.source_pages_*.objs/byte/page.cmi | grep -q 'page.cmi'
  $ ls _build/default/app/.source_pages_*.objs/byte/layout.cmi | grep -q 'layout.cmi'
  $ ls _build/default/app/blog/.source_pages_*.objs/byte/page.cmi | grep -q 'page.cmi'
  $ ls "_build/default/app/blog/[slug]"/.source_pages_*.objs/byte/page.cmi | grep -q 'page.cmi'
