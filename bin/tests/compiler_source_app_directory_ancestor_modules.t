  $ mkdir -p app/about _utopia
  $ cat > dune-project <<'EOF'
  > (lang dune 3.9)
  > (using melange 0.1)
  > EOF
  $ cat > dune <<'EOF'
  > (data_only_dirs _utopia)
  > (include _utopia/dune)
  > EOF
  $ touch _utopia/dune
  $ cat > app/button.ml <<'EOF'
  > let value = 1
  > EOF
  $ cat > app/page.ml <<'EOF'
  > let root = 0
  > EOF
  $ cat > app/about/page.ml <<'EOF'
  > let child = Button.value
  > EOF
  $ utopia.compiler > /dev/null
  $ grep -qF '(subdir app' _utopia/dune
  $ grep -qF '(subdir app/about' _utopia/dune
  $ grep -Eq 'source_pages_.*_app_about' _utopia/dune
  $ [ "$(grep -oE 'source_pages_[^ )]*_app_root' _utopia/dune | wc -l)" -ge 2 ]
