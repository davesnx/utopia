$ mkdir pages _utopia
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
$ printf "(dirs :standard _utopia)\n" > dune
$ touch _utopia/dune
$ printf "let page = ()\n" > pages/Home.mlx
$ utopia.compiler > /dev/null
$ python3 - <<'PY'
> from pathlib import Path
> text = " ".join(Path("_utopia/dune").read_text().split())
> checks = [
>     "deps ../pages/Home.mlx",
>     "subdir native",
>     "modules Utopia_page__Home Utopia_routes Utopia Utopia_route Utopia_types ReactServerDOMEsbuild Utopia_router Utopia_router_route Utopia_router_link client_entry_melange",
>     "modules FunctionReferences Utopia Utopia_route Utopia_types Utopia_router Utopia_router_route Utopia_router_link Utopia_routes Utopia_page__Home",
> ]
> missing = [needle for needle in checks if needle not in text]
> if missing:
>     raise SystemExit("\n".join(missing))
> PY
