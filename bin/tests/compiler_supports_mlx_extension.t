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
$ grep -qF 'deps ../pages/Home.mlx' _utopia/dune
$ grep -qF '(subdir native' _utopia/dune
$ cat _utopia/dune | tr -s ' \n' ' ' | grep -qF 'modules Utopia_page__Home Utopia_routes Utopia Utopia_route Utopia_types ReactServerDOMEsbuild Utopia_router Utopia_router_route Utopia_router_link client_entry_melange'
$ cat _utopia/dune | tr -s ' \n' ' ' | grep -qF 'modules FunctionReferences Utopia Utopia_route Utopia_types Utopia_router Utopia_router_route Utopia_router_link Utopia_route_builder Utopia_routes Utopia_page__Home'
