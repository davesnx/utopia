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
$ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
$ touch _utopia/dune
$ cat > pages/Home.mlx <<'EOF'
> let[@react.server.function] action () : string Js.Promise.t =
>   Js.Promise.resolve "ok"
>
> module Widget = struct
>   let[@react.client.component] make ~(label : string) () =
>     <div> (React.string label) </div>
> end
>
> let[@react.component] make () = <Widget label="hello" />
> EOF
$ utopia.compiler > /dev/null
$ grep -qF 'deps ../pages/Home.mlx' _utopia/dune
$ grep -qF -- '-shared-folder-prefix=_utopia/' _utopia/dune
$ grep -qF -- '-shared-folder-prefix=_utopia/native/' _utopia/dune
$ ! grep -qF -- '-shared-folder-prefix=../' _utopia/dune
$ ! grep -qF -- '-shared-folder-prefix=../../' _utopia/dune
$ ! grep -qF '# 1 "../pages/Home.mlx"' _utopia/dune
$ ! grep -qF '# 1 "../../pages/Home.mlx"' _utopia/dune
$ dune build @melange _utopia/server_main.exe > /dev/null
$ grep -qF '(subdir native' _utopia/dune
$ cat _utopia/dune | tr -s ' \n' ' ' | grep -qF 'modules Pages__Home client_entry_melange'
$ cat _utopia/dune | tr -s ' \n' ' ' | grep -qF '(library (name utopia_'
$ cat _utopia/dune | tr -s ' \n' ' ' | grep -qF 'modules Routes'
