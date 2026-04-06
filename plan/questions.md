- why don't we have a (flags -open) in the pages library, isntead of `echo "open! Melange_json.Primitives\n") (echo "open! Lib\n")` on each file?
- why do we create routes.manifest and not directly the Utopia_routes.ml?
- Instead of generating Utopia_types, Utopia_server, Utopia_router, Utopia_route, Utopia_call_server, and ReactServerDOMEsbuild and server_main, why don't we make the generated dune library/executable DEPEND ON A LIBRARY CALLED UTOPIA_RUNTIME that exposes Utopia.Router, Utopia.callServer, Utopia... etc
- Can you rename Utopia_page__ into Pages?
- Can you rename Utopia_lib__ into Lib?
- Why do we have 2 rules for a single file
  ```
   (rule (deps ../lib/button.mlx) (target Utopia_lib__Button.mlx)
   (action
    (with-stdout-to %{target}
     (progn (echo "open! Melange_json.Primitives\n") (echo "open! Lib\n")
      (run cat %{deps})))))

  (rule (deps ../lib/button.mlx) (target Lib__Button.mlx)
   (action
    (with-stdout-to %{target}
     (progn (echo "open! Melange_json.Primitives\n") (run cat %{deps})))))
  ```
- We don't need to generate all pages into melange. We only need the client components, so technically if a page doesn't contain a client component definition (a module with a [@react.client.component]) we shouldn't compile to melange... and even then, I only want to compile the client.component, not the entire page.
