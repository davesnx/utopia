let () =
  Utopia_server.run_generated_routes_server_cli
    (module Routes_server)
    ~lookup_server_function:FunctionReferences.get ()
