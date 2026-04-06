  $ utopia prod 2>&1
  
  utopia prod
  
    ▸ Verifying build artifacts
    ✗ Missing required build artifacts. Run 'utopia build' first.
      missing: _utopia/dune
      missing: _build/default/_utopia/server_main.exe
  [1]

The 'start' alias should also work:

  $ utopia start 2>&1
  
  utopia prod
  
    ▸ Verifying build artifacts
    ✗ Missing required build artifacts. Run 'utopia build' first.
      missing: _utopia/dune
      missing: _build/default/_utopia/server_main.exe
  [1]

If the generated server executable is missing, prod should fail too:

  $ mkdir -p _utopia
  $ : > _utopia/dune
  $ utopia prod 2>&1
  
  utopia prod
  
    ▸ Verifying build artifacts
    ✗ Missing required build artifacts. Run 'utopia build' first.
      missing: _build/default/_utopia/server_main.exe
  [1]
