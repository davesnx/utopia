  $ utopia prod 2>&1
  
  utopia prod
  
    ▸ Verifying build artifacts
    ✗ Missing required build artifacts. Run 'utopia build' first.
      missing: _utopia/dune
      missing: _utopia/routes.manifest
  [1]

The 'start' alias should also work:

  $ utopia start 2>&1
  
  utopia prod
  
    ▸ Verifying build artifacts
    ✗ Missing required build artifacts. Run 'utopia build' first.
      missing: _utopia/dune
      missing: _utopia/routes.manifest
  [1]
