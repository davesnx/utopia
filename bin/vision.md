# Vision

- React based (reason-react and server-reason-react)
- It wraps dune (it generates dune rules for you)
- BOYD, but recommends:
  - https://github.com/tjdevries/octane.ml + mysql
  - ppx_rapper + catqui (a bit hard to use and ugly but it works well)
- "lib" folder where BANANAS can happen
- Expose via utopia.??? a config
  - How people will configure dependencies of pages?
- A CLI to run (compiler, server, etc)
- Define pages (could be markdown, or components)
  - How to create the router?
  - How to extend the router?
- Provide a "RemoteData" or "RPC" module to help handle data
- Should have a way to SSG static pages (wget ...)
- Integrates with Dream
- Deployments
 - https://coolify.io
 - https://www.flightcontrol.dev
 - ???
 - Simply docker

# CLI

## pages
- Pages will contain our melange code (inject code)
- Pages will contain user melange code (inject users code)
- Pages have the folder "lib" -open Lib. We recommend (include qualified) under lib
-

## dev
- Runs the dev server
  - Compiles the code
  - Setups the router and loads all pages
- Hot reloads
