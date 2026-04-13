# Client/server library boundary

**Status**: Deferred -- the melange.emit mechanism requires client modules to be raw source file copies, not library dependencies. The comment in `Generated_dune.ml:420-425` explains: "They cannot be a library dependency because server-reason-react.react-server-dom-esbuild produces a pre-compiled JS that conflicts with melange.emit's output." A proper solution would require changes to how server-reason-react packages its melange output, which is an upstream concern.
**Priority**: High
**Dependencies**: 19-split-utopia-server

## Problem

The `utopia` library mixes server-only and client-safe modules in a single `(wrapped false)` dune library. The only indication of which modules are client-safe is a manually maintained string list in `Generated_dune.ml` (lines 421-438):

```ocaml
let utopia_client_modules = [
  "Utopia_types.ml"; "Utopia_route.ml"; "Utopia_call_server.re";
  "Utopia_router.re"; "Utopia_router_link.re"; "Utopia_router_route.re";
  "Utopia.re"; "React_server_dom_esbuild.re";
]
```

And a parallel `(install ...)` stanza in `lib/utopia/dune` that lists the same files. If a developer adds a server-only import to a "client" module, the error surfaces only when the melange build fails -- there is no structural enforcement.

Additionally, `(wrapped false)` pollutes the global module namespace with all 11+ modules.

## Target state

Split into two dune libraries:

1. **`utopia.client`** -- client-safe modules compiled by both native and melange
2. **`utopia`** (or `utopia.server`) -- server-only modules that depend on `utopia.client`

The compiler's `Generated_dune.ml` references `utopia.client` as a melange dependency instead of manually copying files.

## Considerations

### Why this is non-trivial

The current setup uses `server-reason-react.ppx`, `browser_ppx`, and `melange_ppx` preprocessors, plus `%platform` switches. Client modules need to compile in both native (for server-side rendering) and melange (for browser JS) targets. The `(install ...)` stanza ships raw source files so the compiler can copy them into `melange.emit` stanzas.

Splitting into two libraries requires:
- Both libraries to share the same PPX configuration for `%platform` switches
- The `(install ...)` stanza to move to the client library
- `Generated_dune.ml` to reference the client library's installed files
- Testing that melange.emit still works with the new library boundary

### Incremental approach

Rather than restructuring everything at once:

1. **First**: Create `utopia.client` sub-library with just the client modules
2. **Second**: Have `utopia` depend on `utopia.client` (re-exporting if needed)
3. **Third**: Update `Generated_dune.ml` to use the library rather than file lists
4. **Fourth**: Remove `(wrapped false)` from the server library (optional, separate)

## Plan

### Step 1: Create `lib/utopia_client/` directory

Move these files:
- `Utopia_types.ml` (shared types)
- `Utopia_route.ml` (route value type)
- `Utopia_call_server.re` (server action transport)
- `Utopia_router.re` (client-side router)
- `Utopia_router_link.re` (Link component)
- `Utopia_router_route.re` (route rendering)
- `Utopia.re` (public API)
- `React_server_dom_esbuild.re` (RSC client bindings)

### Step 2: Write `lib/utopia_client/dune`

```
(library
 (name utopia_client)
 (public_name utopia.client)
 (wrapped false)
 (libraries
  reason-react
  server-reason-react.runtime
  server-reason-react.react
  server-reason-react.fetch
  server-reason-react.url_native
  server-reason-react.webapi
  melange-json)
 (preprocess
  (pps
   server-reason-react.ppx
   server-reason-react.browser_ppx
   server-reason-react.melange_ppx
   melange-json-native.ppx)))

(install
 (section lib)
 (package utopia)
 (files
  (Utopia_types.ml as utopia/Utopia_types.ml)
  (Utopia_route.ml as utopia/Utopia_route.ml)
  ...))
```

### Step 3: Update `lib/utopia/dune`

Add `utopia.client` to `(libraries ...)`. Remove the moved modules from `(modules ...)` and `(install ...)`.

### Step 4: Update `Generated_dune.ml`

Replace the `utopia_client_modules` string list with a reference to the `utopia.client` library's installed location. The `copy_dependency_rule` generation should derive paths from the installed package rather than hardcoding filenames.

### Step 5: Update compiler dependency

The compiler (`bin/compiler/dune`) depends on `utopia` which now depends on `utopia.client`. No change needed unless the compiler imports client-only modules directly.

## Verification

- `make build` succeeds (both native and melange targets)
- All cram tests pass
- Demo projects build successfully (`make -C demo/notes build`, `make -C demo/blog build`)
- Adding a `Dream` import to a client module causes a dune-level error (structural enforcement works)

## Risk

This is the most invasive structural change. The `(install ...)` stanza and how `Generated_dune.ml` copies source files into melange stanzas is the most brittle part of the build. Test extensively with real demo projects.

## Files modified

- New: `lib/utopia_client/dune`, `lib/utopia_client/*.re`, `lib/utopia_client/*.ml`
- `lib/utopia/dune` -- remove moved modules, add `utopia.client` dep
- `bin/compiler/Generated_dune.ml` -- replace file list with library reference
- `dune-project` -- possibly add `utopia.client` public name
