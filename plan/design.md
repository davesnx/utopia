# Utopia Routing Notes (Internal)

This is an internal working note (not public docs) about what routing-like behavior exists today.

## What the compiler does today

The current compiler is `bin/compiler.ml` and it is focused on generating `_utopia/dune` rules from files inside `pages/`.

High-level flow:

1. Reads direct entries from `pages/` via `Sys.readdir`.
2. Splits files by extension:
   - `.re` and `.ml` -> treated as code pages.
   - `.md` -> treated as markdown pages.
   - any other extension -> ignored for rule generation.
3. Writes dune rules into `_utopia/dune`:
   - A copy rule per `.re`/`.ml` page to duplicate it into `<Name>_melange` and `<Name>_native` modules.
   - One `melange.emit` stanza containing all `<Name>_melange` modules.
   - One markdown conversion rule per `.md` page producing `<Name>.html` via `utopia.markdown`.
   - One `library` stanza containing all `<Name>_native` modules.

## What routes it can generate right now

Right now, routing is effectively file-name based build output, not URL-pattern routing.

- Supported, indirectly:
  - static top-level page names based on file basename (example: `pages/Home.re`, `pages/Guide.md`).
- Not currently represented in compiler output:
  - dynamic params (`[slug]`, `:slug` style).
  - catch-all or optional segments.
  - nested routes via folder structure.
  - query-string parsing or typed route params.
  - HTTP method-based route matching.

There is a `ppx_deriving_router` experiment in `bin/Ppx_deriving_router_runtime.ml`, but it is currently commented out and not wired into compiler output.

## Discovery tests added

I added cram tests under `bin/tests/` to lock in current behavior:

- `bin/tests/compiler_generates_dune_rules.t`
  - Builds a minimal fake `pages/` directory.
  - Runs `utopia.compiler`.
  - Asserts exact generated `_utopia/dune` stanzas for one `.re` page and one `.md` page.

- `bin/tests/compiler_requires_bootstrap_dune_file.t`
  - Documents current bootstrap requirement/bug:
    - compiler fails unless `_utopia/dune` already exists, because it calls `Sys.remove` before creating the file.

## Immediate implications for Next.js-style router work

Before adding dynamic and nested routes, the compiler likely needs a route manifest model (segments, params, loaders, output modules) instead of only extension-based file copying.
