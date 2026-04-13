# Fix naming conventions

**Status**: Completed
**Priority**: Medium
**Dependencies**: None (but easier after 21-client-server-library-boundary)

## Problem

Per AGENTS.md rules: *"All modules, variants, polyvariants should use this casing: Ocaml_case, not PascalCase."*

Two modules violated this. One has been fixed; one is blocked:

| Original name | Target name | File | Status |
|-------------|-----------|------|--------|
| `ReactServerDOMEsbuild` | `React_server_dom_esbuild` | `lib/utopia/React_server_dom_esbuild.re` | **Done** |
| `FunctionReferences` | `Function_references` | `lib/utopia/FunctionReferences.re` | **Blocked** -- the `server-reason-react` PPX hardcodes `FunctionReferences.register` in generated code for `[@react.server.function]` (see `server_reason_react_ppx.ml:1303-1308`). Renaming requires an upstream change to server-reason-react. |

## Impact analysis

### `ReactServerDOMEsbuild`

Referenced in:
- `lib/utopia/Utopia_call_server.re` -- calls `ReactServerDOMEsbuild.encodeReply`, `ReactServerDOMEsbuild.createFromReadableStream`, etc.
- `lib/utopia/client_entry.re` -- calls `ReactServerDOMEsbuild.createFromFetch`, `ReactServerDOMEsbuild.createServerReference`
- `lib/utopia/dune` -- listed in `(modules ...)` and `(install ...)`
- `bin/compiler/Generated_dune.ml` -- listed in `utopia_client_modules`
- `bin/compiler/Runtime_files.ml` -- copies this file

### `FunctionReferences`

Referenced in:
- `lib/utopia/dune` -- listed in `(modules ...)`
- `lib/utopia/server_main.ml` -- calls `FunctionReferences.register` and `FunctionReferences.get`
- `bin/compiler/Generated_dune.ml` -- possibly in native module lists
- `bin/compiler/Runtime_files.ml` -- copies this file

## Plan

### Step 1: Rename `ReactServerDOMEsbuild.re` -> `React_server_dom_esbuild.re`

1. `mv lib/utopia/ReactServerDOMEsbuild.re lib/utopia/React_server_dom_esbuild.re`
2. Update `lib/utopia/dune`: `(modules ...)` and `(install ...)` -- rename entries
3. Update `lib/utopia/Utopia_call_server.re`: `ReactServerDOMEsbuild.` -> `React_server_dom_esbuild.`
4. Update `lib/utopia/client_entry.re`: same
5. Update `bin/compiler/Generated_dune.ml`: update `utopia_client_modules` list
6. Update `bin/compiler/Runtime_files.ml`: update file references

### Step 2: Rename `FunctionReferences.re` -> `Function_references.re`

1. `mv lib/utopia/FunctionReferences.re lib/utopia/Function_references.re`
2. Update `lib/utopia/dune`: `(modules ...)` entry
3. Update `lib/utopia/server_main.ml`: `FunctionReferences.` -> `Function_references.`
4. Update `bin/compiler/Generated_dune.ml`: update module lists
5. Update `bin/compiler/Runtime_files.ml`: update file references

### Step 3: Verify no other PascalCase violations

Run: `ls lib/ bin/ -R | grep -E '^[A-Z][a-z]+[A-Z]'` to find any other PascalCase module names.

Known exceptions that are fine:
- `Utopia_*.ml` -- these use `Ocaml_case` with a capitalized first letter (standard OCaml)
- Files in `_utopia/` -- generated, not hand-written

## Cram test impact

Cram tests that reference these module names in expected output will need updating. Search for `ReactServerDOMEsbuild` and `FunctionReferences` in `bin/tests/*.t` files.

## Verification

- `make build` succeeds
- All cram tests pass (after promoting expected output)
- `grep -r 'ReactServerDOMEsbuild\|FunctionReferences' lib/ bin/` returns zero results (only in generated `_utopia/` dirs)
- Demo projects build

## Files modified

- `lib/utopia/ReactServerDOMEsbuild.re` -> `lib/utopia/React_server_dom_esbuild.re`
- `lib/utopia/FunctionReferences.re` -> `lib/utopia/Function_references.re`
- `lib/utopia/dune`
- `lib/utopia/Utopia_call_server.re`
- `lib/utopia/client_entry.re`
- `lib/utopia/server_main.ml`
- `bin/compiler/Generated_dune.ml`
- `bin/compiler/Runtime_files.ml`
- Various cram test `.t` files (expected output updates)
