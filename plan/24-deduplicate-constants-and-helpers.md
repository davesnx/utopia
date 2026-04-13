# Deduplicate constants and helpers

**Status**: Completed
**Priority**: Medium
**Dependencies**: 19-split-utopia-server (partial -- can proceed independently for compiler-only items)

## Problem

Several constants, helper functions, and code patterns are duplicated across the codebase. Each instance is a maintenance risk -- if the logic changes, all copies must be updated in sync.

## Inventory

### A. `app_directory` constant (3 copies in compiler)

| File | Line | Definition |
|------|------|-----------|
| `bin/compiler/Routes.ml` | 3 | `let app_directory = "app"` |
| `bin/compiler/Names.ml` | 1 | `let app_directory = "app"` |
| `bin/compiler/Diagnostics.ml` | 1 | `let app_directory = "app"` |

**Fix**: Define once in `Names.ml` (which is the conventions module). Have `Routes.ml` and `Diagnostics.ml` reference `Names.app_directory`.

### B. Inline JS error reporter (3 copies in client code)

The same `window.__utopia_dev_report_error(...)` pattern appears in:

| File | Lines | Pattern |
|------|-------|---------|
| `lib/utopia/Utopia_call_server.re` | 8-18 | `reportServerActionError` function (well-named) |
| `lib/utopia/Utopia_router.re` | ~248-260 | Inlined raw JS |
| `lib/utopia/client_entry.re` | ~26-33 | Inlined raw JS |

**Fix**: Extract a shared `Utopia_dev_report.re` module with typed error reporting functions. Or, have `Utopia_router.re` and `client_entry.re` call `Utopia_call_server.reportServerActionError` (rename to `report_error` since it's not server-action-specific).

### C. HTTP header constants (duplicated in `Utopia_call_server.re`)

| File | Lines | Issue |
|------|-------|-------|
| `lib/utopia/Utopia_call_server.re` | 44-55 | Two header arrays that share `Accept` and `ACTION_ID`/`X-Action-ID` but differ only on `Content-Type` |

**Fix**: Build the common headers once and extend with the Content-Type conditionally:

```reason
let commonHeaders = [|
  ("Accept", "application/react.action"),
  ("ACTION_ID", id),
  ("X-Action-ID", id),
|];
let headers = if (isFormData) {
  Fetch.HeadersInit.makeWithArray(commonHeaders);
} else {
  Fetch.HeadersInit.makeWithArray(
    Array.append(commonHeaders, [|("Content-Type", "text/plain;charset=utf-8")|])
  );
};
```

### D. `application/react.action` and `application/react.component` content types

| File | Lines | String |
|------|-------|--------|
| `lib/utopia/Utopia_call_server.re` | 45, 51 | `"application/react.action"` (2x) |
| `lib/utopia/Utopia_router.re` | 270, 275 | `"application/react.component"` (2x) |

**Fix**: Define these as module-level constants. Low urgency since they're only duplicated within their own files.

### E. Reserved file convention basenames (compiler)

| File | Line | Definition |
|------|------|-----------|
| `bin/compiler/Generated_dune.ml` | 238 | `["page"; "layout"; "route"; "_middleware"; "not-found"]` |
| `bin/compiler/Routes.ml` | various | Same basenames referenced in pattern matches |

**Fix**: Define `Names.reserved_basenames` and reference it.

### F. Dependency path helpers in `Generated_dune.ml` (lines 216-235)

Seven tiny functions that are all variations of `Printf.sprintf "../%s"` or `Printf.sprintf "../../%s"` with `Filename.concat`.

**Fix**: Parameterize into 1-2 functions that take a depth/prefix argument:

```ocaml
let relative_dep ~depth name =
  let prefix = String.concat "" (List.init depth (fun _ -> "../")) in
  prefix ^ name
```

### G. Repetitive validation in `Route_schemas.ml` (lines 116-204)

Six sequential nearly-identical blocks checking `has_X && not (module_has_function ...)`:

```ocaml
let errors = if has_params && not (module_has_function source "Params" "encode") then ... :: errors else errors in
let errors = if has_params && not (module_has_function source "Params" "decode") then ... :: errors else errors in
let errors = if has_query  && not (module_has_function source "Query"  "encode") then ... :: errors else errors in
(* ... 3 more *)
```

**Fix**: Fold over a list of `(predicate, module_name, function_name)` triples:

```ocaml
let required_functions = [
  (has_params, "Params", "encode"); (has_params, "Params", "decode");
  (has_query,  "Query",  "encode"); (has_query,  "Query",  "decode");
  (has_hash,   "Hash",   "encode"); (has_hash,   "Hash",   "decode");
] in
let errors = List.fold_left (fun errors (pred, mod_name, fn_name) ->
  if pred && not (module_has_function source mod_name fn_name)
  then Printf.sprintf "Route schema %s defines module %s but is missing `let %s = ...`" source_file mod_name fn_name :: errors
  else errors
) errors required_functions
```

## Plan

### Step 1: Compiler constants (A, E)
Consolidate `app_directory` and reserved basenames into `Names.ml`.

### Step 2: Client error reporter (B)
Extract or reuse `reportServerActionError` across client modules.

### Step 3: HTTP headers (C, D)
Simplify header construction in `Utopia_call_server.re`.

### Step 4: Compiler helpers (F, G)
Parameterize path helpers and refactor validation loop.

## Verification

- `make build` succeeds
- All cram tests pass
- `grep` for each deduplicated constant confirms single definition

## Files modified

- `bin/compiler/Names.ml` -- add `app_directory`, `reserved_basenames`
- `bin/compiler/Routes.ml` -- import from `Names`
- `bin/compiler/Diagnostics.ml` -- import from `Names`
- `bin/compiler/Generated_dune.ml` -- import from `Names`, parameterize path helpers
- `bin/compiler/Route_schemas.ml` -- refactor validation loop
- `lib/utopia/Utopia_call_server.re` -- simplify headers, generalize error reporter
- `lib/utopia/Utopia_router.re` -- call shared error reporter
- `lib/utopia/client_entry.re` -- call shared error reporter
