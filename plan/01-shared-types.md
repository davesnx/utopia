# Shared types

Extract duplicated types between compiler and server into a shared library.

---

## Goal

`compiler.ml` and `server.ml` both define `page_kind`, `param_kind`, and `route_segment`. This duplication is a maintenance hazard. Extract them into a shared library that both executables depend on.

---

## Dependencies

- `plan/00-cleanup.md` must be completed first

---

## Create the shared library

Create `lib/utopia_types/` with a single module:

```
lib/
  utopia_types/
    dune
    utopia_types.ml
```

The dune stanza:

```scheme
(library
 (name utopia_types)
 (public_name utopia.types))
```

---

## Define shared types

Move these types into `utopia_types.ml`:

```ocaml
type page_kind =
  | Code_page
  | Markdown_page

type param_kind =
  | Single
  | Catch_all
  | Optional_catch_all

type route_segment =
  | Static of string
  | Param of string * param_kind
```

Also move shared utility functions that both executables use:

- `string_of_kind` (compiler) / `parse_kind` (server) -- keep both, they are inverses
- `string_of_param_kind` (compiler) / `parse_param_kind` (server) -- same
- `kind_of_extension` (compiler only, but useful for server too)
- `is_valid_identifier` (compiler only, but useful for validation anywhere)

---

## Update compiler.ml

Replace local type definitions with `open Utopia_types`. Update `bin/dune` to add `utopia.types` as a dependency of the compiler executable.

---

## Update server.ml

Replace local type definitions with `open Utopia_types`. Update `bin/dune` to add `utopia.types` as a dependency of the Server executable.

---

## Update bench/bench_routing.ml

The benchmark file duplicates types from `server.ml` for isolation. Keep the duplication in benchmarks (benchmarks should be self-contained to avoid measurement noise from module loading). Add a comment noting the intentional duplication.

---

## Testing

### Verify compilation

`dune build` must succeed with zero errors after the extraction.

### Run all cram tests

All existing cram tests must pass unchanged. The extraction is purely structural -- no behavior change.

### Edge cases

- Verify that `utopia.types` does not pull in any heavy dependencies (it should depend on nothing)
- Verify that the compiler and server produce identical output before and after the extraction
- Run benchmarks to confirm no performance regression from the additional module

---

## Files changed

| Action | File |
|--------|------|
| Create | `lib/utopia_types/dune` |
| Create | `lib/utopia_types/utopia_types.ml` |
| Modify | `bin/dune` (add utopia.types dependency to compiler and Server) |
| Modify | `bin/compiler.ml` (replace local types with open) |
| Modify | `bin/server.ml` (replace local types with open) |
| Modify | `dune-project` (library entry if needed) |

---

## Acceptance criteria

- `grep -n "type page_kind" bin/compiler.ml bin/server.ml` returns zero matches
- `dune build` succeeds
- `dune runtest` passes all tests
- Benchmarks show no measurable regression
- `utopia_types.ml` has zero external dependencies
