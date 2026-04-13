# Add module interfaces

**Status**: Partial -- added `.mli` for Utopia_route_match, Utopia_request_context, Utopia_dev_events, Utopia_types (library modules) and Names, Analysis (compiler modules). Remaining: Utopia_route, Utopia_server, Utopia_route_builder, FunctionReferences, and other compiler modules.
**Priority**: Medium
**Dependencies**: 19-split-utopia-server

## Problem

Only 5 out of ~40 source modules have `.mli` interface files (12.5% coverage). The existing interfaces are well-written (`dune_sexp.mli`, `utopia_path.mli`, `Ocaml_gen.mli`, `Esbuild.mli`, `utopia_runtime.mli`), but the most important modules lack them entirely.

Missing interfaces mean:
- No documentation of public API surface
- No enforcement of encapsulation
- Every internal function is accessible to every consumer
- Harder for developers (and AI agents) to understand what a module provides

## Modules that need `.mli` files

### Priority 1: Library modules (public API)

These are consumed by generated user code and the compiler:

| Module | Lines | Notes |
|--------|-------|-------|
| `Utopia_types.ml` | ~110 | Core type vocabulary. Interface should document all types. |
| `Utopia_route.ml` | ~210 | Route value type. Interface hides internals, exposes constructors. |
| `Utopia_server.mlx` | ~2225 (or ~1300 after plan 19) | The main server API. Critical to define what's public. |
| `Utopia_route_builder.mlx` | ~122 | Used by code generators. |
| `Utopia_markdown_api.ml` | small | Public markdown rendering surface. |
| `FunctionReferences.re` | 5 | Small but exposes global mutable state. |

### Priority 2: New modules from plan 19

Each extracted module from plan 19 should get an `.mli` at creation time:

| Module | Notes |
|--------|-------|
| `Utopia_request_context.ml` | Document request context API |
| `Utopia_route_match.ml` | Document route matching primitives |
| `Utopia_assets.ml` | Document asset serving |
| `Utopia_html.mlx` | Document HTML rendering |
| `Utopia_rsc.mlx` | Document RSC normalization |
| `Utopia_dev_events.ml` | Document dev event types and SSE |
| `Utopia_ssg.ml` | Document SSG pipeline |

### Priority 3: Compiler modules

| Module | Lines | Notes |
|--------|-------|-------|
| `Routes.ml` | 676 | Central to compiler. Many internal consumers. |
| `Analysis.ml` | ~250 | Used by `Client_component_scan.ml` and `Client_graph.ml`. |
| `Diagnostics.ml` | ~200 | Used by `compiler.ml`. |
| `Names.ml` | small | Constants and name sanitization. |
| `Generated_dune.ml` | 854 | Complex but single consumer (`compiler.ml`). Lower priority. |
| `Generated_routes.ml` | 848 | Same as above. |

### Priority 4: CLI modules

Lower priority since the CLI modules are all private to the executable:

| Module | Notes |
|--------|-------|
| `Flags.ml` | CLI flag types |
| `Process.ml` | Process management |
| `Terminal.ml` | Terminal formatting |

### Not needed

- `compiler.ml`, `cli.ml` -- entry points, no consumers
- `Build.ml`, `Dev.ml`, `Prod.ml`, etc. -- internal CLI commands
- Demo project files -- not library code

## Plan

### Phase 1: Write interfaces for plan 19 extractions

Each new module created in plan 19 starts with an `.mli` file. Write the interface *first*, then extract the implementation to match.

### Phase 2: Write interfaces for existing library modules

1. `Utopia_types.mli` -- document all types
2. `Utopia_route.mli` -- hide internal constructors, expose public API
3. `Utopia_server.mli` -- define public API (module types, `start_generated`, `run_generated_cli`, etc.)
4. `Utopia_route_builder.mli` -- expose builder functions
5. `FunctionReferences.mli` -- expose `register`, `get`, hide `registry`

### Phase 3: Write interfaces for compiler modules

1. `Routes.mli` -- types and scanning functions
2. `Analysis.mli` -- `origin` type and scanning functions
3. `Names.mli` -- constants and naming functions
4. `Diagnostics.mli` -- diagnostic functions

## Guidelines

- Interfaces should include doc comments (`(** ... *)`) on every exposed value
- Hide internal types and helper functions
- Use abstract types where possible (e.g., `type t` without exposing the record)
- Follow the existing style in `dune_sexp.mli` and `utopia_path.mli`

## Verification

- `make build` succeeds after each interface is added
- All cram tests pass
- No new compiler warnings about unused values (interfaces may surface these)

## Files created

~15-20 new `.mli` files across `lib/utopia/`, `bin/compiler/`, and `markdown/`.
