# Optimization for Melange Page Compilation

Compile only browser-required code for Melange builds.

---

## Goal

Avoid compiling every page/layout module to Melange when most code is server-only.

Target outcome:

1. Only client component entrypoints are emitted to Melange
2. Their client-side dependency closure is emitted to Melange
3. Server-only page/layout code remains native-only

This reduces Melange compile time, JS bundle size pressure, and watch-mode rebuild cost.

---

## Dependencies

- `plan/02-compiler-rsc.md` -- generated `_utopia/dune`, melange, esbuild wiring
- `plan/04-client-components.md` -- client component extraction + bootstrap manifest flow
- `plan/11-dev-full-reload-and-browser-overlay.md` -- dev loop expectations for rebuild responsiveness

---

## Problem Statement

Current flow mirrors many page/layout/lib files into Melange contexts even when they do not contribute browser-executed code. That overbuilds significantly for server-heavy projects.

We need a deterministic selection pipeline that emits only code required by `[@react.client.component]` modules.

---

## Design Overview

Split code selection into two layers:

1. **Client component discovery**
   - Identify every client component definition in `pages/` and `lib/`
2. **Melange reachability closure**
   - Build a module dependency graph
   - Keep only transitive deps reachable from discovered client component entrypoints

For mixed files (server + client in same source), generate component-level bridge modules so Melange can target the client module directly without treating the page module as a bundle entrypoint.

---

## Discovery Algorithm

### 1) Scan source files

Input set:

- `pages/**/*.re|ml|mlx`
- `lib/**/*.re|ml|mlx`

Scanner requirements:

- detect `[@react.client.component]` safely
- ignore comments and string literals
- record source location, owning module path, and component export symbol

Output record shape:

```ocaml
type client_component_ref = {
  source_file : string;
  module_path : string list;
  export_name : string;
  source_kind : [ `Page | `Layout | `Lib ];
}
```

### 2) Build dependency graph

Construct a per-project module graph from resolved local imports:

- node: source module
- edge: local module import/use dependency

Use existing compiler path/module normalization to avoid duplicate graph identities.

### 3) Compute melange closure

Seed graph traversal from modules that host discovered client components.

Rules:

- include transitive local deps needed by client components
- exclude API-only/server-only modules not reachable from seeds
- preserve deterministic ordering for generated dune stanzas

---

## Component-Level Bridge Generation

For each discovered client component, generate a small `_utopia/client_components/<id>.re` bridge module that re-exports the client component symbol with a stable module identity for extraction/plugin tooling.

Example generated bridge:

```reason
module Source = Utopia_page__notes__index;
let make = Source.ClientPanel.make;
```

Behavior:

- bridge modules are melange entrypoints for client extraction
- source page modules are not treated as melange entrypoints
- only bridge + reachable deps are compiled for browser output

If a component shape cannot be bridged safely (unsupported syntax), compiler falls back to file-level inclusion for that source and emits a warning with location.

---

## Compiler Changes

1. Add `Client_component_scan` pass
   - lexical scan for `[@react.client.component]`
2. Add `Client_graph` pass
   - build local dependency graph + compute reachability closure
3. Add `Client_bridge_gen` pass
   - emit `_utopia/client_components/*.re` bridge modules
4. Update generated `_utopia/dune`
   - melange emits only bridge modules + reachable closure aliases
5. Update compiler diagnostics
   - print summary: discovered components, included melange modules, skipped modules, fallback cases

---

## Dune Impact

Generated `_utopia/dune` changes:

- add subdir for generated bridge modules (`_utopia/client_components/`)
- melange stanzas depend on bridge modules rather than broad mirrored page sets
- keep native pages/API libraries unchanged
- keep `@_utopia/esbuild` alias contract unchanged

The optimization is internal to generated dune graph shape; user build commands remain the same.

---

## Runtime / Bundle Impact

- `server-reason-react` client-component extraction still operates on melange output
- bootstrap manifest semantics unchanged
- smaller melange graph should reduce watch rebuild time and generated JS volume

No public runtime API changes.

---

## Testing

### Cram tests

**`melange_optimization_skips_server_only_pages.t`**
- Project with many server-only pages and one client component
- Assert generated melange stanzas exclude server-only page modules

**`melange_optimization_includes_client_dependency_closure.t`**
- Client component imports local helper modules
- Assert helpers are included in melange closure

**`melange_optimization_generates_component_bridges.t`**
- Client components defined inside page module
- Assert bridge modules are generated and used as melange entrypoints

**`melange_optimization_fallback_for_unsupported_component_shape.t`**
- Use unsupported client component declaration pattern
- Assert compiler warning + file-level fallback inclusion

**`melange_optimization_no_client_components.t`**
- Project with zero client components
- Assert melange step is skipped or reduced to minimal runtime scaffold

**`melange_optimization_nested_project_paths.t`**
- Nested workspace project
- Assert generated paths and aliases remain correct

### Regression checks

- Existing client-component fixtures still build and hydrate
- Existing `@_utopia/esbuild` build target remains valid
- Dev watch rebuild still triggers full reload behavior from plan 11

### Performance checks

- Compare before/after melange module count on representative project
- Compare before/after `utopia dev` rebuild latency for client-only edits
- Compare before/after full production build duration

---

## Edge Cases

- Multiple client components in one file
- Nested modules with `[@react.client.component]`
- Conditional compilation branches around client components
- Cyclic local imports in client dependency closure
- Client component importing module with server-only side effects
- Same component name in different source files (stable bridge naming)
- Empty closure due to scan/parser failure (hard error)

---

## Files Changed

| Action | File |
|--------|------|
| Modify | `bin/compiler.ml` (or split compiler modules) |
| Create | `bin/compiler/client_component_scan.ml` |
| Create | `bin/compiler/client_graph.ml` |
| Create | `bin/compiler/client_bridge_gen.ml` |
| Modify | `bin/compiler/Generated_dune.ml` |
| Modify | `lib/utopia/ReactServerDOMEsbuild.re` (only if bridge IDs need runtime support) |
| Create | `bin/tests/melange_optimization_skips_server_only_pages.t` |
| Create | `bin/tests/melange_optimization_includes_client_dependency_closure.t` |
| Create | `bin/tests/melange_optimization_generates_component_bridges.t` |
| Create | `bin/tests/melange_optimization_fallback_for_unsupported_component_shape.t` |
| Create | `bin/tests/melange_optimization_no_client_components.t` |
| Create | `bin/tests/melange_optimization_nested_project_paths.t` |

---

## Acceptance Criteria

- Melange compilation no longer includes all page/layout modules by default
- Only discovered client components and required dependency closure are compiled for browser output
- Mixed server/client files are handled via generated component bridges (or explicit fallback with warning)
- Existing client component behavior and bootstrap manifest semantics remain correct
- `utopia build` and `utopia dev` workflows remain unchanged from user perspective
- New optimization coverage passes, including fallback and nested-project scenarios
