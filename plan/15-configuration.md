# Configuration

Implement the `utopia.ml` project configuration module.

---

## Goal

A project configuration file compiled and validated at build time. Serializable compile-time settings flow through the compiler; non-serializable runtime hooks/modules are resolved via a linked runtime registry. No YAML/TOML/JSON parsing.

---

## Dependencies

- `plan/01-shared-types.md` -- shared types
- `plan/03-server-rewrite.md` -- server library accepts configuration

---

## Design the configuration API

Create a configuration library that users import. `utopia.ml` records only serializable values (strings, ints, booleans, enums, lists). Runtime modules/hooks are referenced by key and resolved by a linked registry at server startup.

```ocaml
(* utopia.ml *)
let () =
  Utopia_config.build
    ~output_dir:"_utopia"
    ~dune_profile:`Dev
    ();

  Utopia_config.server
    ~port:8080
    ~host:"0.0.0.0"
    ();

  Utopia_config.markdown
    ~components_key:"my_markdown_components"
    ();

  Utopia_config.routing
    ~rewrites:[
      ("/old-path", "/new-path");
    ]
    ~redirects:[
      ("/legacy", "/modern", `Permanent);
    ]
    ()
```

Runtime hooks are registered in a linked module, for example `lib/utopia_runtime_registry.ml`:

```ocaml
let () =
  Utopia_runtime_registry.register_markdown_components
    ~key:"my_markdown_components"
    (module My_components : Utopia_runtime_registry.Markdown_components)
```

This keeps compile-time config serialization clean while still allowing rich runtime extension points.

---

## Create the configuration library

```
lib/
  utopia_config/
    dune
    utopia_config.ml
    utopia_config.mli
  utopia_runtime_registry/
    dune
    utopia_runtime_registry.ml
    utopia_runtime_registry.mli
```

`utopia_config` defines only serializable settings. `utopia_runtime_registry` owns non-serializable runtime hooks/modules (first-class modules, callback handlers, custom renderers) keyed by string identifiers.

The serializable config record is populated when `utopia.ml` is evaluated:

```ocaml
type redirect_kind = Permanent | Temporary

type config = {
  mutable build_output_dir : string;
  mutable dune_profile : [ `Dev | `Release ];
  mutable server_port : int;
  mutable server_host : string;
  mutable rewrites : (string * string) list;
  mutable redirects : (string * string * redirect_kind) list;
  mutable markdown_components_key : string option;
}

let global_config = { (* defaults *) }
```

---

## Compile and load serializable configuration

The compiler needs evaluated config values, but only for serializable fields. Options:

**Option A: Dynlink** -- compile `utopia.ml` and load it at compiler runtime. This is complex and fragile.

**Option B: Code generation** -- the compiler reads `utopia.ml` as source text and extracts configuration values via pattern matching on OCaml AST. This is also complex.

**Option C: Two-phase build** -- the compiler first compiles `utopia.ml` into a standalone executable that outputs serializable configuration as JSON/sexp, then reads that output. This is the cleanest approach.

Go with **Option C**:

1. Compiler generates a `_utopia/config_runner.ml` that imports `utopia.ml` and serializes the config
2. `dune build _utopia/config_runner.exe`
3. Compiler runs `_utopia/config_runner.exe` and reads the output
4. Compiler uses the serializable config to generate dune rules, manifests, etc.

`config_runner` does not serialize first-class modules or callbacks. It only emits key references (for example `markdown_components_key`) that can be resolved later by the runtime registry.

---

## Apply configuration in the compiler

The compiler uses serializable configuration values to:

- Set output directories in dune rules
- Add custom rewrites/redirects to the route manifest
- Carry runtime hook keys (e.g., markdown component key) into generated runtime wiring
- Set dune profile in build commands

---

## Runtime hooks/module registry

Non-serializable configuration is handled by a linked registry module.

Design:

1. User code registers hooks/modules by key in `utopia_runtime_registry`
2. Generated runtime/server code receives key references from compile-time config
3. At runtime, keys are resolved to linked implementations
4. Unknown keys produce clear startup errors (include available keys)

This split keeps compiler transport simple and deterministic while preserving extension power.

---

## Apply configuration in the server

At startup, the server receives both:

- serializable config payload (from compiler/config runner)
- runtime registry resolver (linked module lookup)

The server then uses configuration to:

- Apply rewrites (URL rewriting before route matching)
- Apply redirects (return 301/302 responses)
- Set custom response headers
- Resolve markdown component overrides via registry key lookup

---

## Handle missing utopia.ml

If `utopia.ml` doesn't exist, the compiler uses default configuration values. No error. The configuration file is optional.

---

## Testing

### Cram tests

**`config_default_without_utopia_ml.t`**
- Create a project without `utopia.ml`
- Run the compiler
- Assert success with default configuration

**`config_custom_rewrites.t`**
- Create `utopia.ml` with rewrites
- Run the compiler
- Verify the server handles rewrites correctly

**`config_custom_redirects.t`**
- Create `utopia.ml` with redirects
- Verify 301/302 responses

**`config_invalid_module.t`**
- Create `utopia.ml` with a syntax error
- Run the compiler
- Assert clear error message pointing to the config file

**`config_runtime_registry_key_resolves.t`**
- Configure `markdown_components_key` in `utopia.ml`
- Register matching key in `utopia_runtime_registry`
- Start server and verify custom markdown renderer is applied

**`config_runtime_registry_key_missing.t`**
- Configure `markdown_components_key` in `utopia.ml`
- Do not register that key at runtime
- Start server and assert clear startup error listing missing key

### Edge cases

- `utopia.ml` with no configuration calls (empty module)
- `utopia.ml` that calls configuration functions multiple times (last wins? error?)
- `utopia.ml` with unknown configuration fields
- Redirect loop detection (`/a` -> `/b` -> `/a`)
- Rewrite that targets a non-existent route
- Very large number of rewrites/redirects (1000+)
- Configuration that conflicts with CLI flags (CLI should win)
- Duplicate runtime registry key registration (error)
- Runtime hook registered but never referenced (warn only)

---

## Files changed

| Action | File |
|--------|------|
| Create | `lib/utopia_config/dune` |
| Create | `lib/utopia_config/utopia_config.ml` |
| Create | `lib/utopia_config/utopia_config.mli` |
| Create | `lib/utopia_runtime_registry/dune` |
| Create | `lib/utopia_runtime_registry/utopia_runtime_registry.ml` |
| Create | `lib/utopia_runtime_registry/utopia_runtime_registry.mli` |
| Modify | `bin/compiler.ml` (read and apply configuration) |
| Modify | `lib/utopia_server/utopia_server.ml` (apply rewrites, redirects) |
| Create | `bin/tests/config_default_without_utopia_ml.t` |
| Create | `bin/tests/config_custom_rewrites.t` |
| Create | `bin/tests/config_custom_redirects.t` |
| Create | `bin/tests/config_invalid_module.t` |
| Create | `bin/tests/config_runtime_registry_key_resolves.t` |
| Create | `bin/tests/config_runtime_registry_key_missing.t` |

---

## Acceptance criteria

- `utopia.ml` is optional
- When present, it is compiled and validated at build time
- Type errors in `utopia.ml` produce clear compiler errors
- Serializable config fields are transported through config runner output only
- Non-serializable runtime hooks/modules are resolved via linked runtime registry keys
- Rewrites work (URL is rewritten before route matching)
- Redirects work (301/302 responses)
- Default configuration is sensible and requires zero setup
- All tests pass
