# Configuration

Implement the `utopia.ml` project configuration module.

---

## Goal

A project configuration file that is compiled and validated at build time. Type-safe, no runtime parsing of YAML/TOML/JSON. OCaml all the way down.

---

## Dependencies

- `plan/01-shared-types.md` -- shared types
- `plan/03-server-rewrite.md` -- server library accepts configuration

---

## Design the configuration API

Create a configuration library that users import. The `utopia.ml` file is an OCaml module that calls configuration functions:

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
    ~components:(module My_components : Utopia_config.Markdown_components)
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

This approach is type-checked by OCaml at compile time. No parsing needed.

---

## Create the configuration library

```
lib/
  utopia_config/
    dune
    utopia_config.ml
    utopia_config.mli
```

The library defines the configuration types and a global mutable config that gets populated when `utopia.ml` is evaluated:

```ocaml
type redirect_kind = Permanent | Temporary

type config = {
  mutable build_output_dir : string;
  mutable dune_profile : [ `Dev | `Release ];
  mutable server_port : int;
  mutable server_host : string;
  mutable rewrites : (string * string) list;
  mutable redirects : (string * string * redirect_kind) list;
  mutable markdown_components : (module Markdown_components) option;
}

let global_config = { (* defaults *) }
```

---

## Compile and load configuration

The compiler needs to evaluate `utopia.ml` to read the configuration. Options:

**Option A: Dynlink** -- compile `utopia.ml` and load it at compiler runtime. This is complex and fragile.

**Option B: Code generation** -- the compiler reads `utopia.ml` as source text and extracts configuration values via pattern matching on OCaml AST. This is also complex.

**Option C: Two-phase build** -- the compiler first compiles `utopia.ml` into a standalone executable that outputs configuration as JSON/sexp, then reads that output. This is the cleanest approach.

Go with **Option C**:

1. Compiler generates a `_utopia/config_runner.ml` that imports `utopia.ml` and serializes the config
2. `dune build _utopia/config_runner.exe`
3. Compiler runs `_utopia/config_runner.exe` and reads the output
4. Compiler uses the config to generate dune rules, manifests, etc.

---

## Apply configuration in the compiler

The compiler uses configuration values to:

- Set output directories in dune rules
- Add custom rewrites/redirects to the route manifest
- Pass markdown component overrides to the markdown rendering pipeline
- Set dune profile in build commands

---

## Apply configuration in the server

The server library reads configuration at startup to:

- Apply rewrites (URL rewriting before route matching)
- Apply redirects (return 301/302 responses)
- Set custom response headers
- Configure markdown component overrides

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

### Edge cases

- `utopia.ml` with no configuration calls (empty module)
- `utopia.ml` that calls configuration functions multiple times (last wins? error?)
- `utopia.ml` with unknown configuration fields
- Redirect loop detection (`/a` -> `/b` -> `/a`)
- Rewrite that targets a non-existent route
- Very large number of rewrites/redirects (1000+)
- Configuration that conflicts with CLI flags (CLI should win)

---

## Files changed

| Action | File |
|--------|------|
| Create | `lib/utopia_config/dune` |
| Create | `lib/utopia_config/utopia_config.ml` |
| Create | `lib/utopia_config/utopia_config.mli` |
| Modify | `bin/compiler.ml` (read and apply configuration) |
| Modify | `lib/utopia_server/utopia_server.ml` (apply rewrites, redirects) |
| Create | `bin/tests/config_default_without_utopia_ml.t` |
| Create | `bin/tests/config_custom_rewrites.t` |
| Create | `bin/tests/config_custom_redirects.t` |
| Create | `bin/tests/config_invalid_module.t` |

---

## Acceptance criteria

- `utopia.ml` is optional
- When present, it is compiled and validated at build time
- Type errors in `utopia.ml` produce clear compiler errors
- Rewrites work (URL is rewritten before route matching)
- Redirects work (301/302 responses)
- Default configuration is sensible and requires zero setup
- All tests pass
