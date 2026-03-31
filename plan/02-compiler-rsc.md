# Compiler RSC

Update the compiler to generate all artifacts needed for the React Server Components pipeline.

---

## Goal

The compiler currently generates dune rules for basic melange/native dual compilation. Extend it to emit the full RSC build pipeline: new PPXes, esbuild bundling rule, client entry, esbuild config, and per-project server executable.

---

## Dependencies

- `plan/00-cleanup.md` -- dead code removed
- `plan/01-shared-types.md` -- shared types extracted

---

## Update melange.emit stanza

Change the generated `melange.emit` stanza to include RSC-specific PPXes and libraries:

```scheme
(melange.emit
 (target target)
 (modules ...)
 (libraries reason-react server-reason-react.react-server-dom-esbuild)
 (preprocess
  (pps server-reason-react.browser_ppx -js
       reason-react-ppx
       server-reason-react.melange_ppx)))
```

Current stanza only uses `reason-react` and `reason-react-ppx`.

---

## Update native library stanza

Change the generated `library` stanza to include RSC-specific PPXes:

```scheme
(library
 (name pages)
 (modules ...)
 (libraries server-reason-react.react server-reason-react.reactDom)
 (preprocess
  (pps server-reason-react.ppx
       server-reason-react.melange_ppx
       melange-json-native.ppx)))
```

Remove the `(public_name utopia)` line (already addressed in cleanup, but enforce here).

---

## Generate client_entry.re

The compiler generates `_utopia/client_entry.re`, a Reason file that serves as the browser-side RSC shell:

```reason
[@mel.module "react-dom/client"]
external hydrateDocumentRoot: (Dom.document, React.element) => unit = "hydrateRoot";

[@mel.module "server-reason-react-server-dom-esbuild/client"]
external createFromFetch: Js.Promise.t(Fetch.Response.t) => React.element = "createFromFetch";

let document: Dom.document = Webapi.Dom.document;

let () = {
  let response = Fetch.fetch(
    Webapi.Dom.Window.location(Webapi.Dom.window) |> Webapi.Dom.Location.href,
    ~init=Fetch.RequestInit.make(
      ~headers=Fetch.HeadersInit.make({"Accept": "application/react.component"}),
      ()
    )
  );
  let tree = createFromFetch(response);
  hydrateDocumentRoot(document, tree)
}
```

This is a template. The exact bindings depend on what `server-reason-react` exposes via Melange. Because the streamed HTML/model root is the full document tree, the client hydrates the browser `document`, not just a `#root` subtree. The compiler writes this file and includes `client_entry_melange` in the melange.emit modules list.

---

## Generate esbuild.config.mjs

The compiler generates `_utopia/esbuild.config.mjs`:

```javascript
import esbuild from "esbuild"
import { plugin } from "server-reason-react-esbuild-plugin"

await esbuild.build({
  entryPoints: ["./target/client_entry_melange.js"],
  bundle: true,
  format: "esm",
  splitting: true,
  outdir: "./dist",
  plugins: [plugin({ targetDir: "./target" })]
})
```

The `targetDir` points to the melange output directory where `// extract-client` markers live.

---

## Generate server_main.ml

The compiler generates `_utopia/server_main.ml`, a per-project server executable that wires user page modules to the framework server library:

```ocaml
let () =
  let routes = Utopia_server.load_routes () in
  Utopia_server.start ~routes ()
```

The exact API depends on how the server library is structured (see `plan/03-server-rewrite.md`). The key point: this file imports user page modules (the `pages` library) and the framework server library, then starts the server.

The compiler knows all page modules and can generate explicit wiring:

```ocaml
let pages = [
  ("/", (module Home_native : Utopia_server.Page));
  ("/about", (module About_native : Utopia_server.Page));
]

let () = Utopia_server.start ~pages ()
```

---

## Generate esbuild dune rule

Add to the generated `_utopia/dune`:

```scheme
(rule
 (alias esbuild)
 (deps (alias melange) esbuild.config.mjs (file ../package.json))
 (action (run node esbuild.config.mjs)))
```

This rule depends on the melange alias (so melange compiles first) and the esbuild config.

---

## Generate server executable dune stanza

Add to the generated `_utopia/dune`:

```scheme
(executable
 (name server_main)
 (libraries utopia.server_lib pages dream lwt lwt.unix)
 (preprocess
  (pps server-reason-react.ppx
       server-reason-react.melange_ppx
       melange-json-native.ppx)))
```

---

## Include client_entry in melange modules

Add `client_entry_melange` to the melange.emit modules list. The compiler writes `client_entry.re` as a source file, then generates a copy rule like other pages:

```scheme
(rule
 (deps client_entry.re)
 (target client_entry_melange.re)
 (action (run cp %{deps} client_entry_melange.re)))
```

---

## Update route manifest format

Review whether the manifest format needs changes for RSC. The current format includes `layouts` as semicolon-separated source paths. For RSC, the server needs to know which modules to compose at render time. The manifest format may need to reference module names (e.g., `Home_native`, `About_native`) in addition to or instead of source file paths.

Add a `module_name` field (the native module name, derived from the filename) to each manifest entry. This lets the generated `server_main.ml` wire routes to modules.

New format:

```
<route>\t<kind>\t<source_file>\t<module>\t<matcher>\t<params>\t<layouts>
```

Where `<module>` is the native module name (e.g., `Home_native` for `pages/home.re`).

---

## Testing

### New cram tests

Create these test files:

**`compiler_generates_rsc_dune_rules.t`**
- Create a `pages/` directory with a `.re` page and a `lib/` module
- Run the compiler
- Assert the generated `_utopia/dune` contains:
  - `server-reason-react.browser_ppx -js` in melange preprocess
  - `server-reason-react.melange_ppx` in melange preprocess
  - `server-reason-react.ppx` in native library preprocess
  - `melange-json-native.ppx` in native library preprocess
  - `(executable (name server_main) ...)` stanza
  - `(rule (alias esbuild) ...)` stanza
  - No `(public_name ...)` in the library stanza

**`compiler_generates_client_entry.t`**
- Run the compiler
- Assert `_utopia/client_entry.re` exists and contains `createFromFetch`

**`compiler_generates_esbuild_config.t`**
- Run the compiler
- Assert `_utopia/esbuild.config.mjs` exists and contains `server-reason-react-esbuild-plugin`

**`compiler_generates_server_main.t`**
- Create pages: `index.re`, `about.re`
- Run the compiler
- Assert `_utopia/server_main.ml` exists and references both page modules

**`compiler_manifest_includes_module_names.t`**
- Create `pages/home.re` and `pages/about/team.re`
- Run the compiler
- Assert `_utopia/routes.manifest` includes module names like `Home_native` and `Team_native`

### Update existing cram tests

All compiler tests need promoted output to match the new dune rules format.

### Edge cases

- Empty `pages/` directory: compiler generates valid dune file with no page modules but still has client_entry, esbuild config, and server_main
- Pages with `lib/` folder: lib modules appear in both melange and native stanzas with correct namespacing
- Pages with dynamic segments: module names are derived correctly (e.g., `[id].re` becomes a valid OCaml module name)
- Deeply nested pages: module names don't collide
- `.mlx` pages: handled correctly in all stanzas
- Mixed `.re`, `.ml`, `.mlx` pages: all included correctly

---

## Performance

The compiler runs once per build (not in a hot loop). Focus on correctness over speed. However, avoid O(n^2) patterns when generating rules for large page sets.

---

## Files changed

| Action | File |
|--------|------|
| Modify | `bin/compiler.ml` (new dune rule generation, new file generation) |
| Create | `bin/tests/compiler_generates_rsc_dune_rules.t` |
| Create | `bin/tests/compiler_generates_client_entry.t` |
| Create | `bin/tests/compiler_generates_esbuild_config.t` |
| Create | `bin/tests/compiler_generates_server_main.t` |
| Create | `bin/tests/compiler_manifest_includes_module_names.t` |
| Modify | All existing compiler cram tests (promote output) |

---

## Acceptance criteria

- Compiler generates `_utopia/client_entry.re`, `_utopia/esbuild.config.mjs`, `_utopia/server_main.ml`
- Generated dune file contains RSC PPXes for both melange and native stanzas
- Generated dune file contains esbuild rule and server executable stanza
- Generated dune file has no `public_name` on the library
- Route manifest includes module names
- All cram tests pass
- Demo project compiles with the new generated rules (may require `server-reason-react` to be pinned)
