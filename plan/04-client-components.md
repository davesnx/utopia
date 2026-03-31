# Client components

Implement client component boundary detection, the esbuild bundling pipeline, and client-side hydration.

---

## Goal

Make `[@react.client.component]` work end-to-end. A module annotated with this attribute gets compiled to JavaScript via Melange, bundled by esbuild with code splitting, and hydrated on the client via the RSC protocol.

---

## Dependencies

- `plan/02-compiler-rsc.md` -- compiler generates esbuild config and client entry
- `plan/03-server-rewrite.md` -- server renders RSC payloads with client component references

---

## Understand the existing PPX pipeline

The heavy lifting is done by `server-reason-react`:

**Native side** (`server-reason-react.ppx`): transforms `[@react.client.component]` modules to emit `React.Client_component { import_module; props; client }` during server rendering. The server's RSC renderer includes a client component reference in the RSC payload instead of rendering the component's body.

**Melange side** (`server-reason-react.browser_ppx -js`): compiles the component to JS normally and emits `// extract-client <path> <module>` comments plus a `make_client` function.

**Extract step** (`server-reason-react.extract_client_components`): scans Melange `target/` output for `// extract-client` markers and generates `bootstrap.js`.

Utopia's job is to wire these tools together correctly in the build pipeline. The PPXes and extraction tool come from `server-reason-react`.

---

## Verify PPX configuration

Ensure the compiler generates the correct PPX configuration (from `plan/02-compiler-rsc.md`):

- Native stanza: `server-reason-react.ppx`, `server-reason-react.melange_ppx`, `melange-json-native.ppx`
- Melange stanza: `server-reason-react.browser_ppx -js`, `reason-react-ppx`, `server-reason-react.melange_ppx`

The order matters. `browser_ppx -js` must run before `reason-react-ppx`.

---

## Verify esbuild pipeline

The esbuild config (generated in phase 2) must:

1. Use the `server-reason-react-esbuild-plugin` which runs `extract_client_components`
2. Set entry points to include the client entry
3. Enable code splitting (`splitting: true`)
4. Output to `dist/` directory
5. Use ESM format (required for code splitting)

Verify the generated `esbuild.config.mjs` produces correct output by running it manually against a test project with a client component.

---

## Verify bootstrap.js generation

The `server-reason-react-esbuild-plugin` generates `bootstrap.js` which populates `window.__client_manifest_map`. Verify the map entries match what the server emits in the RSC payload.

---

## RSC boundary serialization

Props crossing the server-to-client boundary must be JSON-serializable. The `melange-json` (client) and `melange-json-native` (server) libraries handle this via PPX-generated serializers.

Ensure both libraries are:
1. Listed as dependencies in the generated dune stanzas
2. Available as PPXes in the correct stanzas
3. Properly installed in the opam switch

---

## Add melange-json dependencies

Update `dune-project` to include `melange-json` and `melange-json-native` as dependencies. Update the opam file.

---

## Create an end-to-end test project

Create `demo/rsc/` with:

```
demo/rsc/
  dune
  package.json
  pages/
    layout.re
    index.re
    lib/
      counter.re    # [@react.client.component]
```

Where `counter.re` is:

```reason
[@react.client.component]
[@react.component]
let make = (~initial_count) => {
  let (count, set_count) = React.useState(() => initial_count);
  <button onClick={_ => set_count(c => c + 1)}>
    {React.string(string_of_int(count))}
  </button>
}
```

And `index.re` uses it:

```reason
[@react.component]
let make = () => {
  <div>
    <h1> {React.string("RSC Demo")} </h1>
    <Counter initial_count=0 />
  </div>
}
```

---

## Testing

### Cram tests

**`client_component_compiles_both_targets.t`**
- Create a page with `[@react.client.component]`
- Run the compiler
- Run `dune build`
- Verify both native and melange outputs exist
- Verify melange output contains `// extract-client` marker

**`client_component_props_serialization.t`**
- Create a client component with typed props (string, int, list)
- Run the full build pipeline
- Verify the build succeeds (melange-json generates serializers correctly)

**`client_component_in_lib.t`**
- Place a client component in `lib/`
- Verify it's available to both pages and other lib modules
- Verify esbuild bundles it correctly

### Edge cases

- Client component with no props
- Client component with complex props (records, variants)
- Client component with optional props
- Client component that imports another client component
- Server component that renders multiple different client components
- Client component in a nested directory
- Client component with the same name as a server component in a different directory
- Very large client component (verify code splitting works)
- Client component that uses React hooks (useState, useEffect, useRef)
- Client component with a server function prop

### Integration test

If possible, create an automated browser test that:
1. Starts the server
2. Loads a page with a client component
3. Verifies the component hydrates (button click works)
4. Navigates client-side to another page
5. Verifies the RSC payload is fetched (not a full page reload)

This may require a headless browser setup (playwright or similar).

---

## Performance

- esbuild code splitting ensures each page only loads the client components it uses
- Dynamic `import()` calls create separate chunks per client component
- The client manifest map uses `React.lazy` for lazy loading
- Verify bundle sizes are reasonable for the demo project

---

## Files changed

| Action | File |
|--------|------|
| Modify | `dune-project` (add melange-json dependencies) |
| Modify | `utopia.opam` (add melange-json dependencies) |
| Create | `demo/rsc/dune` |
| Create | `demo/rsc/package.json` |
| Create | `demo/rsc/pages/layout.re` |
| Create | `demo/rsc/pages/index.re` |
| Create | `demo/rsc/lib/counter.re` |
| Create | `bin/tests/client_component_compiles_both_targets.t` |
| Create | `bin/tests/client_component_props_serialization.t` |
| Create | `bin/tests/client_component_in_lib.t` |

---

## Acceptance criteria

- A page with a `[@react.client.component]` builds successfully (both native and melange)
- The melange output contains `// extract-client` markers
- esbuild produces bundled output in `dist/` with code splitting
- `bootstrap.js` is generated with correct manifest map entries
- The server renders an RSC payload that references the client component by module ID
- The client entry hydrates the page and the client component becomes interactive
- Props are correctly serialized across the server-client boundary
- The demo/rsc project builds and runs end-to-end
