# Dev Full Reload And Browser Overlay

## Goal

Implement the first real `utopia dev` browser feedback loop:

- full page reload on successful rebuilds
- in-browser compiler/build diagnostics when a rebuild fails
- in-browser runtime diagnostics for hydration, navigation, and server-action failures
- fail-fast npm dependency validation before dev/build start

This task intentionally does **not** implement HMR, React state preservation, `Js.import` wrappers, or per-module hot swapping.

## Locked Decisions

- First milestone is **full reload only**.
- `utopia build` and `utopia dev` must build both the generated server executable and `@_utopia/esbuild`.
- Missing npm deps are a hard error. Do not auto-run `npm install`.
- Browser overlay must show compiler/build errors, not only runtime errors.
- Build warnings should be transmitted but collapsed by default; build errors are expanded.
- The dev server port must stay stable for the full dev session.
- Use a Utopia-owned dev event channel, not `dream-livereload`.
- Runtime overlay should live in a dedicated client-only root mounted under `document.body` so it still works when the main app hydration path is broken.
- `plan/11-dev-full-reload-and-browser-overlay.md` is authoritative for dev-loop behavior; `plan/09-dev-mode.md` and `plan/10-client-error-overlay.md` are subordinate implementation slices.

## Research Notes

### Existing repo behavior

- `bin/cli/Dev.ml` already starts the generated server executable and already connects to dune RPC via `bin/cli/Build_rpc.ml`.
- Current restart behavior is based on polling the generated server executable mtime.
- `lib/server/server.ml` already streams HTML through `ReactServerDOM.render_html` and includes bootstrap modules from `dist/client_entry_melange.js` when present.
- `server-reason-react`'s esbuild plugin already prepends generated `bootstrap.js` to the client entry and populates `window.__client_manifest_map` through `React.lazy(() => import(...))`.

### Melange notes for later HMR work

- `melange-re/melange#762` / `#1164` added dynamic import support.
- `melange-re/melange#1169` / `#1172` fixed ignored `Js.import` being optimized away by treating dynamic imports as side-effectful.
- Local switch uses `melange 6.0.1-51`, so these fixes are present.
- None of this is required for the first full-reload milestone. Wrapper-based HMR is explicitly deferred.

## Architecture

### 1. Build ownership

`utopia build` and `utopia dev` must explicitly build:

- the generated server executable
- `@_utopia/esbuild`

`dune build .` is not enough for this task because client bundle freshness is part of the contract.

### 2. Npm preflight

Before the initial compiler/build steps in both `build` and `dev`, validate from the project root that:

- `package.json` exists
- these packages resolve: `react`, `react-dom`, `esbuild`, `server-reason-react-esbuild-plugin`, `server-reason-react-server-dom-esbuild`

Failure mode: exit immediately with a clear error and an `npm install` hint.

### 3. Stable dev port

Choose the port once at dev startup and keep it pinned across restarts. If the restarted generated server cannot bind the same port later, fail the dev command with a clear error rather than silently hopping origins.

### 4. Dev event bridge

The CLI owns dune RPC diagnostics, but the browser talks to the generated server. Bridge them with:

- `GET /_utopia/dev-events` on the generated server for browser SSE subscriptions
- `POST /_utopia/dev-events` on the generated server for CLI-originated dev events

The POST endpoint must be dev-only and protected by a per-session secret token passed from the CLI to the generated server via environment.

### 5. Dev event channel

The generated server keeps an in-memory dev state and broadcasts it to connected browser SSE clients. New SSE subscribers should immediately receive the current state.

Minimum state for the first pass:

- `building`
- `failed` with current errors and warnings
- `healthy`

The browser should not infer build failure from reconnect behavior alone. Failed builds must arrive as explicit events from the CLI.

### 6. Reload behavior

- On `build_started`, keep showing the last build failure state but mark the overlay as rebuilding.
- On `build_failed`, update the overlay with the latest diagnostics. Do not restart the server. Do not reload.
- On `build_succeeded`, the CLI restarts the generated server.
- The browser detects SSE disconnect/reconnect and performs a full page reload after reconnecting to a healthy server.

No explicit fine-grained client update behavior is required in this phase.

### 7. Unified browser overlay

Add a dedicated generated runtime module, likely `Utopia_dev.re`, that owns:

- dev-mode detection
- SSE connection to `/_utopia/dev-events`
- build diagnostic state
- runtime error state
- global `error` and `unhandledrejection` listeners
- rendering the overlay into its own DOM node under `document.body`

The overlay has two independent slices:

- **Build diagnostics** from dune RPC via server SSE
- **Runtime diagnostics** from the browser runtime itself

Build diagnostics dominate the surface when present, because they explain why a reload is not happening.

### 8. Runtime error capture points

Capture runtime errors in these places:

- `lib/utopia_runtime/files/client_entry.re`
  - initialize the dev runtime early
  - report bootstrap and hydration failures
- `lib/utopia_runtime/files/Utopia_router.re`
  - catch rejected navigation promises
  - include current path, target path, and freshness/diff context when available
- `lib/utopia_runtime/files/Utopia_call_server.re`
  - catch failures in `encodeReply`, request fetch, and action response decoding
  - include action id and failing phase
- `window.onerror` and `window.onunhandledrejection`
  - catch uncaught browser/runtime failures that escape the explicit promise chains

### 9. Overlay interaction rules

- Build errors: read-only for now.
- Build warnings: collapsed by default.
- Hydration/bootstrap errors: dismiss only.
- Runtime navigation/action retry is optional and not required for this first task.
- Production builds must not ship or mount the overlay or expose the dev event endpoints.

## Structured Event Shape

The browser-facing payload should stay small and explicit. A reasonable first shape is:

```json
{
  "kind": "build_state",
  "build_id": 12,
  "status": "failed",
  "rebuilding": false,
  "errors": [
    {
      "severity": "error",
      "message": "Syntax error",
      "location": "pages/Home.re:3:7",
      "targets": ["pages/Home.re"]
    }
  ],
  "warnings": []
}
```

Runtime errors can use a separate payload shape internal to `Utopia_dev`, but should at least capture:

- operation kind
- message
- optional stack
- small context payload

## Implementation Checklist

- [ ] Update CLI build target selection so `utopia build` and `utopia dev` build the generated server executable and `@_utopia/esbuild`.
- [ ] Add fail-fast npm dependency validation before the initial compiler/build steps.
- [ ] Refactor `bin/cli/Build_rpc.ml` to expose structured build lifecycle + diagnostic events while preserving terminal output.
- [ ] Add a per-session dev publish token and pass it from the CLI to the generated server environment.
- [ ] Add dev-only server endpoints for SSE subscription and authenticated CLI event publishing.
- [ ] Store current dev build state in the generated server process and replay it to new SSE subscribers.
- [ ] Restart the generated server on every successful rebuild while keeping the selected port pinned.
- [ ] Add a generated browser dev runtime module for SSE, overlay state, and global error listeners.
- [ ] Mount the overlay in its own client-only root under `document.body`.
- [ ] Wire bootstrap/hydration failures in `client_entry.re` into the runtime overlay.
- [ ] Wire navigation failures in `Utopia_router.re` into the runtime overlay.
- [ ] Wire server-action failures in `Utopia_call_server.re` into the runtime overlay.
- [ ] Keep build errors expanded and warnings collapsed by default in the overlay UI.
- [ ] Add CLI/server/runtime/browser coverage for success reloads, failed builds, and runtime failures.
- [ ] Update `plan/09-dev-mode.md`, `plan/spec.md`, and `plan/primitives.md` to match the implemented behavior.

## Candidate File Touches

- `bin/cli/Artifacts.ml`
- `bin/cli/Build.ml`
- `bin/cli/Build_rpc.ml`
- `bin/cli/Dev.ml`
- `lib/server/server.ml`
- `lib/utopia_runtime/utopia_runtime.ml`
- `bin/compiler/Generated_dune.ml`
- `lib/utopia_runtime/files/client_entry.re`
- `lib/utopia_runtime/files/Utopia_router.re`
- `lib/utopia_runtime/files/Utopia_call_server.re`
- `lib/utopia_runtime/files/Utopia_dev.re` (new)
- `plan/09-dev-mode.md`
- `plan/spec.md`
- `plan/primitives.md`

## Test Plan

### CLI / build tests

- [ ] `utopia dev` fails immediately when required npm deps are missing.
- [ ] `utopia build` builds both the generated server executable and `@_utopia/esbuild`.
- [ ] `utopia dev` keeps the selected origin stable across successful rebuild restarts.

### Dev event tests

- [ ] The generated server exposes the SSE endpoint only in dev mode.
- [ ] The CLI publish endpoint rejects missing or invalid dev publish tokens.
- [ ] New SSE subscribers immediately receive the current dev build state.

### Browser overlay tests

- [ ] A failed build shows compiler diagnostics in-browser without reloading the page.
- [ ] Fixing the build clears the failed-build overlay and reloads the page.
- [ ] Hydration/bootstrap failures show the runtime overlay.
- [ ] Navigation failures show the runtime overlay with path context.
- [ ] Server-action failures show the runtime overlay with action context.
- [ ] Build warnings are visible but collapsed by default.

### Regression / environment tests

- [ ] Nested-project dev mode still resolves the correct generated server executable, npm deps, and event endpoints.
- [ ] Production mode does not expose dev endpoints or mount the overlay.

## Out Of Scope

- React state preservation across edits
- per-component HMR
- `Js.import` wrappers
- bootstrap-level module invalidation
- CSS-only hot swap without a full page reload
- upstream `server-reason-react` or Melange changes

## Acceptance Criteria

- Editing server code, client code, or CSS in `utopia dev` causes a full page reload after a successful rebuild.
- Failed rebuilds do not reload the page and instead show dune/compiler diagnostics in-browser.
- Runtime browser failures from hydration, navigation, and server-action paths show in the same overlay surface.
- `utopia build` and `utopia dev` stop early when required npm deps are missing.
- The dev origin stays stable for the full session.
- Production builds do not expose the dev event channel or render the overlay.
