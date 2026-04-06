# Dev mode

This document is now a compact execution companion for core dev-loop mechanics.

Authoritative behavior and acceptance criteria live in `plan/11-dev-full-reload-and-browser-overlay.md`.

---

## Goal

Keep `utopia dev` fast and predictable while deferring all product-level dev UX decisions (overlay behavior, event payloads, diagnostics display) to plan 11.

---

## Status

- `plan/11-dev-full-reload-and-browser-overlay.md` is the source of truth for dev behavior
- This file only tracks lower-level restart/build wiring details that feed into plan 11

---

## Dependencies

- `plan/02-compiler-rsc.md` -- compiler-generated server executable and esbuild rules
- `plan/03-server-rewrite.md` -- per-project generated server executable
- `plan/11-dev-full-reload-and-browser-overlay.md` -- authoritative dev UX and event contract

---

## Per-project server executable lifecycle

`cmd_dev` must run the generated server executable from `_build/default/.../_utopia/server_main.exe` (project-relative for nested projects).

Core loop:

1. Build generated server executable and `@_utopia/esbuild`
2. Spawn generated executable
3. Monitor executable mtime
4. On successful rebuild + mtime change, restart process

Restart policy:

1. SIGTERM
2. Wait with timeout
3. SIGKILL if needed
4. Spawn replacement

Use ~500ms polling for executable mtime checks.

---

## Build/watch integration baseline

`utopia dev` owns dune watch + RPC wiring and emits structured lifecycle hooks consumed by plan 11's dev-event bridge:

- `build_started`
- `build_failed` (diagnostics attached)
- `build_succeeded`

Reload and browser-overlay semantics are defined by plan 11, not this file.

---

## Npm preflight baseline

Before initial `build` or `dev` execution:

1. Require `package.json`
2. Require resolvable deps: `react`, `react-dom`, `esbuild`, `server-reason-react-esbuild-plugin`, `server-reason-react-server-dom-esbuild`
3. On failure, exit immediately with clear remediation (`npm install`)

Do not auto-run `npm install`.

---

## Port/origin stability baseline

`utopia dev` chooses a port once at startup and keeps that origin stable across restarts. If a restart cannot bind the pinned port, fail with a clear error instead of hopping to another port.

---

## `utopia prod` alignment

`utopia prod` uses the same generated per-project executable path strategy as `utopia dev`, but without dev-only event channels or overlay/runtime injection.

---

## Testing

### Cram tests

**`dev_starts_per_project_server.t`**
- Verifies generated server executable path is used

**`dev_restart_on_rebuild.t`**
- Verifies successful rebuild restarts the generated server

**`dev_requires_npm_deps.t`**
- Verifies fail-fast npm preflight (no auto-install)

**`dev_port_is_stable_across_restart.t`**
- Verifies dev origin stays pinned across restarts

**`prod_uses_per_project_server.t`**
- Verifies prod launches generated server executable

### Edge cases

- Server executable missing at startup
- Server executable removed between rebuilds
- Restart timeout and forced kill path
- Failed rebuild (no restart)
- SIGINT/SIGTERM during restart window
- Pinned port unavailable on restart

---

## Acceptance criteria

- This document does not conflict with `plan/11-dev-full-reload-and-browser-overlay.md`
- `utopia dev` launches and restarts the generated server executable correctly
- Build/watch lifecycle hooks are available for the plan-11 event bridge
- npm preflight is fail-fast (no auto-install)
- Dev origin remains stable for the full session
- `utopia prod` uses generated per-project server executable
