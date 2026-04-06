# Client Runtime Overlay Slice

This plan defines the runtime-error slice of the unified browser overlay.

Authoritative overall dev-loop behavior lives in `plan/11-dev-full-reload-and-browser-overlay.md`.

## Goal

During development, show browser/runtime failures (hydration, navigation, server-action, uncaught errors) in the same overlay surface used for build diagnostics.

## Scope

- Runtime diagnostics only (build diagnostics come from plan 11 dev-event channel)
- Overlay mounted in its own client-only DOM root under `document.body`
- Development-only behavior; no production overlay code path
- Dismiss support for runtime errors; retry controls are optional and can be deferred

## Runtime Capture Points

- `lib/utopia_runtime/files/client_entry.re`
  - report bootstrap and hydration failures
- `lib/utopia_runtime/files/Utopia_router.re`
  - report rejected navigation/update promises with path context
- `lib/utopia_runtime/files/Utopia_call_server.re`
  - report action encode/fetch/decode failures with action context
- global handlers
  - `window.onerror`
  - `window.onunhandledrejection`

## Error Payload Contract (Runtime Slice)

Minimum fields:

- `operation` (`bootstrap` | `hydration` | `navigation` | `server_action` | `global_error`)
- `message`
- `stack` (optional)
- `context` (small object, e.g. current path, target path, action id, phase)
- `timestamp`

## Interaction Rules

- Runtime slice can be dismissed
- Dismiss does not mutate build state
- If build diagnostics are present, build diagnostics dominate overlay surface
- Retry buttons are optional for first implementation pass

## Implementation Checklist

- [ ] Add shared `Utopia_dev` runtime store for runtime diagnostic events
- [ ] Mount overlay root under `document.body` independently from app hydration root
- [ ] Wire `client_entry.re` failures into runtime diagnostics
- [ ] Wire router navigation failures into runtime diagnostics
- [ ] Wire server-action failures into runtime diagnostics
- [ ] Wire global `error`/`unhandledrejection` listeners into runtime diagnostics
- [ ] Gate everything behind development mode
- [ ] Add focused tests for payload shape + reporting hooks

## Acceptance Criteria

- Runtime failures appear in-browser during development without opening DevTools first
- Overlay payload includes enough context to identify failing phase/path/action
- Dismissing runtime errors does not require page reload when runtime remains recoverable
- Production builds do not expose/mount runtime overlay behavior
