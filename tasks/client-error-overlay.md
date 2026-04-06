# Client Error Overlay

## Goal

Implement a client-side error overlay that appears during development when browser-side rendering, hydration, navigation, or action flows fail.

## Scope

- Surface client-visible failures without forcing the user to inspect DevTools first.
- Show the error message, a short stack trace when available, and the failing operation context.
- Keep the overlay development-only so production behavior is unchanged.
- Allow dismissing the overlay and retrying the failed fetch/navigation when that is safe.

## Candidate Touchpoints

- Generated client entry runtime in `_utopia/client_entry.re`
- Router client runtime under `lib/utopia_runtime/files/Utopia_router.re`
- Client fetch / RSC response handling
- Server action client call path

## Implementation Checklist

- [ ] Audit the current client runtime paths where uncaught errors can happen during hydration, navigation, and server action calls.
- [ ] Introduce a small client-only overlay component/state holder for reporting the latest runtime error.
- [ ] Wire the overlay into the generated client entry so bootstrap/hydration failures render into the document reliably.
- [ ] Hook router navigation and server action failures into the same reporting path.
- [ ] Gate the overlay behind development mode so production builds keep the current behavior.
- [ ] Add focused regression coverage for the generated client/runtime output shape where feasible.
- [ ] Verify manually in a demo by triggering a client-visible failure and confirming the overlay appears.

## Acceptance Criteria

- A developer sees an in-browser overlay for client runtime failures during local development.
- The overlay includes enough context to identify the failing request, component tree edge, or action call.
- Dismissing the overlay does not require a full page reload unless the underlying runtime is unrecoverable.
- Production builds do not ship or render the overlay.
