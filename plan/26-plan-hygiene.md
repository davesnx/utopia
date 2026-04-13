# Plan hygiene

**Status**: Completed
**Priority**: Low
**Dependencies**: None

## Problem

The plan directory has accumulated stale, deprecated, and stub entries that reduce signal-to-noise:

| File | Issue |
|------|-------|
| `10-client-error-overlay.md` | Deprecated -- 12 lines, just a redirect to plan 11 |
| `13-not-found-page.md` | Stub (1 line) -- feature is already fully implemented |
| `16-remove-hardcoded-strings.md` | Stub (1 line) -- no content |
| `17-console-output.md` | Stub (1 line) -- no content |
| `18-caching-primitives.md` | Stub (1 line) -- no content |
| `roadmap.md` | Outdated -- still references `let rendering = \`Static\`` which was replaced by `let before` detection |

Additionally, completed plans (00-05, 07-08, 12, 14) should be clearly marked as completed at the top of each file, so readers don't need to cross-reference the spec.

## Plan

### Step 1: Delete deprecated/empty stubs

- Delete `10-client-error-overlay.md` (deprecated, content lives in plan 11)
- Delete `13-not-found-page.md` (feature is implemented, plan was never written)
- Delete `16-remove-hardcoded-strings.md` (now covered by plan 24)
- Delete `17-console-output.md` (empty)
- Delete `18-caching-primitives.md` (empty)

### Step 2: Update `roadmap.md`

Either:
- **Option A**: Delete it (the numbered plans serve as the roadmap)
- **Option B**: Regenerate it from the current plan status table

Recommendation: Option A. The numbered plans + spec are authoritative. A separate roadmap adds a maintenance burden with no unique value.

### Step 3: Mark completed plans

Add a `**Status**: Completed` line at the top of plans 00, 01, 02, 03, 04, 05, 07, 08, 12, 14. This matches the convention used in the new plans (19+).

### Step 4: Update plan 05 superseded note

Plan 05 references `pages/` + `api/` filesystem roots that are superseded by plan 14 (app directory). Add a clear "Historical context" header and note that the filesystem root info is stale while the API runtime behavior is still accurate.

## Verification

- All remaining plan files have a clear status line
- No orphan references (grep for deleted plan numbers in other plans)
- `plan/primitives.md` is unaffected

## Files modified

- Delete: `10-client-error-overlay.md`, `13-not-found-page.md`, `16-remove-hardcoded-strings.md`, `17-console-output.md`, `18-caching-primitives.md`, `roadmap.md`
- Edit: `00-cleanup.md`, `01-shared-types.md`, `02-compiler-rsc.md`, `03-server-rewrite.md`, `04-client-components.md`, `05-api-routes.md`, `07-ssg.md`, `08-dev-mode.md`, `12-optimization-for-melange-pages.md`, `14-app-directory-unification.md` (add status lines)
