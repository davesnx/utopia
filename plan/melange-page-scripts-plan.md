# Melange Page Scripts Plan

## Goal
Allow pages to declare browser scripts and have the build pipeline compile, map, and inject them reliably.

## Priority
This should be implemented before full `dev` orchestration, because `dev` depends on stable build and asset contracts.

## Execution phases

### Phase 1: Define the contract (first)
- Decide where script declarations live (page module metadata, frontmatter, or manifest config).
- Define normalized identifiers for route/page script entries.
- Define generated manifest schema (route/page -> js/css assets).

### Phase 2: Build integration (second)
- Generate melange entrypoints from page script declarations.
- Compile entries during `utopia build`.
- Write asset manifest as a generated artifact in `_utopia` (or equivalent output dir).

### Phase 3: Runtime integration (third)
- Load the manifest in server rendering.
- Inject script/style tags for each page based on route match.
- Keep deterministic ordering and deduplicate shared chunks.

### Phase 4: Validation and errors (fourth)
- Detect missing script entry declarations.
- Detect duplicate/ambiguous entries.
- Detect unresolved modules and invalid paths.
- Print actionable build errors with page + route context.

### Phase 5: Test coverage (fifth)
- Add fixture pages with and without scripts.
- Add integration test: build emits manifest + assets.
- Add runtime test: server output includes expected script tags.
- Add negative tests for validation failures.

## Definition of done
- A page can opt into scripts declaratively.
- `build` emits all required client assets and a manifest.
- `prod` serves/injects those assets correctly.
- Errors are explicit and route-aware.
- `dev` can later reuse the same contract in watch mode without redesign.

## Suggested immediate next step
- Implement Phase 1 + Phase 2 first (contract + build output), then wire minimal Phase 3 in `prod`.
