# Utopia CLI Subcommands Plan

## Goal
Ship the CLI in a safe order so foundational build/runtime behavior exists before workflow orchestration.

## Recommended implementation order
1. `build`
2. `prod` (alias: `start`)
3. `dev`
4. `clean`
5. `info`

## Why this order
- `build` defines artifacts and manifests that every other command depends on.
- `prod` verifies runtime behavior on top of real build output.
- `dev` should orchestrate existing pieces, not invent core behavior.
- `clean` and `info` are low-risk support commands once paths/contracts are stable.

## Command-by-command scope

### `build` (first)
- Validate project shape and required directories/files.
- Generate route manifest and generated dune rules.
- Build server + melange client outputs.
- Emit a build report (routes, entries, assets, output directories).
- Fail fast on route conflicts, missing entries, invalid script references.

### `prod` (second)
- Verify required build artifacts exist.
- Start production server with static asset serving.
- Resolve page -> asset mapping from generated manifest.
- Respect env vars/flags (`PORT`, `HOST`) and print startup diagnostics.

### `dev` (third)
- Run initial compile/build bootstrap.
- Start watch processes (compiler + dune/melange).
- Start local server and stream prefixed logs.
- Handle process lifecycle (restart/teardown on failure/exit).
- Add flags later (`--port`, `--host`, `--no-watch`, `--verbose`).

### `clean` (fourth)
- Remove generated artifacts (`_build`, `_utopia`, client asset output).
- Support selective cleanup (`--client`, `--server`, `--all`) later.
- Print exactly what was removed.

### `info` (fifth)
- Print tool versions (OCaml, dune, melange, utopia).
- Print detected project paths and mode.
- Print command support status (scaffolded vs implemented).

## Milestones
- M1: `build` + manifest contract considered stable.
- M2: `prod` runs fully from manifest and compiled artifacts.
- M3: `dev` orchestrates `build`/watch/server with clean logs.
- M4: `clean`/`info` UX polish.
