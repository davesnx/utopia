# Extract shared routing logic

**Status**: Completed -- `Utopia_route_match` created with shared primitives. Compiler's `Generated_routes.ml` and `Routes.ml` now use the shared module. Benchmark duplication is intentional (self-contained, per bench comments). `json_escape`/`file_mtime` consolidation skipped: CLI and server don't share a dependency, and the functions are 1-line utilities where duplication is cheaper than adding a library dependency.
**Priority**: High
**Dependencies**: 19-split-utopia-server (specifically `Utopia_route_match`)

## Problem

Route matching logic is duplicated across three locations:

1. **`lib/utopia/Utopia_server.mlx`** (lines 443-553, 683-739) -- runtime route matching
2. **`bin/compiler/Generated_routes.ml`** (lines 21-45) -- compile-time route tree sorting
3. **`bench/bench_routing.ml`** (lines 115-155) -- benchmark harness

Duplicated functions:
- `specificity_of_segment` -- 3 copies (server, compiler, bench)
- `compare_route_specificity` -- 3 copies (server, compiler, bench)
- `parse_matcher_segment` / `parse_matcher` -- 2 copies (server, bench)
- `render_matcher_segment` -- 2 copies (server:730, compiler `Routes.ml`:110)

Additionally, `json_escape` has 3 copies:
- `lib/utopia/Utopia_server.mlx:1450` (full version)
- `lib/utopia/Utopia_server.mlx:1634` (`json_escape_dev`, simplified)
- `bin/cli/Dev.ml:12` (copy of `json_escape_dev`)

And `file_mtime` has 2 copies:
- `lib/utopia/Utopia_server.mlx:440`
- `bin/cli/Process.ml:27`

## Target state

A shared `Utopia_route_match` module in `lib/utopia/` (created in plan 19) that the compiler, server, and benchmark all depend on. This module lives in the `utopia` library and contains all route matching primitives.

For `json_escape`: consolidate into a single `Utopia_json` or `Utopia_string` helper module, or just keep one copy in the appropriate module and reference it.

For `file_mtime`: add to `utopia_path` library (which both CLI and server can depend on) or extract a tiny shared utility.

## Plan

### Step 1: Create `Utopia_route_match.ml` (from plan 19)

This module provides:
```ocaml
(* lib/utopia/Utopia_route_match.ml *)
open Utopia_types

val parse_matcher_segment : string -> (route_segment, string) result
val parse_matcher : string -> (route_segment list, string) result
val specificity_of_segment : route_segment -> int
val compare_specificity : route_segment list -> route_segment list -> int
val normalize_target : string -> string
val target_segments : string -> string list
val strip_query_and_hash : string -> string
val path_segments : string -> string list
val render_matcher_segment : route_segment -> string
val route_definition_of_segments : route_segment list -> string
```

### Step 2: Update consumers

1. **`Utopia_server.mlx`**: Replace inline definitions with calls to `Utopia_route_match.*`
2. **`bin/compiler/Generated_routes.ml`**: Replace `specificity_of_segment` and `compare_route_specificity` (lines 21-45) with `Utopia_route_match.specificity_of_segment` and a wrapper that maps segments first
3. **`bin/compiler/Routes.ml`**: Replace `render_matcher_segment` (line 110) with `Utopia_route_match.render_matcher_segment`
4. **`bench/bench_routing.ml`**: Replace all inline copies with calls to the shared module

### Step 3: Consolidate `json_escape`

Keep one implementation in `Utopia_server.mlx` (the full version at line 1450). Have `Dev.ml` call `Utopia_server.json_escape` -- but wait, the CLI does NOT depend on `utopia` library. Two options:

**Option A** (preferred): Move `json_escape` to a tiny shared module in `utopia_path` or a new `utopia_utils` library that both CLI and server depend on.

**Option B**: Keep two copies but with a comment noting the intentional duplication (CLI avoids linking the full server runtime). The bench intentional-duplication precedent applies here.

### Step 4: Consolidate `file_mtime`

Same approach as `json_escape` -- move to `utopia_path` since both CLI and server already depend on it.

## Verification

- `make build` succeeds
- All cram tests pass
- `make bench` still works
- Grep for duplicate function names to confirm they're gone

## Files modified

- `lib/utopia/Utopia_route_match.ml` (new, from plan 19)
- `lib/utopia/Utopia_route_match.mli` (new)
- `lib/utopia/Utopia_server.mlx` -- remove duplicated code
- `bin/compiler/Generated_routes.ml` -- import from shared module
- `bin/compiler/Routes.ml` -- import from shared module
- `bench/bench_routing.ml` -- import from shared module
- `lib/utopia_path/utopia_path.ml` -- add `file_mtime` (and possibly `json_escape`)
- `bin/cli/Dev.ml` -- import `json_escape` from shared location
