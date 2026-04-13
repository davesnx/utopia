# Split Utopia_server.mlx

**Status**: Partial -- extracted Utopia_request_context (83 lines), Utopia_dev_events (276 lines), Utopia_route_match (117 lines). Utopia_server.mlx reduced from 2,225 to 1,760 lines. SSG/HTML/assets extraction deferred due to tight coupling with the rendering pipeline (would require circular dependencies or significant interface refactoring).
**Priority**: High
**Dependencies**: None

## Problem

`lib/utopia/Utopia_server.mlx` is a 2,225-line god module containing 15+ distinct responsibilities. It is the single largest architectural issue in the codebase. It mixes routing logic, SSG, asset serving, HTML rendering, SEO metadata, RSC normalization, request context, server actions, dev SSE events, route caching, HTTP helpers, and server startup.

This makes it:
- Hard to navigate and reason about
- Impossible to test individual concerns in isolation
- A magnet for accidental coupling between unrelated features
- A barrier to adding `.mli` interfaces (nobody will write a 2,225-line interface)

## Target state

Split into focused modules within `lib/utopia/`, each with a clear single responsibility. The `utopia` library dune stanza gains new module entries but remains `(wrapped false)`.

## Proposed modules

### 1. `Utopia_request_context.ml` (~70 lines)
Extract from lines 10-99. Request context for server components and server functions.
- `Request_context` module (types, Lwt keys, get/set)
- `with_render_context`, `with_action_context`, `serialize_pending_cookies`

### 2. `Utopia_route_match.ml` (~120 lines)
Extract from lines 443-553, 683-739. Route matching engine (also used by plan 20).
- `parse_matcher_segment`, `parse_matcher`
- `specificity_of_segment`, `compare_route_specificity`, `compare_api_route_specificity`
- `normalize_target`, `target_segments`, `strip_query_and_hash`, `path_segments`
- `render_matcher_segment`, `route_definition_of_segments`

### 3. `Utopia_assets.ml` (~100 lines)
Extract from lines 590-681. Static asset serving.
- `contains_path_traversal`, `normalize_asset_path`
- `first_existing_asset`, `content_type_for_asset`
- `serve_asset`

### 4. `Utopia_html.mlx` (~250 lines)
Extract from lines 775-1066. HTML page rendering + SEO metadata.
- `html_page` construction (head, body, scripts, styles)
- `render_metadata_tags` (OG, Twitter, robots, icons, verification)
- `available_stylesheet_paths`, `available_bootstrap_module_paths`

### 5. `Utopia_rsc.mlx` (~50 lines)
Extract RSC model normalization (the recursive element tree rewriting).
- `normalize_model_element`

### 6. `Utopia_dev_events.ml` (~100 lines)
Extract from lines 1597-1689+. Dev event SSE channel.
- Types: `dev_severity`, `dev_diagnostic`, `dev_build_status`, `dev_build_state`
- `json_escape_dev`, `format_dev_*` functions
- `dev_build_state_ref`, `dev_event_condition`, `dev_publish_token`
- `format_sse_event`

### 7. `Utopia_ssg.ml` (~170 lines)
Extract from lines 2000-2188. Static site generation.
- `ensure_directory`, `write_file`
- `ssg_output_path`, `render_ssg_page`, `copy_ssg_asset`
- `collect_ssg_tasks`, `run_ssg_tasks_parallel`, `ssg_generated`

### 8. `Utopia_server.mlx` (~1,300 lines, reduced from 2,225)
Retains the remaining orchestration:
- Type definitions (route_entry, api_route_entry, generated_route, module types)
- Generated route resolution (module types, builder/registry patterns)
- Route caching
- Server action handling
- API route handling
- HTTP response helpers (streaming, JSON, error formatting)
- Server startup (`start_runtime_routes`, `run_generated_cli`, etc.)
- Markdown registry

## Extraction order

1. `Utopia_request_context.ml` -- zero internal dependencies, cleanest extraction
2. `Utopia_dev_events.ml` -- self-contained, only uses stdlib
3. `Utopia_route_match.ml` -- depends only on `Utopia_types`
4. `Utopia_assets.ml` -- depends only on stdlib
5. `Utopia_html.mlx` -- depends on `Utopia_types`, server-reason-react
6. `Utopia_rsc.mlx` -- depends on server-reason-react
7. `Utopia_ssg.ml` -- depends on several of the above; extract last

## Verification

- `make build` succeeds
- All cram tests pass (`make test`)
- Each extracted module gets an `.mli` interface (see plan 23)
- The `(modules ...)` stanza in `lib/utopia/dune` is updated with new entries
- The `(install ...)` stanza is updated for any client-safe modules (though most extracted modules are server-only)

## Files modified

- `lib/utopia/Utopia_server.mlx` -- remove extracted code, add `open` for new modules
- `lib/utopia/dune` -- add new modules to `(modules ...)`
- New files: `lib/utopia/Utopia_request_context.ml`, `lib/utopia/Utopia_route_match.ml`, `lib/utopia/Utopia_assets.ml`, `lib/utopia/Utopia_html.mlx`, `lib/utopia/Utopia_rsc.mlx`, `lib/utopia/Utopia_dev_events.ml`, `lib/utopia/Utopia_ssg.ml`
