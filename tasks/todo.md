# Execution Plan

## Active slice

- [completed] Prototype a `demo/notes/_dune` source-ownership experiment so `lib/` belongs to real Dune stanzas for editor/LSP work
- [completed] Rewire the checked-in `demo/notes/_utopia/dune` native build to depend on the source `lib/` library instead of mirrored `Lib__*` copies
- [completed] Add a focused Merlin config for `demo/notes/pages/` and measure how far source `.mlx` diagnostics get, especially for dynamic filenames like `[tag].mlx`
- [completed] Verify the experiment with targeted `dune build` and `ocamllsp` probes, then record the working limits and fallout

## Review

- The compiler now emits a single `_utopia/dune` file that carries both the runtime build stanzas and the source-owned editor-support stanzas. Root projects opt into it with `(include _utopia/dune)` plus `(data_only_dirs _utopia)`.
- `demo/notes/dune` and `demo/blog/dune` now use the single generated `_utopia/dune` flow instead of the earlier `_dune/dune.inc` experiment.
- The updated cram coverage in `bin/tests/compiler_generates_source_lib_support.t` and `bin/tests/compiler_generates_source_page_support.t` now proves the compiler emits source-owned `lib/` and static-page stanzas into `_utopia/dune` itself.
- Direct `ocamllsp` probing with an injected type error now reports source diagnostics for `demo/notes/lib/notes_data.ml`, so the previous `No config found` failure is resolved for source-owned `.ml` shared-lib files.
- A real source-owned `.mlx` file under `demo/notes/lib/button.mlx` no longer hits the `No config found` path, but `ocamllsp` still reports an `mlx-pp` migration error plus bogus syntax/hole diagnostics there. That points at the current Merlin/`mlx` tooling path rather than Dune ownership.
- The remaining invalid-basename dynamic routes were migrated to directory-based params (`demo/notes/pages/notes/[tag]/index.mlx` and `demo/blog/pages/posts/[slug]/index.mlx`). After also switching source-owned page libraries to `-shared-folder-prefix=`, `ocamllsp` now reports direct type diagnostics on those moved `.mlx` files too.
- Generated demo code now depends on a real shared `utopia` library for reusable runtime modules, while `_utopia/Utopia.re` is reduced to a tiny project wrapper over `Utopia_base` plus generated `Utopia_routes`. The public route API also moved from `Utopia.Routes.Current/current` to `Utopia.Routes.t/of_route`, and router links now use `Utopia.Router.Link`.

## Active slice

- [completed] Audit `tasks/` notes, `plan/spec.md`, and the live compiler/CLI/server code for spec drift
- [completed] Refresh `plan/spec.md` so implemented behavior matches the current build, server-function, SSG, and dev/runtime flow
- [completed] Add explicit spec coverage for generated `_utopia/dune` structure and current dune RPC usage in `utopia dev`
- [completed] Sync `plan/primitives.md` with any terminology or behavior changes introduced by the spec refresh

## Review

- `plan/spec.md` now reflects the implemented server-function flow, current RSC/SSG behavior, generated-server request handling, route-manifest format, and the actual `utopia build` / `utopia dev` / `utopia prod` behavior.
- The spec now includes explicit sections for how `_utopia/dune` is generated, what stanzas it contains, how the Melange/native/esbuild pieces fit together, and how dune RPC is used by `utopia dev` today.
- `plan/primitives.md` was synced for the updated route manifest shape, client entry/runtime support files, SSG semantics, server library/executable wording, current dev-mode behavior, and a new `Dune RPC` glossary entry.

## Active slice

- [completed] Capture the agreed first-pass dev task in `tasks/dev-full-reload-and-browser-overlay.md`
- [pending] Teach `utopia build` and `utopia dev` to fail fast on missing npm deps and explicitly build `@_utopia/esbuild`
- [pending] Replace the old live-reload plan with a dev event channel that carries build diagnostics to the browser
- [pending] Add a unified development overlay for build failures plus hydration/navigation/action runtime failures

## Completed

- [completed] Phase 00 cleanup and legacy removal
- [completed] Phase 01 shared types extraction into `lib/utopia_types/`
- [completed] Phase 02 compiler-generated RSC artifacts and dune stanzas
- [completed] Phase 03 server extraction into `lib/utopia_server/`
- [completed] Upgraded local opam toolchain to `server-reason-react` `0.4.1` stack with `reason-react`, `reason-react-ppx`, `melange-json`, and `melange-json-native`
- [completed] Switched GET HTML to `ReactServerDOM.render_html`
- [completed] Switched GET RSC payloads to `ReactServerDOM.render_model_value`
- [completed] Added generated local `ReactServerDOMEsbuild` wrapper and native `FunctionReferences` support files in compiler output
- [completed] Fixed generated native+melange server-function compilation by mirroring page/lib sources with stable SRR path prefixes and JSON preludes
- [completed] Confirmed supported page syntax for server functions uses explicit `` `Function(...)`` action values behind `switch%platform`
- [completed] Added passing compiler coverage for a real `[@react.server.function]` page fixture

## Current blockers

- [completed] Investigate `server_reason_react_ppx` fatal error when compiling a real `[@react.server.function]` fixture through generated `_utopia/dune`
- [completed] Confirm the exact supported Reason syntax and module shape for SRR server functions in user pages
- [completed] Verify whether additional native/melange support modules or dune stanza changes are required for PPX-transformed server functions

## Active slice

- [completed] Rework generated `_utopia/dune` so native and melange builds compile mirrored page copies with stable SRR file IDs
- [completed] Align generated SRR support code with upstream expectations (`FunctionReferences.re`, `melange-json` PPX/runtime deps, `-shared-folder-prefix`, `Melange_json.Primitives` availability)
- [completed] Update generated route wiring/tests/primitives and prove the real server-function fixture builds with `dune build @melange _utopia/server_main.exe`

## Next implementation steps

- [pending] Finish compiled route wiring in `_utopia/server_main.ml` so code pages and layouts render through compiled native modules, not source placeholders
- [completed] Finish real POST action handling in `lib/utopia_server/utopia_server.ml` using the generated `FunctionReferences.get`
- [completed] Wire generated client `callServer` flow end-to-end once the PPX fixture compiles cleanly
- [completed] Add an end-to-end build test for a page that defines and uses a real `[@react.server.function]`
- [completed] Update CLI `prod`/`dev` flows to prefer generated `_utopia/server_main.exe` for compiled-page/runtime correctness

## Active slice

- [completed] Switch CLI `prod`/`dev` server launch paths to generated `_utopia/server_main.exe`
- [completed] Add CLI coverage proving generated server executable is preferred over `utopia.server`
- [completed] Update primitives/review notes and re-run build/test verification

## Active slice

- [completed] Add explicit `utopia dev` coverage for rebuilds triggered by edits under `pages/`
- [completed] Add explicit `utopia dev` coverage for rebuilds triggered by edits under `pages/lib/`
- [completed] Re-run targeted and full verification after the new dev rebuild coverage lands

## Verification targets

- [completed] `dune build _utopia/server_main.exe @melange` succeeds for a real server-function fixture
- [completed] Generated demo/basic project builds cleanly with compiled page wiring enabled
- [completed] Add/request-level test for successful POST with `X-Action-ID` and encoded args
- [completed] Add CLI-level coverage for `utopia prod`/`utopia dev` using the generated server executable
- [completed] Add CLI-level coverage for `utopia dev` rebuilds after `pages/` and `pages/lib/` edits

## Active slice

- [completed] Reproduce why demo pages reference `/dist/client_entry_melange.js` when no client bundle is available
- [completed] Make bootstrap asset lookup robust for generated and checked-in `_utopia` layouts, and stop emitting a broken bootstrap URL when the bundle is absent
- [completed] Add regression coverage for optional bootstrap injection and asset serving, then verify the demo route no longer points at a missing file

## Active slice

- [completed] Audit compiler hotspots that still hand-concatenate dune stanzas and choose a structured sexp builder
- [completed] Refactor `_utopia/dune` generation to build `Sexplib0.Sexp` values and serialize them instead of stitching strings
- [completed] Update dependency/test/primitives coverage and verify generated demo + fixture builds still succeed

## Active slice

- [completed] Extract the new `Dune_sexp` helpers out of `bin/compiler.ml` into a dedicated library module
- [completed] Rewire the compiler to depend on the library rather than `sexplib0` directly, and document the new abstraction
- [completed] Re-run the compiler/demo verification after the extraction to prove behavior is unchanged

## Active slice

- [completed] Add an explicit `dune_sexp.mli` so the library exposes only the small constructor/render surface the compiler actually needs
- [completed] Tighten the documented `dune_sexp` abstraction to match the narrowed interface
- [completed] Re-run compiler/demo verification after the interface cleanup

## Review

- `dune build bin/compiler.exe` passes on the upgraded toolchain.
- `dune runtest bin/tests markdown/tests` passes after the server-function compiler fixes.
- `dune build` now passes end-to-end with regenerated `demo/basic/_utopia` artifacts and the new action handling/runtime changes.
- The generated `_utopia/dune` now mirrors page/lib sources into root melange targets and `_utopia/native/` native targets, using SRR-compatible shared-folder prefixes.
- Real `[@react.server.function]` fixtures compile when page actions use SRR's supported explicit action value syntax (`switch%platform` with `` `Function(...)`` on the server branch).
- Successful POST action coverage now exists for both encoded request bodies and multipart form-data through generated `_utopia/server_main.exe`.
- `utopia prod` now refuses to start without `_build/default/_utopia/server_main.exe` and launches that generated executable when artifacts are present.
- `utopia dev` now launches the generated server executable, and watch mode restarts it automatically when `_build/default/_utopia/server_main.exe` changes.
- `utopia dev` rebuild coverage now explicitly proves hot rebuilds after editing both `pages/Home.re` and `pages/lib/Message.re`.
- `dune build` now passes at repo root after regenerating checked-in demo compiler artifacts with the fixed `_utopia/dune` layout.
- Research for phase 02/03 shows the installed `server-reason-react` exposes public `ReactServerDOM.*` APIs and demo `DreamRSC.re` wrappers, not a packaged `DreamRSC` module.
- Standalone demo/source-mode servers now only emit the bootstrap client script when `dist/client_entry_melange.js` is actually present, and asset lookup also checks generated `_utopia` build directories.
- `dune build @bin/tests/runtest` passes with regression coverage for both missing-bundle omission and serving a present `_utopia/dist/client_entry_melange.js` asset.
- The compiler now emits `_utopia/dune` from structured `Sexplib0.Sexp` stanzas instead of manual string concatenation, which removes hand-rolled dune escaping and keeps stanza assembly typed.
- `dune build bin/compiler.exe utopia.opam demo/basic/_utopia/server_main.exe @demo/basic/_utopia/melange` passes after adding `sexplib0` to the compiler/dependency graph.
- Manual fixture checks still match the expected generated `_utopia/dune` structure for basic, nested-page, and `.mlx` projects after the sexp refactor.
- The dune sexp helpers now live in a dedicated internal library at `lib/dune_sexp/`, so `bin/compiler.ml` consumes `Dune_sexp` as a normal library module instead of defining it inline.
- `bin/compiler.exe` still builds cleanly and the extracted-library refactor preserves generated `_utopia/dune` output for the basic, nested-page, `.mlx`, and checked-in `demo/basic` paths.
- `lib/dune_sexp/dune_sexp.mli` now narrows the public API to the constructor/render helpers the compiler actually uses, while the implementation keeps lower-level helpers private.

## Active slice

- [completed] Create a dedicated `demo/notes/` project so the Utopia Notes demo does not overwrite the existing `demo/basic/` worktree changes
- [completed] Add sqlite-backed note data helpers, seed data, and async/suspense-friendly server components for the notes demo
- [completed] Build the nested notes routes and layouts under `/notes/*` with an Utopia Notes-inspired shell and note detail panels
- [completed] Verify the compiler/build/runtime flow in an isolated workspace, and capture the remaining RSC GET limitation below

## Review

- Added a new standalone demo project at `demo/notes/` with nested folder layouts (`inbox`, `pinned`, `archive`), sqlite-backed note content, and streamed suspense panels for note insights/related notes.
- The note data now seeds itself into `demo/notes/notes.sqlite3` on first server request, so the demo is runnable without checking in a binary database file.
- Regenerating both `demo/basic/_utopia/dune` and `demo/notes/_utopia/dune` now produces project-scoped native library names (`pages_demo_basic`, `pages_demo_notes`), so both demos build together inside the same Dune workspace without collisions.
- GET `Accept: application/react.component` responses now normalize compiled `React.DangerouslyInnerHtml` children into DOM `dangerouslySetInnerHTML` props before model serialization, which restores compiled-route RSC payloads for both the minimal regression case and the notes demo route.
- In-repo verification now passes for `dune build demo/basic/_utopia/server_main.exe demo/notes/_utopia/server_main.exe @demo/basic/_utopia/melange @demo/notes/_utopia/melange`, and `/notes/inbox/daily-brief` returns both a streamed HTML page and a non-empty RSC payload with the suspense cards.

## Active slice

- [completed] Scope generated `_utopia` native library names per project path so `demo/basic` and `demo/notes` can build inside the same workspace
- [completed] Normalize compiled `DangerouslyInnerHtml` children before GET RSC serialization so generated page modules can return `application/react.component` payloads
- [completed] Add regression coverage for both the project-scoped pages library naming and compiled-route RSC GET payloads, then re-run targeted verification

## Active slice

- [completed] Generate a project-local `Utopia` router surface and wire compiled routes so direct loads SSR the requested page while client navigation can update via RSC
- [completed] Teach the generated server runtime to return either a full route tree or a parent-relative diff tree for `Accept: application/react.component` requests
- [completed] Update the demos to use the public `Utopia.useRouter()` API plus delegated `.js-route-link` SPA navigation, and verify nested notes routes swap through diff requests
- [completed] Refresh compiler/server/primitives coverage and record the verification results for the router work

## Review

- Generated projects now include a public `Utopia` module that exposes `Utopia.useRouter()` while the internal router/runtime files stay generated under `_utopia/`.
- The generated server responds to `Accept: application/react.component` requests with either a full tree payload or a diff payload keyed by `X-Utopia-Current-Path`, so nested SPA navigation can swap only the changed branch.
- `lib/utopia_server/utopia_server.ml` now normalizes compiled `DangerouslyInnerHtml` nodes inside router-managed client props and HTML fallbacks, which fixes compiled-route SSR/RSC payloads for both `demo/basic` and `demo/notes`.
- `demo/basic` and `demo/notes` now both demonstrate explicit programmatic navigation through `Utopia.useRouter()`, rendering button-based navigation surfaces instead of source-level `href` links.
- Targeted verification passes for router-related compiler tests plus live requests against `demo/basic/_utopia/server_main.exe` and `demo/notes/_utopia/server_main.exe`, including diff RSC responses.
- The missing SPA behavior in `demo/` came from the client bundle never being built or loaded; the fix was to add JS runtime dependencies, exclude `node_modules/` from Dune scanning, and correct the generated esbuild config so nested demo targets bundle `client_entry_melange.js` into `_utopia/dist/`.

## Active slice

- [completed] Reproduce why `.js-route-link` anchors in `demo/` still trigger full page reloads instead of router-driven SPA navigation
- [completed] Fix the generated client router interception so same-origin link clicks call `Utopia.useRouter().navigate` reliably
- [completed] Re-run focused verification proving the demo HTML now loads the client bundle and the built bundle contains the delegated click interception path

## Investigation note

- [completed] Inspect upstream `server-reason-react` PPX/tests for the native shape of `[@react.client.component]` and whether generated `_utopia/server_main` can call client-component `make` functions directly

## Review

- Upstream native PPX output rewrites `[@react.client.component]` so `make` itself returns `React.Client_component { import_module; props; client }`; post-PPX generated OCaml can therefore call `Module.make ~props ... ()` directly, provided it passes serializable props and concrete `React.element` children rather than function-component props.
- `_utopia/server_main`'s current route-tree style of calling `Utopia_router.make` / `Utopia_router_route.make` directly matches SRR's own nested-router design, where the server constructs concrete `React.element` props for a client `Route` component and lets the renderer serialize that client boundary.

## Active slice

- [completed] Fix the hydration mismatch by hydrating the streamed full-document RSC tree against the browser `document`
- [completed] Prefer source `_utopia/dist` bundles over stale `_build/default/.../dist` copies when a generated server serves bootstrap assets
- [pending] Align generated route trees with the SRR nested-router demo by emitting pass-through route boundaries for ancestor path segments, not only explicit layout nodes
- [pending] Add regression coverage proving nested navigation diff payloads mount under the nearest shared route boundary instead of falling back to a full-tree swap
- [pending] Update router/spec/primitives notes and re-run focused verification for the revised nested router behavior

## Active slice

- [completed] Retire the checked-in `demo/basic/` project now that `demo/notes/` is the canonical demo workspace
- [completed] Point the root demo and benchmark entrypoints at `demo/notes/`
- [completed] Verify the notes-targeted commands still resolve after removing `demo/basic/`

## Review

- Removed the checked-in `demo/basic/` tree so `demo/notes/` is now the only checked-in demo workspace.
- Retargeted `Makefile` demo helpers plus `bench/bench_http.sh` from `demo/basic/` to `demo/notes/`, and documented `demo/notes/` as the checked-in demo workspace in `plan/primitives.md`.
- Verification passes for `make -n run-demo run-demo-watch compile-demo compile-demo-watch build-generated` and `opam exec -- dune build @demo/notes/_utopia/all`.

## Review

- The generated client entry now binds `hydrateRoot` against `Dom.document`, so document-root HTML streamed by `ReactServerDOM.render_html` hydrates the full browser document instead of mismatching against `#root`.
- Generated executables now prefer source `_utopia/dist` bundles over stale `_build/default/.../dist` copies, which prevents rebuilt demos from accidentally serving an old client bundle with the wrong hydration target.
- Verification passes for `opam exec -- npm run build` in `demo/notes/`; a live request to `http://127.0.0.1:8140/notes/inbox/daily-brief` still streams an `html` root payload, and `http://127.0.0.1:8140/dist/client_entry_melange.js` now serves `Client.hydrateRoot(browserDocument, ...)`.

## Active slice

- [completed] Audit the notes demo structure so page-local rendering moves under `pages/` while shared UI/data stays in `pages/lib/`
- [completed] Refactor the notes demo from `.re` to `.mlx` without changing the visual behavior
- [completed] Fix the demo entrypoints to run the generated notes server so compiled pages render instead of raw source
- [completed] Re-run targeted verification and update notes/docs for the corrected demo flow and file layout

## Review

- The notes demo now uses `.mlx` route/layout sources under `demo/notes/pages/`, while reusable navigation, cards, skeletons, streamed side panels, and note body/checklist components live in `demo/notes/pages/lib/notes_ui.mlx` and data stays in `demo/notes/pages/lib/notes_data.ml`.
- Page-level JSX now lives in the route files themselves (`demo/notes/pages/index.mlx`, `demo/notes/pages/notes/layout.mlx`, the folder layouts/indexes, and each note route), so the compiled pages no longer render wrapper source like `Notes_ui.NotePage`.
- The workspace now declares an `mlx` dialect in `dune-project` and depends on `mlx`, with matching coverage updates in `bin/tests/compiler_supports_mlx_extension.t` and regenerated `utopia.opam` metadata.
- Demo helpers now build and launch the generated notes server without relying on `dune exec` from the nested demo workspace: `Makefile` uses `npm run build` or direct `_build/default/bin/*.exe` paths, `bench/bench_http.sh` benchmarks `_build/default/demo/notes/_utopia/server_main.exe`, and the CLI strips Dune nested-exec env vars before shelling out to `dune`.
- Verification passes for `opam exec -- npm run build` in `demo/notes/`, `opam exec -- dune build bin/cli.exe`, `opam exec -- dune runtest bin/tests/compiler_supports_mlx_extension.t`, `make -n run-demo run-demo-watch compile-demo compile-demo-watch build-generated`, a direct `../../_build/default/bin/cli.exe build` from `demo/notes/`, and a live `make run-demo` boot that serves `http://127.0.0.1:8080/notes/inbox/daily-brief` with rendered note content.

## Review

- Removed stray temporary repro workspaces (`tmp_cli_nested_env`, `.tmp_cli_nested_env`, `.tmp_mlx_test`) that were accidentally left in the repo root and getting picked up by root `dune build` / `make build`.
- Re-verified the workspace with `opam exec -- dune build .` at `/home/me/utopia` after cleaning those directories.

## Active slice

- [in_progress] Replace the notes demo's fixed folder enum and four static tag pages with persisted dynamic tags plus a single `/notes/[tag]` page
- [pending] Update the notes sidebar and note chrome: remove the sidebar heading, pin bordered `New Note` and `New Tag` actions to the sidebar bottom, remove inline note tag footers, and show only the time above note titles when a timestamp contains one
- [pending] Support creating tags both from the sidebar and while composing a note, then verify the demo build and refresh `plan/primitives.md`

## Active slice

- [completed] Replace the notes demo's fixed folder enum and four static tag pages with a persisted `tags` + `notes` data model plus a single `/notes/[tag]` page
- [completed] Update the notes sidebar and note chrome: remove the sidebar heading, pin bordered `New Note` and `Create Tag` actions to the sidebar bottom, remove inline note tag footers, and show only the time above note titles when a timestamp contains one
- [completed] Support creating tags from the sidebar and while composing a note, rebuild the demo, and refresh `plan/primitives.md`

## Review

- `demo/notes/lib/notes_data.ml` now persists tag records separately from notes, builds arbitrary tag routes through `Utopia.Routes.Notes.Param_tag.make`, and creates missing tags during note saves so new tags can exist before or after their first note.
- `demo/notes/pages/notes/layout.mlx` now renders the sidebar without the old `Tags` heading, moves the bordered `New Note` action to the pinned bottom section, and adds an inline `Create Tag` form beside the tag list.
- The fixed route files `demo/notes/pages/notes/{launch,travel,design,archive}.mlx` were replaced by `demo/notes/pages/notes/[tag].mlx`, which renders any persisted tag route and drops the old inline note tag footer while keeping checklist interactions.
- `demo/notes/pages/notes/new.mlx` now uses a freeform tag input with a datalist of existing tags, so composing a note can either reuse an existing tag or create a new one on save.
- Verification passes for `opam exec -- npm run build` in `demo/notes/`, HTTP checks against `/notes`, `/notes/design`, and `/notes/new`, and a SQLite-backed runtime check proving a newly inserted tag + note render at `/notes/brainstorm` with the expected sidebar entry and note content. Browser click-through automation was not available in this session because Chrome DevTools was unavailable.

## Active slice

- [completed] Reproduce the root cause of `make dev` failing with `inotify_add_watch(...): No space left on device`
- [completed] Exclude the local `_opam` switch from root Dune workspace scanning so watch mode does not traverse dependency trees under the repo
- [completed] Re-run non-watch and watch-mode verification to confirm the inotify failure is gone

## Review

- `make dev` was failing because `opam exec -- dune build -w @all` ran in a repo-local switch, so Dune watch mode traversed `_opam/` under the workspace root and exhausted Linux inotify watches while walking `_opam/lib/tm-grammars` and other dependency directories.
- The root `dune` file now excludes `_opam` alongside `node_modules`, which keeps the local switch available for `opam exec` while preventing Dune from treating it as part of the watched workspace.
- Pre-fix reproduction showed `df -h` and `df -i` had ample free space/inodes, so the error was a watch-limit failure rather than actual disk exhaustion.
- Post-fix verification passed with `opam exec -- dune build @all`, and a bounded `timeout 20s make dev` run no longer emitted any `inotify_add_watch(...): No space left on device` errors before termination.

## Active slice

- [in_progress] Fix the stale notes demo runtime path so removed folder-based generated artifacts do not survive the dynamic-tag refactor
- [pending] Clean the notes demo build outputs before bundling and verify no folder-based generated chunks remain

## Active slice

- [in_progress] Refine the notes tag model to store a display name plus optional description, and stop note creation from inventing new tags inline
- [pending] Move create-tag to the top of the sidebar and replace the inline form with a small popup dialog with Accept/Cancel
- [pending] Replace the note tag datalist with a custom autocomplete + fuzzy dropdown for existing tags only, hide tag description on note views, and re-verify the rebuilt demo

## Active slice

- [completed] Refine the notes tag model to store a display name plus optional description, and stop note creation from inventing new tags inline
- [completed] Move create-tag to the top of the sidebar and replace the inline form with a small popup dialog with Accept/Cancel
- [completed] Replace the note tag datalist with a custom autocomplete + fuzzy dropdown for existing tags only, hide tag description on note views, and re-verify the rebuilt demo

## Review

- `demo/notes/lib/notes_data.ml` now stores tags as `slug + name + optional description`, bumps the demo schema version so the SQLite store resets to the new shape, and requires note creation to submit an existing `tag_slug` instead of creating tags implicitly.

## Active slice

- [completed] Add a Utopia-owned build mode to compiler output so generated `_utopia/paths.mjs` carries `projectPath`, `buildMode`, and `nodeEnv`
- [completed] Rewire generated esbuild config to set `process.env.NODE_ENV` internally, enable production minification, and keep bundle behavior unchanged in development
- [completed] Split browser `callServer` into a standalone runtime module so `client_entry.re` no longer imports router code through `Utopia.re`
- [completed] Refresh demo build scripts, test coverage, and `plan/primitives.md`, then verify the real `demo/blog` production bundle size

## Review

- `bin/compiler/compiler.ml` now accepts `--mode development|production`, with direct compiler runs defaulting to development while `utopia build` and `utopia dev` explicitly select production and development modes.
- `_utopia/paths.mjs` now carries `projectPath`, `buildMode`, and `nodeEnv`, and the generated `esbuild.config.mjs` sets `process.env.NODE_ENV` internally before loading env-sensitive Node tooling.
- The runtime now has a dedicated `Utopia_call_server.re` support module, and `client_entry.re` imports that module directly so the initial browser shell no longer needs to pull `callServer` through `Utopia.re` and `Utopia_router.re`.
- The focused cram coverage passes for generated esbuild config output, default-vs-production build mode metadata, and `utopia build` emitting production-mode `_utopia/paths.mjs` for nested projects.
- A clean production bundle measurement for `demo/blog` now shows `337,938` bytes raw / `105,912` bytes gzip / `91,206` bytes brotli total across emitted JS, with `client_entry_melange.js` at `194,570` raw / `60,769` gzip / `52,060` brotli and React/RSC runtime chunks resolved to `*.production.js` artifacts.
- `demo/notes/pages/notes/layout.mlx` moves `Create Tag` above the tag list and replaces the old inline footer form with a compact popup dialog that accepts a required name and optional description, while keeping `New Note` pinned at the bottom.
- `demo/notes/pages/notes/new.mlx` replaces the `datalist` tag field with a local fuzzy combobox that filters existing tags by name, description, or slug and only allows saving once a real tag is selected.
- `demo/notes/pages/index.mlx`, `demo/notes/pages/notes/index.mlx`, and `demo/notes/pages/notes/[tag].mlx` now render tag display names instead of slug labels, and the note view header no longer renders the tag description.
- Verification passes for `opam exec -- npm run build` in `demo/notes/`, plus HTTP checks against `/notes`, `/notes/new`, and `/notes/design` on a temporary generated server showing the top-level `Create Tag` trigger, the new existing-tag-only note composer markup, and the description-free note-view header.

## Active slice

- [completed] Inspect the stale sidebar-after-tag-create bug against the nested router and confirm whether the surrounding `/notes` layout is being revalidated
- [completed] Align the notes mutation flow with the upstream `server-reason-react` nested-router demos by revalidating after tag creation navigation

## Review

- The sidebar staleness came from `demo/notes/pages/notes/layout.mlx` navigating to the new tag route without an explicit revalidation mode, so the nested router requested only a sibling-route diff and left the `/notes` layout cache untouched.
- Upstream `server-reason-react` nested-router demos (`NestedRouter_DeleteNoteButton.re` and `NestedRouter_NoteEditor.re`) force a revalidation-style navigation after mutations for exactly this reason.
- `create_tag` success handling now navigates with an explicit revalidation mode, so a new tag refreshes the sidebar layout immediately after navigation.

## Active slice

- [completed] Inspect the notes demo navigation button implementation and confirm why clicks do nothing after the mlx refactor
- [completed] Refactor the navigation control so server-rendered buttons still hydrate interactive client behavior
- [completed] Verify navigation works and update notes/docs/task tracking

## Review

- The broken note navigation came from `demo/notes/pages/lib/notes_ui.mlx` rendering a plain server-side `<button>` from a `switch%platform` branch, so no interactive client behavior survived in the rendered page.
- `Notes_ui.Navigate` now delegates to the generated `Utopia_router_link` client component, which keeps a real `href` in the server HTML and still upgrades to client-side router navigation once hydrated.
- Verification passes for `opam exec -- npm run build` in `demo/notes/` and live HTTP checks against `http://127.0.0.1:8142/` plus `http://127.0.0.1:8142/notes`, which now render `a.js-route-link` anchors instead of inert buttons.

## Active slice

- [completed] Inspect the notes demo build/runtime flow to wire Tailwind CSS assets into the generated server output
- [completed] Add a Tailwind CLI build rule plus source/config files for the notes demo stylesheet
- [completed] Load the compiled stylesheet from server-rendered HTML and ensure the server can serve the CSS asset
- [completed] Verify the Tailwind setup with targeted builds/live asset requests and update docs/task notes

## Review

- The notes demo now has a Tailwind pipeline in `demo/notes/dune` that builds `output.css` from `demo/notes/styles.css` and `demo/notes/tailwind.config.js`, while `demo/notes/package.json` includes the Tailwind CLI, core package, and typography plugin plus a build script that compiles the stylesheet before bundling the client JS.
- `lib/utopia_server/utopia_server.ml` now treats `/output.css` as a known stylesheet asset, injects it into the rendered document head when present, and extends generated-server asset lookup to include build-root artifacts such as `_build/default/demo/notes/output.css`.
- Verification passes for `opam exec -- dune build output.css` in `demo/notes/`, `opam exec -- npm run build` in `demo/notes/`, `opam exec -- dune build .` at the repo root, live HTTP checks against `http://127.0.0.1:8143/` plus `http://127.0.0.1:8143/output.css`, `PORT=8144 make run-demo`, and direct inspection of `_build/default/demo/notes/output.css` for expected utilities like `min-h-screen`, `prose-stone`, `font-[Newsreader]`, and the custom `notes-scrollbar` rules.

## Active slice

- [completed] Map the current notes demo structure and choose a minimal Utopia Notes-style one-sidebar route model
- [completed] Refactor the notes demo to a single sidebar with four top-level tag routes and a single content pane
- [completed] Redesign the notes UI to a minimal Utopia Notes-inspired look with no border radius or shadows
- [completed] Remove obsolete nested note routes/components and align demo data with the new tag-driven structure
- [completed] Verify the rebuilt demo visually via HTTP output/builds and update docs/task notes

## Review

- The notes demo route tree is now flat under `demo/notes/pages/notes/`: `/notes/launch`, `/notes/travel`, `/notes/design`, and `/notes/archive` are the four tag routes, with `demo/notes/pages/notes/layout.mlx` as the only persistent sidebar shell.
- `demo/notes/pages/lib/notes_data.ml` now seeds a smaller four-note Utopia Notes-style data model and resets the demo SQLite table on first server access so the route/data shape always matches the current demo code.
- `demo/notes/pages/lib/notes_ui.mlx` was simplified to flat Apple-style primitives: sidebar tags, plain list rows, a single note header/body/checklist flow, and no decorative cards, rounded corners, or shadows.
- `demo/notes/pages/layout.mlx`, `demo/notes/pages/index.mlx`, `demo/notes/pages/notes/layout.mlx`, `demo/notes/pages/notes/index.mlx`, and the new tag pages under `demo/notes/pages/notes/*.mlx` now use a restrained system-font layout with subtle separators and one content pane.
- Verification passes for `opam exec -- npm run build` in `demo/notes/`, `opam exec -- dune build .` at the repo root, live HTTP checks against `http://127.0.0.1:8145/notes` plus `http://127.0.0.1:8145/notes/design`, and source checks confirming there are no remaining `rounded-*` or `shadow-*` utilities in the notes demo source.

## Active slice

- [completed] Add a `/notes/new` route with a lightweight WYSIWYG note editor for the Utopia Notes demo
- [completed] Support checklist item creation and toggling inside the editor and persist new notes into the demo store
- [completed] Integrate created notes back into the tag routes/sidebar model and verify the flow end-to-end

## Review

- The notes demo now includes `demo/notes/pages/notes/new.mlx`, which exposes a `/notes/new` route with a direct-edit note surface, tag selection, and checklist editing controls.
- `demo/notes/pages/lib/notes_ui.mlx` now contains a client-side `NewNoteEditor` that builds a browser `FormData`, adds checklist items, toggles their done state inline, and saves via the ppx-generated `create_note.call(...)` server action path instead of a custom client fetch.
- `demo/notes/pages/lib/notes_data.ml` now stores note bodies as HTML, keeps a schema version for the demo SQLite file, preserves created notes across restarts until the schema changes, and exposes `create_note_from_form_data` plus `notes_for_folder` so tag pages can render all notes in a tag.
- The four tag routes (`demo/notes/pages/notes/launch.mlx`, `demo/notes/pages/notes/travel.mlx`, `demo/notes/pages/notes/design.mlx`, and `demo/notes/pages/notes/archive.mlx`) now render full tag stacks via `Notes_ui.FolderPage`, so newly created notes appear immediately in their chosen route.
- Verification passes for `opam exec -- npm run build` in `demo/notes/`, `opam exec -- dune build .` at the repo root, compiler PP output checks showing `create_note` is registered in `FunctionReferences`, HTTP checks for `/notes/new`, a direct action POST to `/notes/new` that returns `"/notes/travel"`, and a follow-up fetch of `/notes/travel` showing the created note plus both checklist items.

## Active slice

- [completed] Add a shared `Fpath`-backed path helper for generated `_utopia` artifacts and the built `server_main.exe` location
- [completed] Rewire CLI/compiler path handling so nested projects resolve the generated server executable correctly and update the CLI regression coverage
- [completed] Refresh primitives/review notes and run targeted verification for the new path helper + generated server path flow

## Review

- Added a new internal `lib/utopia_path/` helper backed by `Fpath`, and rewired CLI/compiler project-path derivation through it so generated artifact paths stop relying on hand-built strings.
- `utopia prod` now reports and resolves `_build/default/<project-path>/_utopia/server_main.exe` for nested projects while preserving `_build/default/_utopia/server_main.exe` for root projects; `utopia dev` now uses the same helper for existence checks, spawns, and restart polling.
- Added nested-project CLI regression coverage in `bin/tests/cli_prod_uses_nested_generated_server_main.t` and `bin/tests/cli_dev_uses_nested_generated_server_main.t`, and documented the new path helper plus nested generated-server path shape in `plan/primitives.md` / `plan/spec.md`.
- Verification passed for `opam exec -- dune build bin/cli.exe lib/utopia_path/utopia_path.cmxa` plus manual nested/root `utopia prod` missing-artifact checks showing the expected `_build/default/demo/notes/_utopia/server_main.exe` vs `_build/default/_utopia/server_main.exe` paths.
- The earlier `Utopia_router_source` extraction blocker has been resolved by moving the static support sources into `lib/utopia_project_support/`; any remaining full-suite failures come from other pre-existing worktree changes outside this slice.

## Active slice

- [in_progress] Move the notes demo shared modules from `demo/notes/pages/lib/` into `demo/notes/lib/` and update the demo imports/build inputs to use the new shared lib root
- [completed] Switch compiler shared-lib integration from `pages/lib/` to project-root `lib/`, keeping the auto-opened `Lib` aliases while mirroring shared modules with their real module names
- [completed] Extract the generated `Utopia_router.re` source out of `bin/compiler.ml` into a dedicated support module
- [completed] Refresh shared-lib docs/primitives/tests and run focused verification for the moved demo plus the new `lib/` integration

## Active slice

- [completed] Extract compiler-emitted static support sources out of `bin/compiler.ml` into dedicated `lib/` support files
- [completed] Rewire `_utopia/dune` generation to copy those support files as dependencies instead of `write-file` rules, leaving only dynamic outputs generated inline
- [completed] Remove the remaining hardcoded router/support source blobs from `lib/utopia_router_source/` and any similar compiler helper modules
- [completed] Update compiler tests, primitives, and verification notes for the dependency-based support-file flow

## Review

- Added a new internal `lib/utopia_project_support/` bundle that owns the static generated project sources as real files (`ReactServerDOMEsbuild.re`, `FunctionReferences.re`, `Utopia*.re`, and `client_entry.re`) plus install metadata for packaged CLI use.
- `bin/compiler.ml` now resolves those support files from either the workspace source tree or the installed package location, copies them into `_utopia/` and `_utopia/native/`, and no longer embeds their source code as multiline string literals.
- Generated `_utopia/dune` no longer emits static `write-file` rules for those support modules; the only remaining `write-file` usage is for dynamic `Lib.re` alias files that depend on project-local shared modules.
- Updated compiler regression coverage to assert the copied support files exist, the static support `target ...` rules are gone from generated dune output, and the copied `client_entry.re` still carries the expected hydration/runtime code.

## Active slice

- [completed] Re-verify the `utopia_project_support` extraction against the current worktree and fix any remaining compiler wiring regressions
- [completed] Re-run the targeted compiler/CLI regression suite, including the new nested generated-server tests
- [completed] Record the verification results in the review notes once the suite is green or the remaining blocker is isolated
- Verification passed for `dune build bin/compiler.exe`, `dune build utopia.install`, `dune runtest bin/tests/compiler_generates_dune_rules.t`, `dune runtest bin/tests/compiler_generates_client_entry.t`, `dune runtest bin/tests/compiler_scopes_pages_library_name.t`, and direct temporary-project checks that both `_build/default/bin/compiler.exe` and `_build/install/default/bin/utopia.compiler` copy the static support files correctly while preserving the expected `client_entry.re` contents.

## Review

- Root and nested shared-library coverage now both pass: `bin/tests/compiler_autoopens_lib_folder.t`, `bin/tests/generated_dune_rebuilds_lib_changes.t`, and `bin/tests/compiler_scopes_pages_library_name.t` all verify project-root `lib/` mirroring, alias generation, and rebuild behavior.
- `bin/tests/cli_dev_uses_nested_generated_server_main.t` and `bin/tests/cli_prod_uses_nested_generated_server_main.t` now exercise a nested `demo/notes/lib/Greeting.re` module end-to-end, proving CLI-driven builds and generated servers can consume project-root shared libs in nested projects.
- `lib/utopia_path/utopia_path.ml` now resolves the nearest enclosing `dune-project`, and `bin/cli.ml` passes that root through `dune --root ...` for build/dev/clean so temp nested projects do not get hijacked by unrelated parent Dune files.
- `bin/compiler.ml` no longer emits the stale `module Utopia_server = Lib_server.Server` alias in generated `_utopia/server_main.ml`; it now relies on the copied `Utopia_server.ml` support file consistently with the generated executable modules list.
- Verification passed for `dune build --force bin/compiler.exe bin/cli.exe`, `dune runtest bin/tests/compiler_generates_server_main.t`, `dune runtest bin/tests/compiler_autoopens_lib_folder.t`, `dune runtest bin/tests/generated_dune_rebuilds_lib_changes.t`, `dune runtest bin/tests/compiler_scopes_pages_library_name.t`, `dune runtest bin/tests/compiler_builds_server_function_project.t`, `dune runtest bin/tests/cli_prod_uses_generated_server_main.t`, `dune runtest bin/tests/cli_dev_uses_nested_generated_server_main.t`, and `dune runtest bin/tests/cli_prod_uses_nested_generated_server_main.t`.

## Active slice

- [completed] Inline the notes demo UI components from `demo/notes/lib/notes_ui.mlx` into the route files that render them
- [completed] Add a public `Utopia.Router.Navigate` wrapper so notes pages can use shared router navigation without a demo-local helper
- [completed] Refresh primitives/spec/task notes for the page-local notes UI layout and the new router namespace
- [completed] Re-run focused notes-demo verification after the refactor

## Review

- Removed `demo/notes/lib/notes_ui.mlx`; the notes demo now keeps its visual components inline inside `demo/notes/pages/index.mlx`, `demo/notes/pages/notes/index.mlx`, `demo/notes/pages/notes/layout.mlx`, the four tag pages under `demo/notes/pages/notes/*.mlx`, and `demo/notes/pages/notes/new.mlx`.
- `lib/utopia_project_support/files/Utopia.re` now exposes a public `Utopia.Router.Navigate` client component wrapper around `Utopia_router_link`, and a fresh compiler run regenerates the same API into `demo/notes/_utopia/Utopia.re`.
- `plan/primitives.md` now documents the page-local notes UI structure, the data-only role of `demo/notes/lib/`, and the new `Utopia.Router` namespace; `plan/spec.md` now mentions `Utopia.Router.Navigate` alongside `Utopia.useRouter()`.
- Focused verification passes for `opam exec -- dune exec ../../bin/compiler.exe` and `opam exec -- dune build @_utopia/melange _utopia/native/pages_demo_notes.cmxa` in `demo/notes/`.

## Active slice

- [completed] Reproduce and inspect the generated `demo/notes/_utopia/server_main.exe` failure around `Utopia_server`
- [completed] Make the notes demo build regenerate current `_utopia/` support files before compiling the native executable
- [cancelled] Refresh primitives/task notes if the server runtime public module path changes
- [completed] Re-run the full `demo/notes` build flow after the server-main fix

## Review

- The notes demo build failure was caused by stale generated `_utopia/` support files; `demo/notes/package.json` previously built native and Melange artifacts without first rerunning the current compiler.
- `demo/notes/package.json` now runs `dune exec ../../bin/compiler.exe` before `dune build ...`, so `npm run build` refreshes generated support files such as `_utopia/Utopia_server.ml` before compiling `server_main.exe`.
- `plan/primitives.md` now records that the checked-in notes demo build regenerates `_utopia/` as part of its package build flow.
- Verification passes for `opam exec -- npm run build` in `demo/notes/` and `opam exec -- dune runtest bin/tests/compiler_builds_server_function_project.t` at repo root.

## Active slice

- [completed] Make generated `_utopia/server_main.exe` self-contained by copying the server/type runtime support sources into generated projects
- [completed] Normalize PPX-emitted standalone `React.DangerouslyInnerHtml` page/layout outputs so generated HTML/dev-server routes keep streaming real markup
- [completed] Re-run the focused compiler/CLI/generated-server regression suite after the generated runtime changes land

## Review

- `lib/utopia_project_support/` now installs and copies `Utopia_server.ml` plus `Utopia_types.ml`, so generated projects no longer rely on unpublished workspace libraries when building `_utopia/server_main.exe`.
- `bin/compiler.ml` now emits a local `server_main` executable stanza over `server_main`, `Utopia_server`, and `Utopia_types`, and wraps generated page/layout expressions through `Utopia_server.wrap_raw_inner_html_element` so root routes keep working even when the PPX compiles trivial JSX into standalone `React.DangerouslyInnerHtml` nodes.
- `lib/server/server.ml` now recursively normalizes standalone `React.DangerouslyInnerHtml` elements before router/model serialization, which restores streamed HTML for root-page dev flows and the generated-server restart coverage.
- `plan/primitives.md` now documents the expanded generated project-support bundle plus the standalone-inner-html normalization behavior.
- Serial targeted verification passes for `opam exec -- dune build utopia.install` plus `opam exec -- dune runtest -j 1 bin/tests/compiler_generates_dune_rules.t bin/tests/compiler_generates_client_entry.t bin/tests/compiler_generates_esbuild_config.t bin/tests/compiler_generates_server_main.t bin/tests/compiler_builds_server_function_project.t bin/tests/compiler_autoopens_lib_folder.t bin/tests/generated_dune_rebuilds_lib_changes.t bin/tests/compiler_scopes_pages_library_name.t bin/tests/cli_prod_requires_build_artifacts.t bin/tests/cli_prod_uses_generated_server_main.t bin/tests/cli_dev_uses_generated_server_main.t bin/tests/cli_prod_uses_nested_generated_server_main.t bin/tests/cli_dev_uses_nested_generated_server_main.t bin/tests/cli_dev_restarts_generated_server_main.t bin/tests/server_streams_html_page.t bin/tests/server_generated_rsc_request_returns_model_payload.t bin/tests/server_generated_rsc_request_returns_diff_payload.t bin/tests/server_generated_rsc_request_returns_nested_diff_payload.t`.

## Active slice

- [completed] Inspect the notes demo checklist rendering and current note-creation toggle flow to find the right post-create toggle integration point
- [completed] Add checklist toggle behavior for already-created notes and persist the updated done state in the notes demo store
- [completed] Refresh demo docs/primitives and run focused verification for note checklist toggling on existing notes

## Review

- The four tag-route note views under `demo/notes/pages/notes/*.mlx` now render their checklist sections as client components with clickable toggle buttons instead of static markers, while keeping the existing Utopia Notes visual treatment.
- Each tag page now declares a generated server action wrapper around `toggle_note_checklist_item_from_form_data`, so clicking a checklist marker posts the note slug plus checklist index and then revalidates the current route through `Utopia.useRouter().navigate(~history:Utopia.Replace, ~freshness:Utopia.Revalidate, ...)`.
- `demo/notes/lib/notes_data.ml` now exposes persisted checklist toggling helpers (`note_by_slug`, `toggle_checklist_item_at`, `toggle_note_checklist_item`, and `toggle_note_checklist_item_from_form_data`) that update the SQLite-backed demo store in place.
- `plan/primitives.md` now records that created notes remain toggleable from the tag-route note views after they are saved.
- Verification passed for `opam exec -- npm run build` in `demo/notes/` plus a live server-action toggle check that fetched `/notes/launch`, posted the generated `toggle_note_checklist_item_action` multipart request, and confirmed the previously incomplete checklist item re-rendered with `line-through` styling.

## Active slice

- [in_progress] Split `bin/cli.ml` into a dedicated `bin/cli/` executable directory with semantic modules for terminal, process, filesystem/artifact, flag, rpc, and command concerns
- [pending] Split `bin/compiler.ml` into a dedicated `bin/compiler/` executable directory with semantic modules for filesystem, routes, manifest emission, project support, esbuild, dune emission, and server-main emission
- [pending] Move the standalone server executable into `bin/server/`, update Dune/build entry points and path references for the new per-executable folders
- [pending] Refresh primitives/task notes and run focused build + regression verification for the refactored executable layout

## Active slice

- [in_progress] Rename `utopia_project_support` to `utopia_runtime` and keep the runtime-source bundle/compiler references aligned with that naming
- [pending] Move important hardcoded path layout knowledge out of `bin/compiler/` and `bin/cli/` into `lib/utopia_path/`, leaving only dynamic page/route discovery local to the executables
- [pending] Finish wiring the new `bin/cli/`, `bin/compiler/`, and `bin/server/` folderized executables against the updated runtime/path libraries
- [pending] Refresh primitives/task notes and rerun focused build + regression verification for the renamed runtime + executable refactor

## Active slice

- [in_progress] Inspect the notes demo checklist rendering and current note-creation toggle flow to find the right post-create toggle integration point
- [pending] Add checklist toggle behavior for already-created notes and persist the updated done state in the notes demo store
- [pending] Refresh demo docs/primitives and run focused verification for note checklist toggling on existing notes

## Active slice

- [completed] Replace every `let text = React.string` helper in `demo/notes/pages/` with inline `React.string` calls
- [completed] Re-run targeted search/build verification for the inlined notes page text nodes
- [completed] Record the cleanup result in the review notes

## Review

- Removed the `let text = React.string` alias from each notes demo page/layout source under `demo/notes/pages/`, and now each JSX text node calls `React.string` inline at the render site.
- Verified the cleanup with repo searches showing no remaining `let text = React.string` bindings or `(text ...)` call sites in `demo/notes/pages/*.mlx`.
- Verification passed for `opam exec -- npm run build` in `demo/notes/` after the inline `React.string` refactor.

## Active slice

- [completed] Design and add a generated typed route API that replaces string-based navigation with `Utopia.Route.t`
- [completed] Extend the route model to cover typed query/hash payloads through mirrored route schema modules and generated `Utopia.Routes`
- [completed] Migrate router/demo callsites plus docs/tests/primitives and re-run focused verification

## Review

- Added a generated typed routing layer: `_utopia/Utopia_routes.ml` now mirrors collected page routes, `lib/utopia_runtime/files/Utopia_route.ml` owns canonical `Utopia.Route.t` values (pathname/request-path/hash aware), and public navigation now flows through typed routes instead of raw strings.
- The compiler now understands optional mirrored route schema files under project-root `routes/`, copies them into both Melange/native builds, and lets `Utopia.Routes.<...>.make` expose typed `Route_query` / `Route_hash` modules when a schema defines `module Query` and/or `module Hash`.
- `lib/utopia_runtime/files/Utopia.re`, `lib/utopia_runtime/files/Utopia_router.re`, and `lib/utopia_runtime/files/Utopia_router_link.re` now expose typed route navigation, preserve hash-only navigation changes without unnecessary RSC fetches, and keep the internal route-boundary component under `Utopia.Router.Boundary`.
- The notes demo now consumes typed routes throughout `demo/notes/lib/notes_data.ml` and the page files under `demo/notes/pages/`, including sidebar active-state checks, new-note links, and programmatic checklist/note navigation.
- Updated compiler/runtime/docs coverage in `bin/tests/compiler_generates_dune_rules.t`, `bin/tests/compiler_generates_server_main.t`, `bin/tests/compiler_supports_mlx_extension.t`, `bin/tests/compiler_generates_nested_page_modules.t`, the new `bin/tests/compiler_builds_typed_routes_project.t`, plus `plan/primitives.md` and `plan/spec.md`.
- Verification passed for `opam exec -- dune build bin/compiler/compiler.exe`, `opam exec -- dune runtest bin/tests/compiler_generates_dune_rules.t bin/tests/compiler_generates_server_main.t bin/tests/compiler_supports_mlx_extension.t bin/tests/compiler_generates_nested_page_modules.t bin/tests/compiler_builds_typed_routes_project.t`, `opam exec -- dune runtest bin/tests/server_generated_rsc_request_returns_diff_payload.t bin/tests/server_generated_rsc_request_returns_nested_diff_payload.t`, and `opam exec -- npm run build` in `demo/notes/`.

## Active slice

- [completed] Generate a typed current-route parser over `Utopia.Route.t` so route values can be decoded back into constructors with typed params/query/hash payloads
- [completed] Expose the typed current-route match through `Utopia.Routes.Current` and `Utopia.useRouter().current`
- [completed] Tighten route-schema requirements around `decode`, refresh docs, and rerun focused verification for the current-route parser

## Review

- `bin/compiler/Generated_routes.ml` now emits `Utopia.Routes.Current`, a generated sum type plus `current/of_route` parser that matches `Utopia.Route.t` values back into typed constructors while preserving route specificity and decoding query/hash payloads.
- `lib/utopia_runtime/files/Utopia_route.ml` now exposes decoded `path_segments`, `query_entries`, and `hash` helpers, and `lib/utopia_runtime/files/Utopia.re` now returns `current : option(Utopia.Routes.Current.t)` from `Utopia.useRouter()` alongside the raw route value.
- Route schema modules under `routes/` now require both `encode` and `decode` when they define `module Query` or `module Hash`; `bin/compiler/Route_schemas.ml` reports a compile-time error if `decode` is missing.
- Added positive and negative route-schema coverage in `bin/tests/compiler_builds_typed_routes_project.t` and `bin/tests/compiler_rejects_route_schema_without_decode.t`, while the existing compiler/server regression tests still pass.
- Verification passed for `opam exec -- dune build bin/compiler/compiler.exe`, `opam exec -- dune runtest bin/tests/compiler_generates_dune_rules.t bin/tests/compiler_generates_server_main.t bin/tests/compiler_supports_mlx_extension.t bin/tests/compiler_generates_nested_page_modules.t bin/tests/compiler_builds_typed_routes_project.t bin/tests/compiler_rejects_route_schema_without_decode.t bin/tests/compiler_autoopens_lib_folder.t bin/tests/compiler_builds_server_function_project.t`, `opam exec -- dune runtest bin/tests/server_generated_rsc_request_returns_diff_payload.t bin/tests/server_generated_rsc_request_returns_nested_diff_payload.t`, and `opam exec -- npm run build` in `demo/notes/`.

## Active slice

- [completed] Extend mirrored route schemas with `module Params` for typed dynamic path-param encode/decode
- [completed] Generate `Route_params` builders and current-route decoding for custom path-param schemas
- [completed] Add custom path-param test coverage, refresh route docs, and rerun focused verification

## Review

- `bin/compiler/Route_schemas.ml` now recognizes `module Params` in `routes/...` schemas, requires both `encode` and `decode`, and reports compile-time errors when a params schema is declared for a route with no dynamic path segments.
- `bin/compiler/Generated_routes.ml` now emits `Route_params` aliases and `~params:Route_params.t` builders for routes with custom path-param schemas, while `Utopia.Routes.Current` decodes those routes back into typed `{ params : Route_params.t; ... }` payloads.
- `lib/utopia_runtime/files/Utopia_route.ml` now includes `Utopia_route.Params` helpers (`one`, `many`, `find`, `find_one`, `find_many`, `segments_exn`) so schema modules can encode/decode path params without creating a module cycle through `Utopia.Routes`; `lib/utopia_runtime/files/Utopia.re` still re-exports a public `Utopia.Route.Params` view for ordinary route code.
- Added `bin/tests/compiler_builds_custom_path_params_project.t` and `bin/tests/compiler_rejects_route_params_schema_without_decode.t`, and updated the existing compiler fixture expectations so the generated pages library now carries `Utopia_types` wherever `Utopia_route` depends on route param kinds.
- Verification passed for `opam exec -- dune build bin/compiler/compiler.exe`, `opam exec -- dune runtest bin/tests/compiler_generates_dune_rules.t bin/tests/compiler_generates_server_main.t bin/tests/compiler_supports_mlx_extension.t bin/tests/compiler_generates_nested_page_modules.t bin/tests/compiler_builds_typed_routes_project.t bin/tests/compiler_builds_custom_path_params_project.t bin/tests/compiler_rejects_route_schema_without_decode.t bin/tests/compiler_rejects_route_params_schema_without_decode.t bin/tests/compiler_autoopens_lib_folder.t bin/tests/compiler_builds_server_function_project.t bin/tests/server_generated_rsc_request_returns_diff_payload.t bin/tests/server_generated_rsc_request_returns_nested_diff_payload.t`, and `opam exec -- npm run build` in `demo/notes/`.

## Active slice

- [completed] Switch the notes demo's generated server actions from raw route strings to typed `Utopia.Route.t` return values
- [completed] Add action-payload coverage proving server functions can return typed route values over `application/react.action`
- [completed] Refresh docs and keep the notes demo package build usable under sane Dune concurrency defaults

## Review

- `demo/notes/lib/notes_data.ml` now returns `Utopia.Route.t` values from `toggle_note_checklist_item`, `toggle_note_checklist_item_from_form_data`, and `create_note_from_form_data`, so the notes demo no longer converts server-action navigation results back into strings.
- The notes page wrappers in `demo/notes/pages/notes/new.mlx`, `demo/notes/pages/notes/archive.mlx`, `demo/notes/pages/notes/design.mlx`, `demo/notes/pages/notes/launch.mlx`, and `demo/notes/pages/notes/travel.mlx` now declare `Js.Promise.t(Utopia.Route.t)` server actions and pass the returned route directly into `router.navigate(...)`.
- Added `bin/tests/server_post_action_returns_route_payload.t`, which proves a server action can return `Utopia.Routes.About.route` and stream `0:{"pathname":"/about","request_path":"/about","href":"/about"}` over the action protocol.
- `plan/primitives.md` and `plan/spec.md` now document typed route returns from server functions, and `demo/notes/package.json` now caps `DUNE_JOBS` for the demo build so `npm run build` remains usable in this environment where Dune auto-detects pathological job counts.
- Verification passed for `opam exec -- dune runtest bin/tests/server_post_action_returns_action_payload.t bin/tests/server_post_action_returns_route_payload.t bin/tests/compiler_builds_server_function_project.t` and `opam exec -- npm run build` in `demo/notes/`.

## Active slice

- [completed] Reproduce the notes demo save failure and confirm whether client-side server-function calls for `Js.FormData.t` are sent with the wrong request encoding
- [completed] Fix the generated client `callServer` transport so `FormData` server functions post multipart bodies instead of always sending encoded text payloads
- [completed] Add regression coverage for direct client/server-function `FormData` calls and re-verify the notes demo save flow

## Active slice

- [completed] Namespace generated page/layout and shared-lib mirror modules so user sources no longer collide with runtime/generated `_utopia` support files
- [completed] Tighten route schema validation so `Query` and `Hash` modules require both `encode` and `decode`
- [completed] Add compiler regression tests for the new naming/validation behaviors and re-run focused compiler and demo coverage

## Review

- `bin/compiler/Names.ml`, `bin/compiler/Build_inputs.ml`, `bin/compiler/Generated_dune.ml`, and `bin/compiler/Server_main.ml` now namespace generated page/layout mirrors as `Utopia_page__*` and shared `lib/` mirrors as `Utopia_lib__*`, so user files like `pages/Utopia.re` and `lib/Lib.re` no longer target the same generated `_utopia` filenames as runtime support files or the generated `Lib` alias module.
- Added `bin/tests/compiler_builds_page_and_lib_named_like_support_modules.t`, which proves `utopia.compiler` plus `dune build @melange _utopia/server_main.exe` succeed for a project that defines both `pages/Utopia.re` and `lib/Lib.re`.
- Updated the existing dune, auto-open, server-action, and CLI regression tests to pin the new namespaced mirror filenames, and verification passed for `opam exec -- dune build bin/compiler/compiler.exe`, `opam exec -- dune runtest bin/tests/compiler_generates_dune_rules.t bin/tests/compiler_generates_nested_page_modules.t bin/tests/compiler_supports_mlx_extension.t`, `opam exec -- dune runtest bin/tests/compiler_autoopens_lib_folder.t bin/tests/compiler_builds_page_and_lib_named_like_support_modules.t bin/tests/compiler_builds_typed_routes_project.t bin/tests/server_post_action_returns_action_payload.t bin/tests/server_post_action_returns_route_payload.t`, `opam exec -- dune runtest bin/tests/cli_prod_uses_generated_server_main.t bin/tests/cli_dev_uses_generated_server_main.t bin/tests/cli_prod_uses_nested_generated_server_main.t bin/tests/cli_dev_uses_nested_generated_server_main.t`, plus `opam exec -- dune exec ../../bin/compiler/compiler.exe`, `DUNE_JOBS=1 dune build _utopia/server_main.exe @_utopia/melange`, and `node _utopia/esbuild.config.mjs` in `demo/notes/`.
- `bin/compiler/Route_schemas.ml` now rejects route schemas that declare `module Query` or `module Hash` without `let encode = ...`, matching the existing generated route-builder contract that already calls `Route_query.encode` and `Route_hash.encode`.
- Added `bin/tests/compiler_rejects_route_schema_without_encode.t`, which proves `utopia.compiler` now fails early for both missing `Query.encode` and missing `Hash.encode`.
- Verification passed for `opam exec -- dune runtest bin/tests/compiler_rejects_route_schema_without_encode.t bin/tests/compiler_rejects_route_schema_without_decode.t bin/tests/compiler_rejects_route_params_schema_without_decode.t bin/tests/compiler_builds_typed_routes_project.t`.

## Review

- The note-save failure came from `lib/utopia_runtime/files/Utopia_router.re` always posting server-function calls as `text/plain`, even when `ReactServerDOMEsbuild.encodeReply(args)` returned a `FormData` payload for a `Js.FormData.t` action argument.
- `lib/utopia_runtime/files/ReactServerDOMEsbuild.re` now exposes an `encodedReplyIsFormData` helper, and `Utopia_router.callServer` now omits the `Content-Type` header and posts the raw multipart body when the encoded reply is a browser `FormData` instance.
- Added `bin/tests/compiler_generates_formdata_call_server_transport.t` to lock the generated support sources to that multipart-aware client transport.
- Verification passed for `opam exec -- dune runtest bin/tests/compiler_generates_formdata_call_server_transport.t bin/tests/server_post_action_returns_action_payload.t bin/tests/server_post_action_returns_route_payload.t bin/tests/compiler_builds_server_function_project.t`, plus `opam exec -- dune exec ../../bin/compiler/compiler.exe`, `DUNE_JOBS=1 dune build _utopia/server_main.exe @_utopia/melange`, and `node _utopia/esbuild.config.mjs` in `demo/notes/`.
- A live POST against `http://127.0.0.1:8151/notes/new` with the generated `create_note` action ID now returns the typed `/notes/launch` route payload, and the saved `Transport Test Note` renders on `/notes/launch`.

## Active slice

- [completed] Reproduce the `Entering directory` / `Leaving directory` noise from nested `utopia` CLI commands under `demo/notes/`
- [completed] Silence Dune's `--root` directory banners in the shared CLI arg builder so nested `utopia build` / `utopia dev` stay quiet
- [completed] Re-run the nested build path and focused CLI test coverage to confirm the banners are gone

## Review

- `bin/cli/Artifacts.ml` now appends Dune's `--no-print-directory` flag anywhere the CLI builds its shared `--root <workspace>` argument list, which suppresses the extra `Entering directory ...` / `Leaving directory ...` lines that appeared when nested projects like `demo/notes/` shell out to the workspace root.
- The fix covers every CLI command path that uses `Artifacts.dune_root_args`, including `utopia build`, `utopia clean`, and the `utopia dev` watch/bootstrap calls that power the demo workflows.
- `bin/tests/cli_prod_uses_nested_generated_server_main.t` now captures the nested `utopia build` output and asserts it does not contain `Entering directory` or `Leaving directory`, so the quiet nested-build behavior is covered by regression tests.
- Verification passed for `opam exec -- dune build bin/cli/cli.exe`, `../../_build/default/bin/cli/cli.exe build` from `demo/notes/`, and `opam exec -- dune runtest bin/tests/cli_prod_uses_nested_generated_server_main.t`.

## Active slice

- [completed] Reproduce the notes hydration mismatch and isolate whether the extra wrapper comes from generated route wrapping or server-side model normalization
- [completed] Stop rewriting client-component HTML fallback trees while preserving the model-side standalone-inner-html normalization that generated router props still need
- [completed] Add regression coverage for generated HTML from a client component that renders simple static elements, then rerun focused verification and refresh primitives notes

## Review

- The hydration mismatch on `/notes/launch` came from Utopia rewriting standalone `React.DangerouslyInnerHtml` nodes even inside `React.Client_component.client` fallback trees; that inserted extra wrapper `<div>` nodes into streamed HTML while the browser hydrated against the client component's real JSX.
- `lib/server/server.ml` now keeps client-component fallback trees intact during both `wrap_raw_inner_html_element` and `normalize_model_element`, while still normalizing serializable client props such as generated router layout/pageconsumer elements.
- Added `bin/tests/server_generated_client_component_html_preserves_markup.t`, which builds a generated project with a client component and proves the streamed HTML contains `<section><p>Checklist</p><div>Ready</div></section>` instead of the old wrapped variant.
- Updated `plan/primitives.md` to document that standalone-inner-html normalization now stops at the server/client boundary so streamed client-component HTML remains hydration-safe.
- Verification passed for `opam exec -- dune runtest bin/tests/server_generated_client_component_html_preserves_markup.t bin/tests/server_generated_rsc_request_returns_model_payload.t bin/tests/server_generated_rsc_request_returns_diff_payload.t bin/tests/server_generated_rsc_request_returns_nested_diff_payload.t`, `opam exec -- npm run build` in `demo/notes/`, and a live `curl http://127.0.0.1:8142/notes/launch` check showing the checklist section now starts with the expected `<p>Checklist</p>` markup.

## Active slice

- [completed] Re-pin the local opam switch from `server-reason-react` `36ceb5314b6f68b1c666dc6568518111f16d0c83` to `019f7d90928d27910e3530095d9619a585854ad3`
- [completed] Refresh workspace consumers that read SRR packages from `_opam/.opam-switch/sources/server-reason-react/`
- [completed] Adapt the copied/generated `Utopia_server.ml` runtime compatibility layer to SRR's current `React.element` shape and preserve the existing `wrap_raw_inner_html_element` API as a no-op traversal
- [completed] Re-run focused verification against the updated SRR commit and capture the result

## Review

- The local opam switch is now pinned to `server-reason-react` commit `019f7d90928d27910e3530095d9619a585854ad3`, and the file-based npm consumers under both the repo root and `demo/notes/` were refreshed after the pin moved.
- The SRR update is not a drop-in change for Utopia: `React.DangerouslyInnerHtml` no longer exists as a top-level `React.element`, and `React.Provider` now carries a record payload. `lib/server/server.ml` and the copied `demo/notes/_utopia/Utopia_server.ml` runtime now match the current upstream shape.
- `wrap_raw_inner_html_element` still exists for generated server code compatibility, but on the current SRR API it is just a recursive pass-through because `dangerouslySetInnerHTML` is already represented as a `React.JSX.DangerouslyInnerHtml` prop instead of a standalone element node.
- `plan/primitives.md` and `plan/spec.md` now describe the current serialization behavior instead of the removed standalone-inner-html element normalization path.
- Verification passed for `opam exec -- dune build .` at the repo root and `DUNE_JOBS=1 opam exec -- npm run build` in `demo/notes/`.

## Active slice

- [completed] Inspect the notes demo's `Checklist` render path and identify the missing child key under `Utopia_page__Notes__Launch`
- [completed] Patch the source and generated output so checklist items render with stable keys
- [completed] Rebuild and exercise the notes demo to verify the missing-key warning is gone

## Review

- The launch-page warning came from `demo/notes/pages/notes/launch.mlx`, where `Checklist` rendered `List.mapi` rows into `React.array` without `key` props.
- The same missing-key pattern also existed in the other notes pages and list surfaces, so the fix adds stable keys across the demo's checklist rows, note lists, sidebar tag list, folder landing list, folder `<option>` list, and new-note draft checklist rows.
- Verification passed for `opam exec -- dune build .`, `DUNE_JOBS=1 opam exec -- npm run build` in `demo/notes/`, live requests to `http://127.0.0.1:8170/notes/launch` and its RSC endpoint with no `unique "key" prop` warning in the generated server log, and the rebuilt `demo/notes/_utopia/dist/chunk-GUROEJEI.js` now emits the launch checklist row as `JsxRuntime.jsxs("div", ..., Key)`.

## Active slice

- [completed] Add automatic free-port fallback to CLI and server startup when the requested `PORT` is already occupied
- [completed] Add regression coverage for both CLI startup and standalone server startup under port contention
- [completed] Update `plan/primitives.md` and capture verification results for the new port-selection behavior

## Review

- `bin/cli/Dev.ml` and `bin/cli/Prod.ml` now preflight the requested `PORT`, warn when it is busy, and increment upward to the next free port before spawning the generated server so CLI startup no longer fails on an occupied default port.
- `bin/cli/Process.ml` now validates `PORT`, resolves the requested `HOST`, and probes for the first bindable port on that interface so both CLI entrypoints share the same selection logic.
- `lib/server/server.ml` now treats `PORT` as a preferred starting port rather than a single fixed bind target: invalid values still fall back to `8080`, and `EADDRINUSE` now retries on higher ports so direct `utopia.server` or generated `server_main.exe` launches also recover from contention.
- Added `bin/tests/cli_dev_reassigns_busy_port.t` and `bin/tests/server_reassigns_busy_port.t` to cover both the CLI preflight path and the shared server-runtime fallback path.
- Verification passed for `opam exec -- dune runtest bin/tests/cli_dev_uses_generated_server_main.t bin/tests/cli_dev_reassigns_busy_port.t bin/tests/cli_prod_uses_generated_server_main.t bin/tests/server_reassigns_busy_port.t`.

## Active slice

- [completed] Add reusable notes-demo button variants for accent and full-width actions
- [completed] Apply the new button styling to the new-tag popover and notes primary actions, including a full-width popover button row
- [completed] Rebuild `demo/notes/` and update notes/primitives tracking for the new button treatment

## Review

- `demo/notes/styles.css` now defines local `notes-button` classes with shared neutral styling plus `accent` and `full-width` variants, using an almost-black `#1a1917` background and accessible gray `#ddd9d1` text for the accent treatment.
- `demo/notes/pages/notes/layout.mlx` now uses those variants for the sidebar `Create Tag` and `New Note` actions, and the new-tag popover button row is explicitly `w-full` so both `Cancel` and `Accept` can stretch across the dialog width.
- `demo/notes/pages/notes/new.mlx` now uses the accent button treatment for `Save Note` and the shared neutral button styling for `Add Item`, keeping the repeated action styling consistent across the demo.
- `plan/primitives.md` now documents the notes demo's local `notes-button` action classes so the new UI primitive is recorded alongside the rest of the checked-in demo behavior.
- Verification passed for `GOMAXPROCS=1 RAYON_NUM_THREADS=1 DUNE_JOBS=1 opam exec -- npm run build` in `demo/notes/`, and the accent text/background contrast checks came out to `12.48:1` on the base color and `10.90:1` on hover.

## Active slice

- [completed] Add a shared `demo/notes/lib/button.mlx` component for the notes demo action buttons
- [completed] Switch the repeated notes action button markup to the shared component and add `cursor: pointer`
- [completed] Rebuild `demo/notes/` and update the notes demo primitives/review notes for the new shared button abstraction

## Review

- Added `demo/notes/lib/button.mlx`, which centralizes the repeated notes demo button class composition and exposes shared `Button.Action`, `Button.Submit`, and `Button.Link` components.
- `demo/notes/pages/notes/layout.mlx` now uses that shared button module for the sidebar `Create Tag`, `Cancel`, `Accept`, and `New Note` actions instead of repeating the raw `notes-button` class combinations inline.
- `demo/notes/pages/notes/new.mlx` now uses the same shared button module for `Save Note` and `Add Item`, so the repeated action surfaces in the composer cannot drift from the sidebar/popover treatment.
- `demo/notes/styles.css` now sets `cursor: pointer` on `.notes-button` while keeping the existing disabled-state `cursor: default` override.
- `plan/primitives.md` now records the new `demo/notes/lib/button.mlx` abstraction alongside the existing notes demo button-style primitive.
- Verification passed for `opam exec -- npm run build` in `demo/notes/`.

## Active slice

- [completed] Replace the notes demo's local CSS component classes with Tailwind utility strings in the shared button helper and the page/layout markup
- [completed] Update the shared button API from the old accent boolean to `kind=Accent | Default`, and switch every current call site to the explicit variant
- [completed] Trim `demo/notes/styles.css` down to the Tailwind entrypoint, refresh primitives/lessons tracking, and rebuild `demo/notes/`

## Review

- `demo/notes/styles.css` is now just the Tailwind entrypoint (`@import "tailwindcss"` plus `@config`), and all former demo-specific component rules were removed.
- `demo/notes/lib/button.mlx` now emits Tailwind utility strings directly and uses an explicit `kind` variant (`Default` or `Accent`) instead of the old accent boolean flag.

## Active slice

- [completed] Audit the repo for custom boolean props and helper flags that should become explicit variants
- [completed] Refactor the notes button helper, markdown renderer helpers, and generated router API to use variants instead of boolean props
- [completed] Refresh docs/demo content to the new variant-based navigation/button APIs and verify the affected builds

## Review

- `demo/notes/lib/button.mlx` no longer exposes boolean style/state props: it now uses explicit `kind`, `width`, and `state` variants while still forwarding only native `disabled` booleans to the underlying DOM button.
- The generated router runtime under `lib/utopia_runtime/files/` now exposes `navigation_history` (`Push` or `Replace`) and `navigation_freshness` (`Use_cache` or `Revalidate`) variants instead of `replace` and `revalidate` booleans, and the notes demo call sites were updated to use the new labels.
- The markdown runtime no longer exposes boolean `ariaHidden`, `disabled`, or `checked` helper props: `markdown/elements.re` and `markdown/components.ml` now use `A.visibility` and `Li.marker` variants, and `markdown/render.ml` now threads a `State.safety` variant instead of boolean safety/backend flags.
- `demo/notes/lib/notes_data.ml`, `plan/primitives.md`, `tasks/lessons.md`, and this task log now describe the current variant-based navigation/button APIs instead of the old boolean spellings.
- Repo-wide searches for optional/default boolean prop spellings now come back clean; the only remaining boolean props are native DOM attributes such as `<button disabled=...>` and `<input checked=...>`.
- Verification passed for `opam exec -- dune build markdown/markdown.exe --display=short` at repo root and `opam exec -- dune build output.css _utopia/server_main.exe @_utopia/melange && node _utopia/esbuild.config.mjs` in `demo/notes/` after regenerating `_utopia/`.
- `demo/notes/pages/layout.mlx` now carries the old global selection/color-scheme/font rendering treatment as Tailwind utilities on the root wrapper.
- `demo/notes/pages/notes/{layout,new,index,[tag]}.mlx` now inline the former button, scrollbar, rich-text, and empty-editor placeholder styling as Tailwind utilities, so there are no remaining `notes-button`, `notes-scrollbar`, `note-body`, or `editor-surface` custom classes in the demo code.
- `plan/primitives.md` now documents the new `Button.kind` variant and the fact that `demo/notes/styles.css` is only the Tailwind entrypoint, and `tasks/lessons.md` records the preference for semantic Tailwind-powered variants over bespoke CSS classes/boolean flags.
- Verification passed for `opam exec -- npm run build` in `demo/notes/`.

## Active slice

- [completed] Add an OCaml-only markdown highlighting path using `ochre` + `tm-grammars`, with no new npm dependencies
- [completed] Extract the markdown renderer into a reusable library and route both `utopia.markdown` and the server markdown page path through it
- [completed] Switch the notes demo to store markdown text, render note bodies on the server, and replace the HTML editor with markdown input
- [completed] Update `plan/primitives.md`, verify the root build/markdown tests/demo build, and capture review notes for the new markdown pipeline

## Review

- Added a public native markdown library at `markdown/` (`utopia.markdown_runtime`) so `utopia.markdown`, the standalone server markdown route path, and generated native page code all use the same `cmarkit` -> React -> HTML renderer.
- Fenced code blocks now highlight natively through `ochre` plus curated `tm-grammars` grammars, while inline code spans emit the `utopia-inline-code` class for consumer styling. The `markdown/tests` suite was updated to assert the new output shape.
- `lib/server/server.ml` now renders markdown bodies through `Utopia_markdown.render_string_to_html`, and the generated `_utopia/dune` dependency lists now link `utopia.markdown_runtime` into both generated native pages libraries and `server_main.exe`.
- `demo/notes/lib/notes_data.ml` now stores note bodies as `body_markdown`, escapes multiline markdown before SQLite writes, decodes it on read, renders note HTML on the server, and resets the demo schema to `apple-notes-demo-v6` so the seeded notes are recreated in markdown form.
- `demo/notes/pages/notes/new.mlx` now uses a markdown textarea instead of the old HTML `contentEditable` surface, and `demo/notes/pages/notes/[tag].mlx` styles rendered markdown, inline code, and Ochre blocks inside the note view.
- The seeded `design` note now includes both inline code and a fenced `mlx` block so the checked-in demo actually exercises the new highlighting path.
- Verification passed for `opam exec -- dune build .`, `opam exec -- dune runtest markdown/tests`, the `demo/notes/` build steps (`dune exec ../../bin/compiler/compiler.exe`, `dune build output.css _utopia/server_main.exe @_utopia/melange`, and `node _utopia/esbuild.config.mjs`), plus a live `curl http://127.0.0.1:8176/notes/design` check confirming server-rendered `utopia-inline-code` and `ochre utopia-markdown-code-block` markup.

## Active slice

- [in_progress] Rename the root `Makefile` demo `run-*` targets to the `demo-run-*` naming scheme and update repo references
- [completed] Increase every `demo/blog/` Tailwind slate utility by one grade
- [completed] Re-run the blog demo verification after the slate color tweak and capture review notes

## Review

- The `demo/blog/` source now bumps every Tailwind `slate-*` utility one grade darker across the page shell, post list, about page, markdown renderer components, post footer, and inline-code styling in `styles.css`.
- Verification passed for `make -C demo/blog generate`, which rebuilt the blog assets and re-rendered the static output under `demo/blog/_utopia/static/` with the updated classes.

## Active slice

- [completed] Reproduce the `demo/blog/` markdown line-wrap bug and locate the shared renderer branch dropping soft breaks
- [completed] Render soft markdown breaks as spaces and add regression coverage for wrapped inline content
- [completed] Verify the markdown tests pass and the blog post HTML no longer concatenates wrapped words

## Review

- `markdown/render.ml` now renders CommonMark soft breaks as `React.string " "` instead of dropping them, so wrapped source lines inside a paragraph keep their expected spacing.
- `markdown/tests/simple.t` now asserts both the inline-link case and a plain wrapped-text case, which catches the exact regression that made `demo/blog/` paragraphs collapse words together.
- Verification passed for `opam exec -- dune runtest markdown/tests` and `_build/default/markdown/markdown.exe < demo/blog/content/hello-utopia.md`, whose first paragraph now renders `It takes a file-based routing approach` instead of concatenating `Ittakes`.

## Active slice

- [completed] Move `demo/blog/` post metadata out of `lib/blog_data.ml` and into markdown frontmatter
- [completed] Refactor `demo/blog/lib/blog_data.ml` to load metadata from `content/*.md` frontmatter and strip it before markdown rendering
- [completed] Inline `title`, `date`, and `description` frontmatter into each `demo/blog/content/*.md` file, update `plan/primitives.md`, and verify the demo build/render path

## Review

- `demo/blog/lib/blog_data.ml` no longer owns a hardcoded `posts` metadata list; it now scans `demo/blog/content/*.md`, parses top-of-file frontmatter for `title`, `date`, and `description`, and sorts the resulting posts by descending date.
- The same module now strips the frontmatter block before handing markdown to `Utopia_markdown`, so rendered post bodies start at the actual article content while page metadata still comes from the frontmatter.
- Each checked-in blog post markdown file now carries its own `title`, `date`, and `description` block, and `plan/primitives.md` documents that `demo/blog/` already uses this demo-local frontmatter flow even though compiler-wide markdown frontmatter remains planned.
- Verification passed for `opam exec -- npm run build` in `demo/blog/`, plus a live render check against `_build/default/demo/blog/_utopia/server_main.exe` showing `GET /` and `GET /posts/hello-utopia` both return `200` with the expected frontmatter-derived `<title>`/description and no frontmatter block rendered into the post body.
- `opam exec -- npm run ssg` still fails in `demo/blog/` because the generated `_utopia/ssg` alias looks for `demo/blog/_utopia/_utopia/server_main.exe`; that appears unrelated to this frontmatter refactor and remains unfixed.

## Active slice

- [completed] Replace `demo/blog/` npm-script build automation with a local `Makefile`
- [completed] Fix and explain the generated SSG alias path bug that currently emits `_utopia/_utopia/server_main.exe`
- [completed] Verify the new `demo/blog/Makefile` build and SSG flow, then update docs/review notes

## Review

- `demo/blog/Makefile` is now the canonical local entrypoint for the blog demo, with `compile`, `build`, `run`, `ssg`, and `clean` targets; `demo/blog/package.json` keeps only the JS dependencies and no longer defines npm scripts.
- The root `Makefile` blog helpers (`compile-blog`, `run-blog`, `ssg-blog`) now delegate to `demo/blog/Makefile` instead of shelling out through `npm run ...`.
- The SSG path bug came from `bin/compiler/Generated_dune.ml`: it generated the `_utopia/dune` alias as `(run ./_utopia/server_main.exe --ssg)` even though that stanza already lives inside `_utopia/`, so Dune resolved it as `_utopia/_utopia/server_main.exe`. The generator now emits `(run ./server_main.exe --ssg)` instead.
- Regenerating `demo/blog/_utopia/dune` now produces the corrected alias `(rule (deps (alias all)) (alias ssg) (action (run ./server_main.exe --ssg)))`, and `opam exec -- dune build @_utopia/ssg` succeeds from `demo/blog/`.
- Verification passed for `make build && make ssg` in `demo/blog/`, which rendered the static output under `demo/blog/_utopia/static/`, and `make -n compile-blog run-blog ssg-blog` at repo root, which shows the root wrappers now call the blog-local Makefile.

## Active slice

- [completed] Move `demo/notes/` off npm scripts onto a local `Makefile`
- [completed] Fix the compiler build-mode constructor regression that currently breaks `make -C demo/* build`
- [completed] Rename the blog generation target from `ssg` to `generate`, update root wrappers/docs, and verify the new demo commands

## Review

- `demo/notes/` now has a local `Makefile` with `compile`, `build`, `run`, `dev`, `watch-compile`, and `clean`; `demo/notes/package.json` now carries only the JS dependencies and no longer defines npm scripts.
- The notes demo build preserves the current production-mode behavior from the old npm script by regenerating `_utopia/` with `../../_build/default/bin/compiler/compiler.exe --mode production` before building `output.css`, `_utopia/server_main.exe`, `@_utopia/melange`, and `@_utopia/esbuild`.
- Root notes automation now delegates to that workspace Makefile: `run-demo`, `run-demo-watch`, `compile-demo`, `compile-demo-watch`, `build-generated`, and `bench/bench_http.sh` no longer shell out through `npm run build`.
- The user-facing blog generation target is now `make -C demo/blog generate`, and the root wrapper is `make generate-blog`; the underlying generated Dune alias remains `@_utopia/ssg`, but that shorthand is no longer exposed through the public demo Makefile targets.
- The compiler build-mode regression came from `bin/compiler/compiler.ml` using names outside the public `Esbuild` interface; it now calls the exported values `Esbuild.development` and `Esbuild.production`, which restores `make -C demo/* build`.
- Verification passed for `make -C demo/blog build`, `make -C demo/blog generate`, `make -C demo/notes build`, and `make -n run-demo run-demo-watch compile-demo compile-demo-watch build-generated generate-blog` at repo root.

## Active slice

- [completed] Add a scoped CLI clean mode for transient project build outputs so demo automation can stop shelling out to raw `rm -rf`
- [completed] Replace the remaining in-repo demo `rm -rf` cleanups with the new CLI entrypoint in Makefiles and package scripts
- [completed] Re-run focused CLI/demo verification and update `plan/primitives.md` plus these review notes

## Review

- `bin/cli/Clean.ml` now supports `utopia clean --build-outputs`, which removes only `_utopia/dist`, `_utopia/static`, and the project-scoped Melange target directory under `target/.../_utopia` without running `dune clean` or deleting the checked-in/generated `_utopia/*` scaffold.
- Full `utopia clean` still performs the broader cleanup, but now also removes the project-scoped `target/.../_utopia` output so stale Melange artifacts do not survive a full clean.
- `demo/notes/Makefile`, `demo/blog/Makefile`, `demo/notes/package.json`, and `demo/blog/package.json` now route their transient cleanup through the CLI instead of embedding raw `rm -rf` commands.
- `plan/primitives.md` and `plan/spec.md` now document the narrower `--build-outputs` mode and the extra `target/.../_utopia` cleanup performed by the default clean flow.
- Verification passed for `opam exec -- dune build bin/cli/cli.exe`, `opam exec -- dune runtest bin/tests/cli_clean_with_nothing_to_clean.t bin/tests/cli_clean_build_outputs.t`, `make -n -C demo/notes compile clean`, `make -n -C demo/blog compile clean`, and a focused search showing no remaining `rm -rf` usage in the repo-owned demo Makefiles/package scripts/root Makefile.

## Active slice

- [completed] Add `make -C demo/blog serve` so the static blog can be generated and served from `_utopia/static/` without the generated app server
- [completed] Verify the new static serve target with a live HTTP check and update the docs/task notes

## Review

- `demo/blog/Makefile` now exposes `serve`, which depends on `generate` and then serves the project-local `demo/blog/_utopia/static/` tree through `python3 -m http.server`, with `HOST` and `PORT` overrides.
- The blog `generate` target no longer shells through Dune's `@_utopia/ssg` alias; it runs the built `../../_build/default/demo/blog/_utopia/server_main.exe --ssg` directly from `demo/blog/`, which makes the generated static output land under `demo/blog/_utopia/static/` instead of the repo-root `_utopia/static/`.
- Verification passed for `make -C demo/blog serve PORT=8186`, with live `HEAD /` and `HEAD /posts/hello-utopia/` checks both returning `200` from the plain static file server.

## Active slice

- [completed] Add `# 1` line directives to generated mirrored page and shared-lib wrappers so Merlin/LSP locations point back to the original sources
- [completed] Extend compiler mirror snapshot coverage to pin the new directives for both `.re` and `.ml` wrappers
- [completed] Run focused compiler/demo verification and capture the results here

## Review

- `bin/compiler/Generated_dune.ml` now emits an OCaml line directive immediately after any generated `open!` prelude and before the mirrored source body, using the same relative source path already present in each Dune rule dep (for example `../lib/blog_data.ml` or `../../pages/Home.re`).
- `bin/tests/compiler_autoopens_lib_folder.t` now snapshots those directives for both Reason page/shared-lib mirrors and an `.ml` shared-lib mirror, which pins the exact wrapper shape for both melange and native `_utopia` outputs.
- Focused verification passed for `opam exec -- dune runtest bin/tests/compiler_autoopens_lib_folder.t`, and regenerating `demo/blog/_utopia/dune` via `make -C demo/blog compile` now produces `# 1 "../lib/blog_data.ml"` and `# 1 "../../lib/blog_data.ml"` in the `Utopia_lib__Blog_data.ml` and `Lib__Blog_data.ml` rules.

## Active slice

- [completed] Fix the SRR shared-folder-prefix regression introduced by the new line directives so mirrored sources still preprocess under `make dev`
- [completed] Add regression coverage that exercises `%platform` in a mirrored source and pins the split preprocess prefixes in generated `_utopia/dune`
- [completed] Re-run focused compiler and blog/demo builds, then capture the results here

## Review

- The `make dev` failure was not caused by `copy_files`; it came from `server-reason-react.ppx` validating `loc.pos_fname` against `-shared-folder-prefix`. The new `# 1` directives changed those filenames for mirrored sources, so the PPX prefixes had to be split by module type.
- `bin/compiler/Generated_dune.ml` now emits source-relative PPX prefixes only for mirrored `.ml` and `.re` modules (`../` and `../../`), while generated `_utopia/*` runtime modules keep `_utopia/` and `_utopia/native/`. That preserves the location fix for `blog_data.ml` without breaking SRR's generated runtime modules.
- `.mlx` mirrors now deliberately skip `# 1` directives. Those files pass through the MLX dialect and SRR client-component extraction, which require a stable `_utopia/...` logical filename; using a source-relative directive there causes incompatible filename expectations inside SRR.
- Regression coverage now includes: `bin/tests/compiler_autoopens_lib_folder.t` exercising `switch%platform` in a mirrored `.re` page plus `.ml` shared-lib wrappers with line directives, `bin/tests/compiler_generates_dune_rules.t` pinning the extra `../` and `../../` shared-folder prefixes, and `bin/tests/compiler_supports_mlx_extension.t` proving `.mlx` projects keep `_utopia` prefixes, omit `# 1` directives, and still build with a `[@react.client.component]` page.
- Verification passed for `opam exec -- dune build bin/compiler/compiler.exe`, `opam exec -- dune runtest bin/tests/compiler_generates_dune_rules.t bin/tests/compiler_autoopens_lib_folder.t bin/tests/compiler_supports_mlx_extension.t`, `make -C demo/blog compile`, `opam exec -- dune build demo/blog/_utopia/server_main.exe @demo/blog/_utopia/melange`, and `opam exec -- dune build @all`.

## Active slice

- [completed] Give `demo/blog/lib/` a real source-tree Dune stanza so Merlin sees `blog_data.ml` with SRR PPX context instead of as a raw file
- [completed] Verify `demo/blog/lib/blog_data.ml` now has a preprocess config and still coexists with the generated `_utopia` build
- [completed] Record the results and any remaining gaps for `pages/*.mlx` editor support

## Review

- `demo/blog/dune` now defines a source-tree `subdir lib (library ...)` stanza named `blog_source_lib`, so `demo/blog/lib/blog_data.ml` belongs to a real Dune OCaml library instead of existing only as an input dep to generated `_utopia` wrapper rules.
- That source-tree library uses the native SRR PPX stack with `-shared-folder-prefix=lib/`, depends on `pages_demo_blog` plus the markdown/runtime libraries used by the blog content module, and keeps the existing `-w -26-27-39` warning profile.
- `opam exec -- dune describe pp demo/blog/lib/blog_data.ml` now succeeds and shows `[%platform]` / `match%platform` rewritten away in the preprocessed output, which is the missing condition for ocamllsp to stop reporting `Uninterpreted extension 'platform'` on that source file.
- The new editor-oriented source stanza coexists with the generated build: verification passed for `opam exec -- dune build demo/blog/lib/blog_source_lib.cma demo/blog/_utopia/server_main.exe @demo/blog/_utopia/melange` and `opam exec -- dune build @all`.
- This fix is intentionally scoped to `demo/blog/lib/*.ml`. Raw route files under `pages/*.mlx` still are not covered by a real source-tree stanza, so source-editor support for those files remains a separate, larger problem.

## Active slice

- [completed] Compare why source `lib/` files and source `pages/` files surface different LSP behavior
- [completed] Confirm which paths belong to real source-tree Dune stanzas versus generated `_utopia` wrapper rules
- [completed] Capture the root cause and the remaining follow-up needed for consistent page-source editor support

## Review

- `demo/blog/lib/blog_data.ml` and `demo/blog/pages/layout.mlx` are not owned the same way. `blog_data.ml` belongs to the real source-tree `subdir lib (library ...)` stanza in `demo/blog/dune`, while `layout.mlx` is only compiled through generated `_utopia` wrapper rules in `demo/blog/_utopia/dune`.
- That ownership difference is directly visible in Dune: `opam exec -- dune describe pp demo/blog/lib/blog_data.ml --build-dir _build_lsp_probe_lib` succeeds, while `opam exec -- dune describe pp demo/blog/pages/layout.mlx --build-dir _build_lsp_probe_pages` fails because the source page file is not part of any source-tree stanza.
- The compiler intentionally emits `# 1` line directives for non-`.mlx` mirrored files but skips them for `.mlx` mirrors in `bin/compiler/Generated_dune.ml`, because SRR + MLX preprocessing expects stable `_utopia/...` logical filenames. So page-source diagnostics are not mapped back to `pages/*.mlx` the same way lib-source diagnostics are mapped back to `lib/*.ml`.
- In practice, `pages/` currently looks quieter because it is still a generated-wrapper world, not because it has better direct source-editor support. Making the two folders behave the same will require an editor-oriented page-wrapper strategy for `.mlx` routes, which is a larger follow-up than the small `lib/` source-stanza fix.

## Active slice

- [in_progress] Design a generated `_utopia/dune` editor-support path so both `pages/` and `lib/` use the same approach without project-local source stanzas
- [pending] Implement the generated hidden stanzas/rules, remove the one-off `demo/blog` source-tree `lib` stanza, and keep runtime builds unchanged
- [pending] Add regression coverage for source-owner/editor behavior plus `.mlx` page diagnostics, then verify with focused Dune/compiler checks
- [pending] Update `plan/primitives.md` if the generated `_utopia` editor-support structure becomes a named maintained concept

## Investigation note

- [completed] Verify whether `.mlx` page mirrors can safely use `# 1` source directives while keeping the existing stable `_utopia/...` SRR shared-folder prefixes

## Review

- The user correctly flagged that generated wrappers should place `# 1` directives first. `bin/compiler/Generated_dune.ml` now emits the directive as the first line and re-emits it after any injected prelude, so copied `.ml`/`.re` sources still map back to line 1 of the original file.
- That change is valid for the non-`.mlx` wrappers already using line directives, and `bin/tests/compiler_autoopens_lib_folder.t` was updated to pin the new wrapper shape.
- `.mlx` remains different for a concrete reason: adding `# 1` directives to the real `demo/notes/_utopia/dune` rules for `Utopia_page__Notes__Tag.mlx` immediately caused `Server_reason_react_ppx.Error(_)` in both the melange and native preprocess stanzas, even while leaving the stable `_utopia/` and `_utopia/native/` shared-folder prefixes unchanged.
- So the current `.mlx` exclusion is not just stale policy. The SRR + MLX path still rejects line-directed mirrored `.mlx` files, which is why type errors in `demo/notes/pages/notes/[tag].mlx` still surface through `_utopia/native/Utopia_page__Notes__Tag.mlx` today.

## Active slice

- [completed] Pin `server-reason-react` in the local opam switch to commit `5be5626897` so Utopia uses the improved PPX diagnostics
- [completed] Reproduce the `.mlx` `# 1` failure on `demo/notes/pages/notes/[tag].mlx` against the pinned PPX and capture the exact error text
- [completed] Summarize the precise PPX mismatch that still blocks `.mlx` source mapping

## Review

- The local opam switch now uses `server-reason-react` commit `5be562689712124c1f7344c09f9de4ea9582d13a`, which is the PR branch commit that turns the old `Server_reason_react_ppx.Error(_)` crash into a readable compiler error.
- Baseline reproduction with the user's intentional type error still reported generated filenames (`demo/notes/_utopia/Utopia_page__Notes__Tag.mlx` and `demo/notes/_utopia/native/Utopia_page__Notes__Tag.mlx`), confirming the original source-mapping issue remains in Utopia's generated Dune setup.
- Adding `# 1` directives to the mirrored `[tag].mlx` rules alone now reports the exact blocker: `react.server.function` in `../pages/notes/[tag].mlx` / `../../pages/notes/[tag].mlx` no longer matches the generated `_utopia/` / `_utopia/native/` shared-folder-prefix passed to `server-reason-react.ppx`.
- When `Utopia_page__Notes__Tag` is also moved into source-relative per-module PPX prefixes (`../` for melange and `../../` for native), the PPX succeeds and the build reports the real type error on the source file path instead: `../pages/notes/[tag].mlx` and `../../pages/notes/[tag].mlx`.
- So the corrected conclusion is: `.mlx` source mapping is not blocked by `# 1` itself. It requires `# 1` and matching per-module `-shared-folder-prefix` values to move together for mirrored `.mlx` page modules that contain `react.server.function` / `react.client.component` rewrites.

## Active slice

- [completed] Update `Generated_dune.ml` so mirrored `.mlx` modules opt into both source line directives and source-relative SRR shared-folder-prefixes
- [completed] Extend the `.mlx` compiler regression to cover source-mapped error paths instead of generated `_utopia` filenames
- [completed] Regenerate and verify `demo/notes/_utopia/dune` so the real `[tag].mlx` build now reports the source file path

## Review

- `bin/compiler/Generated_dune.ml` now treats mirrored `.mlx` modules the same way as mirrored `.ml`/`.re` modules: generated page/lib wrappers emit `# 1` line directives, and those same modules move into the source-relative `-shared-folder-prefix=../` / `../../` per-module groups.
- This keeps the two SRR invariants aligned for mirrored `.mlx` sources: the filename seen by the compiler is the source path, and the filename expected by `server-reason-react.ppx` matches that same source path.
- `bin/tests/compiler_supports_mlx_extension.t` now covers the real regression path by generating an `.mlx` page that uses both `[@react.server.function]` and `[@react.client.component]`, verifying the generated `_utopia/dune` contains source line directives plus source-relative prefixes, then introducing a type error and asserting the failing build mentions `pages/Home.mlx` rather than `_utopia/Utopia_page__Home.mlx`.
- Regenerated `demo/notes/_utopia/dune` now shows `# 1` directives for mirrored `.mlx` pages/lib modules and places `Utopia_page__Notes__Tag` in the source-relative per-module preprocess group, which is the concrete fix required for the user's original LSP/build-path issue.
- Verification passed for `opam exec -- dune build bin/compiler/compiler.exe`, `opam exec -- dune runtest bin/tests/compiler_generates_dune_rules.t bin/tests/compiler_supports_mlx_extension.t`, and an isolated `opam exec -- dune build demo/notes/_utopia/server_main.exe @demo/notes/_utopia/melange --build-dir _build_verify_notes_mlx3`, which now reports the user's intentional type error against `../pages/notes/[tag].mlx` and `../../pages/notes/[tag].mlx` instead of the generated `_utopia` paths.

## Follow-up note

- The compiler-side source mapping fix does not give ocamllsp config for `demo/notes/pages/notes/[tag].mlx`. Dune still cannot own `[tag].mlx` as a source module (`bad source file name: [tag] is not a valid module name`), so the editor opens the raw source path without a Dune/Merlin config even though build diagnostics are now remapped onto that same path via `# 1` directives.

## Final choice

- [completed] Revert `.mlx` source mapping so build diagnostics for invalid dynamic-route filenames return to `_utopia/...` paths instead of diverging from what ocamllsp can configure

## Review

- `bin/compiler/Generated_dune.ml` again excludes `.mlx` from source-relative `-shared-folder-prefix` groups and from emitted `# 1` line directives, while keeping the earlier non-`.mlx` line-directive ordering fix intact.
- `bin/tests/compiler_supports_mlx_extension.t` still exercises an `.mlx` page containing both `[@react.server.function]` and `[@react.client.component]`, but now asserts the safe generated-Dune shape for `.mlx`: `_utopia/` and `_utopia/native/` prefixes remain, no source-relative prefixes are emitted, and no `# 1` directives appear for `.mlx` mirrors.
- Regenerated `demo/notes/_utopia/dune` now matches that reverted `.mlx` policy, and an isolated `opam exec -- dune build demo/notes/_utopia/server_main.exe @demo/notes/_utopia/melange --build-dir _build_verify_notes_revert` again reports the user's intentional type error on `demo/notes/_utopia/Utopia_page__Notes__Tag.mlx` and `demo/notes/_utopia/native/Utopia_page__Notes__Tag.mlx`.
- This restores consistency with ocamllsp for dynamic route source files like `[tag].mlx`: the editor still lacks direct config for the raw source filename, but build diagnostics no longer claim the source file is fully configured when Merlin cannot actually typecheck it.
