# Primitives

Canonical glossary of concepts in utopia. Every term used in specs, plans, or code should have a single definition here. When a concept is added or modified anywhere in the project, this file must be updated.

## Core Concepts

### Page
A file inside `pages/` that maps to a URL route. Pages are always **server components**. A page's minimal contract is a single `make` function that returns `React.element`. Path is inferred from the filesystem, layout is inferred from the directory ancestry.

Supported file types: `.re`, `.ml`, `.mlx` (code pages) and `.md` (markdown pages).

A page may export an optional `metadata` function that returns `Utopia_types.metadata` (record with `title : string option` and `description : string option`). The function receives `(string * string) list` params and can compute metadata dynamically. Static pages ignore the params with `_params`. The compiler detects the export and the server uses it to render `<title>` and `<meta name="description">` in `<head>`. If a page also exports bare `let title` or `let description` alongside `metadata`, the compiler emits a warning.

Future planned exports: `static` (SSG flag), `head` (custom head elements).

### Layout
A file named `layout.re` or `layout.ml` placed in any directory under `pages/`. Layouts wrap all child pages and nested layouts within that directory. Layouts compose top-down: a root `pages/layout.re` wraps `pages/about/layout.re` which wraps `pages/about/team.re`.

A layout receives `children` (the rendered child page or nested layout) as its primary prop, plus route context (path, params) for navigation-aware rendering.

Only code pages can be layouts (no `.md` layouts). Exactly one layout per directory; conflicts are compile-time errors.

### Route
A URL path derived from a page's filesystem location. Routes are generated at compile time by the compiler and recorded in the **route manifest**.

### Route Value (`Utopia.Route.t`)
An opaque navigable route value exposed through the generated `Utopia` module. A route value carries a canonical pathname plus encoded query/hash state. User-facing navigation APIs such as `Utopia.Router.Navigate` and `Utopia.useRouter().navigate(...)` accept `Utopia.Route.t` instead of raw strings. The route API also exposes decoded `query_entries` and `hash` accessors for generated route matching.

### Generated Routes Module (`Utopia.Routes`)
A generated module tree emitted into `_utopia/Utopia_routes.ml`. It mirrors the collected page route hierarchy and is the canonical way for user code to construct `Utopia.Route.t` values. Static routes expose `route` values; dynamic routes expose typed `make` builders derived from filename params. The module also emits `Utopia.Routes.Current`, a generated sum type plus parser that decodes a `Utopia.Route.t` back into the matching route constructor and its typed params/query/hash payload.

### Route Schema
An optional code file under project-root `routes/` whose path mirrors a collected route path (for example `routes/index.re`, `routes/notes/index.re`, or `routes/users/[id].re`). A route schema can define `module Params`, `module Query`, and/or `module Hash`.

- `Params` customizes typed path-param encoding/decoding for dynamic routes. It must provide both `encode` and `decode`, using the low-level helper variants in `Utopia_route.Params` so the schema file does not create a module cycle back through `Utopia.Routes`.
- `Query` and `Hash` customize typed query/hash encoding/decoding. Each declared module must provide both `encode` and `decode`.

The generated `Utopia.Routes` builder/parser exposes these schema modules back to user code as `Route_params`, `Route_query`, and `Route_hash` nested modules, and `Utopia.Routes.Current` uses the corresponding `decode` functions during typed current-route matching.

### Route Segment
A single component of a route path. Types:
- **Static**: literal path component (e.g., `about` from `pages/about.re`). Matched case-insensitively at request time.
- **Param (Single)**: dynamic segment matching one path component (e.g., `[id]`)
- **Param (Catch-all)**: dynamic segment matching one or more components (e.g., `[...slug]`)
- **Param (Optional catch-all)**: dynamic segment matching zero or more components (e.g., `[[...slug]]`)

### Group Segment
A directory name wrapped in parentheses (e.g., `(marketing)`). Invisible in the URL path. Used to organize pages without affecting routing. Follows Next.js route group conventions.

### Parallel Slot
A directory name starting with `@` (e.g., `@sidebar`). Ignored for URL path generation. Reserved for future parallel route rendering.

### Page Metadata (`Utopia_types.metadata`)
A record returned by a page's `metadata` function. The server resolves it at request time and renders the corresponding tags in `<head>`. Use `Utopia_types.empty_metadata` as a base and override only the fields you need via functional update (`{ Utopia_types.empty_metadata with title = Some "..." }`). When no metadata function is exported, `<title>` falls back to the route pattern (code pages) or source file path (markdown pages).

Fields:
- `title : string option` — document `<title>`
- `description : string option` — `<meta name="description">`
- `keywords : string list` — `<meta name="keywords">`
- `authors : string list` — `<meta name="author">` (one per entry)
- `canonical : string option` — `<link rel="canonical">`
- `robots : robots option` — `<meta name="robots">` (index, follow, noarchive directives)
- `open_graph : open_graph option` — Open Graph tags (`og:title`, `og:description`, `og:image`, etc.)
- `twitter : twitter option` — Twitter Card tags (`twitter:card`, `twitter:title`, `twitter:image`, etc.)
- `icons : icon list` — `<link rel="icon">` / `<link rel="apple-touch-icon">` entries
- `verification : (string * string) list` — site verification tags (google, yandex, yahoo, or custom)

### Route Manifest
A TSV file at `_utopia/routes.manifest` generated by the compiler. Each row contains: route, kind, source_file, matcher, params, layouts, has_metadata. The server reads this at startup to build its routing table. See "Matcher Format" for the wire representation of route segments.

### API Manifest
A TSV file at `_utopia/api.manifest` generated by the compiler. Each row contains: route, source_file, module, matcher, params, middlewares. Separate from the route manifest to keep page and API concerns isolated.

### Matcher Format
The server-side representation of route segments, used in the manifest's `matcher` field. Differs from filesystem naming:
- Static: `about` (same)
- Single param: `:id` (filesystem: `[id]`)
- Catch-all: `*slug` (filesystem: `[...slug]`)
- Optional catch-all: `**slug` (filesystem: `[[...slug]]`)

### Conflict Key
A normalized route pattern (with param names stripped) used to detect ambiguous routes. Two pages that produce the same conflict key are a compile-time error.

### Frontmatter
YAML metadata block at the top of a markdown page, delimited by `---`. Parsed by the compiler, stripped before rendering. Supported fields: `title`, `description`, `path` (route override), `layout` (explicit layout selection). Not yet implemented.

## Components

### Server Component
The default component type. Rendered on the server using `server-reason-react`. Server components can be async and fetch data directly during rendering (the component IS the loader). They are compiled with the native OCaml toolchain.

### Client Component
A module annotated with `[@react.client.component]`. Compiled with **melange** to JavaScript and shipped to the browser. Props crossing the server-client boundary must be serializable via `melange-json` / `melange-json-native`.

Pages are always server components. To create a fully interactive page, a page's `make` function returns a client component directly.

### Server Function
A function annotated with `[@react.server.function]`. Executes on the server but can be called from client components. The PPX generates a unique ID, registers the function in a server-side registry, and on the client side creates a proxy via `ReactServerDOMEsbuild.createServerReference`. Server functions enable progressive enhancement: forms work without JavaScript (POST to the same page), and with JavaScript, the client calls the server function directly and receives an RSC response.

Server functions can return `Utopia.Route.t` values. Action responses serialize those route values as `{ pathname; request_path; href }`, which lets client code navigate with the typed route object it received instead of reconstructing a string path.

Because Utopia compiles page modules through both native and Melange build paths, page-level form actions must use SRR's explicit action encoding, not a bare function value. Supported page syntax is a platform-guarded action value such as `switch%platform () { | Server => \`Function(action) | Client => "" }`.

Action POST requests use the server-function registry generated into `_utopia/native/FunctionReferences.re`. Utopia accepts `X-Action-ID` as its canonical request header and also recognizes SRR's upstream `ACTION_ID` header for compatibility. Successful POST responses stream `application/react.action` payloads via `ReactServerDOM.create_action_response`.

Client-side direct server-function calls use `ReactServerDOMEsbuild.encodeReply` to choose the request body format. Ordinary argument lists post as encoded `text/plain` bodies, while `Js.FormData.t` arguments post as multipart form-data so the server can decode them with `ReactServerDOM.decodeFormDataReply`.

### RSC Payload (React Flight Protocol)
A binary stream format produced by server-reason-react's `ReactServerDOM` when rendering server components. Contains the serialized component tree with references to client components (by module ID) rather than their code. The client reads this stream via `ReactServerDOMEsbuild.createFromFetch` to reconstruct the UI.

### Client Component Manifest (`bootstrap.js`)
A generated JavaScript file that maps client component module IDs to `React.lazy(() => import(...))` calls. Populated into `window.__client_manifest_map` at runtime. Generated by `server-reason-react.extract_client_components` scanning Melange output for `// extract-client` markers.

### esbuild Plugin (`server-reason-react-esbuild-plugin`)
An esbuild plugin that runs `extract_client_components` before bundling and prepends the bootstrap import to entry points. Configured via `esbuild.config.mjs` generated by the compiler. Runs as a dune rule.

## Build System

### Compiler (`utopia.compiler`)
Scans `pages/` recursively, parses file names into route segments, detects conflicts, collects layouts, validates param accesses, and generates:
- `_utopia/dune` (dune build rules)
- `_utopia/routes.manifest` (route table)
- `_utopia/Utopia_routes.ml` (typed route builders)
- `_utopia/client_entry.re` (RSC client shell)
- `_utopia/esbuild.config.mjs` (esbuild configuration)
- `_utopia/server_main.ml` (per-project server executable)

The compiler now builds `_utopia/dune` as structured `Sexplib0.Sexp` stanzas through the dedicated `dune_sexp` library, instead of hand-concatenating dune source strings.

Static project support sources are no longer embedded as source blobs in `bin/compiler.ml`; the compiler copies them from the dedicated `utopia_runtime` package files into `_utopia/` and `_utopia/native/`.

### Dune Sexp Library (`dune_sexp`)
An internal library under `lib/dune_sexp/` that exposes a narrow helper interface for constructing dune stanzas as structured `Sexplib0.Sexp` values and serializing them. The compiler uses it for `_utopia/dune` generation.

### `_utopia/`
The generated artifacts directory. Contains dune rules, manifests, client entry, esbuild config, and the server executable wiring -- all produced by the compiler. This directory is created and managed by the build system; users should not edit files here. Requires `(dirs :standard _utopia)` in the project's root `dune` file for dune to traverse into it.

Generated files include:
- `dune` -- build rules (copy, melange.emit, library, esbuild, server exe)
- `routes.manifest` -- route table (TSV)
- `Utopia_routes.ml` -- generated typed route tree for public navigation
- `client_entry.re` -- RSC client shell (boots React, calls createFromFetch)
- `esbuild.config.mjs` -- esbuild configuration with the SRR plugin
- `server_main.ml` -- per-project server executable (wires pages to server lib)

Generated build mirrors include:
- root `_utopia/Utopia_page__*.re|.ml|.mlx` page/layout copies for the Melange build
- root `_utopia/Utopia_lib__*.re|.ml|.mlx` shared `lib/` copies for the Melange build
- `_utopia/native/Utopia_page__*` mirrored page/layout copies for the project-scoped native pages library build
- `_utopia/native/Utopia_lib__*` mirrored shared `lib/` copies for the same native build

The compiler injects a small prelude into these mirrored sources so both build paths have `Melange_json.Primitives` in scope and page/layout mirrors can auto-open generated `Lib` aliases consistently.

### Client Entry (`_utopia/client_entry.re`)
A generated Reason file compiled via Melange that serves as the browser-side RSC shell. It imports React, ReactDOM, and ReactServerDOMEsbuild, fetches the RSC payload for the current page from the server, and renders/hydrates the result into the DOM. Included in all pages via `bootstrapModules` in `DreamRSC.stream_html`.

### esbuild Config (`_utopia/esbuild.config.mjs`)
A generated esbuild configuration file that imports `server-reason-react-esbuild-plugin`, configures entry points (the client entry), target directory (Melange output), and output directory. Executed by Node as a dune rule.

### Shared Types Library (`utopia.types`)
A small library containing types shared between the compiler and server: `page_kind`, `param_kind`, `route_segment`, and related utility functions. Eliminates type duplication across executables. Has zero external dependencies.

### `lib/` Folder
A shared code directory at project-root `lib/`. Modules here are automatically available in generated page/layout builds without manual imports. Compiled to both native (via `server-reason-react.ppx`) and JS (via melange). The compiler mirrors shared `lib/` files into both build contexts under internal `Utopia_lib__*` module names, generates a public `Lib` alias module that re-exports them, and injects `open Lib` into generated page/layout mirrors so shared helpers stay available without exposing the internal build-module names.

### Generated Runtime Support Library (`utopia_runtime`)
An internal compiler-support library under `lib/utopia_runtime/` that ships the static source files copied into generated projects: `ReactServerDOMEsbuild.re`, `FunctionReferences.re`, `Utopia.re`, `Utopia_route.ml`, `Utopia_server.ml`, `Utopia_types.ml`, `Utopia_router.re`, `Utopia_router_route.re`, `Utopia_router_link.re`, and `client_entry.re`.

When the compiler runs inside the workspace, it copies these files from either `lib/utopia_runtime/files/` or the canonical source modules under `lib/server/` and `lib/utopia_types/`. When the compiler runs as an installed CLI, it resolves the same files from the package install under the switch `lib/` directory. This keeps static generated support code out of `bin/compiler.ml` and leaves `write-file` usage only for dynamic generated content such as `Lib.re` aliases and `Utopia_routes.ml`.

### `Utopia_path`
An internal native-only path helper backed by `Fpath`. It computes the current workspace root, current project root, project-relative `_utopia/` directory, and generated build artifact locations such as the built `server_main.exe`. CLI/compiler code should use it instead of hand-concatenating path strings, so root projects keep `_build/default/_utopia/server_main.exe` while nested projects resolve `_build/default/<project-path>/_utopia/server_main.exe` correctly.

Workspace detection is based on the nearest enclosing `dune-project`, not an accidental higher-level parent outside the current project. CLI build/dev/clean flows pass that resolved root to `dune --root ...` so nested projects continue to work even when the surrounding temp directory hierarchy contains unrelated Dune files.

### Param Access Validation
The compiler scans code page source files for `params.X` accesses and cross-references them against declared route parameters. Undeclared param accesses produce compile-time errors with fix suggestions.

### Bootstrap Requirement
The compiler creates `_utopia/` if missing, then removes and regenerates all artifacts. The project root `dune` file must include `(dirs :standard _utopia)`.

### Checked-in Demo Workspace
The notes demo also centralizes its repeated action-button styling in `demo/notes/lib/button.mlx`, which exposes shared `Action`, `Submit`, and `Link` components with an explicit `kind` variant (`Default` or `Accent`) while still emitting plain Tailwind utility classes.
The repository's primary checked-in example project lives at `demo/notes/`. Its route/layout files live under `demo/notes/pages/` as `.mlx` sources, while shared data helpers stay under `demo/notes/lib/`. The notes demo's visual components are page-local and inline inside the route files that render them rather than collected in a shared `notes_ui` module. The demo now models a minimal Apple Notes-style shell with one persistent sidebar, a dynamic `/notes/[tag]` tag route plus `/notes/new`, and a SQLite-backed `tags` + `notes` store. Tags persist a route slug, a separate display name, and an optional description. New tags are created from a small sidebar popup, while the note composer selects existing tags only through a custom autocomplete/fuzzy combobox instead of freeform tag creation. Created tags appear in the sidebar even before they contain notes, created notes persist into the selected tag route, and checklist items remain toggleable after creation from those tag-route note views through generated server actions plus router revalidation. The demo keeps its button, scrollbar, selection, and rich-text presentation in Tailwind utility strings directly in the layout/page code, so `demo/notes/styles.css` is now just the Tailwind entrypoint that builds `demo/notes/output.css`. Tag creation, note creation, and checklist toggling all use the generated server action path and return typed `Utopia.Route.t` values directly to the client, so requests flow through the compiler/ppx-registered server-function registry and the generated Dream POST handling without reconstructing raw route strings on the client. `demo/notes/package.json` now regenerates `_utopia/` with the current compiler before building the demo's native and Melange artifacts while capping Dune jobs for this workspace to avoid pathological auto-detected oversubscription. Root helper commands such as `make run-demo`, `make compile-demo`, `make build-generated`, and `bench/bench_http.sh` should target this workspace and launch the generated `server_main.exe` runtime rather than the standalone `utopia.server` source fallback.

## Configuration

### `utopia.ml`
The project configuration file (not yet implemented). An OCaml module that is compiled and validated at build time. Configures:
- **Build settings**: output directories, melange flags, dune profile, optimization level
- **Server settings**: port, host, middleware, static asset paths
- **Routing overrides**: custom rewrites, redirects, or middleware per-route
- **Markdown settings**: custom components, plugins, syntax extensions
- **Page dependencies**: opam/npm packages per page or project-wide

## API

### API Route
A file inside `api/` that maps to an API endpoint (not yet implemented). Uses the same file-based routing conventions as pages (including `[param]`, `[...slug]`, `[[...slug]]`).

An API route exports a single `handler` function with signature: `Dream.request -> Dream.response Lwt.t`. The handler receives the raw Dream request and pattern-matches on HTTP method internally.

### API Middleware
A file named `_middleware.ml` placed in any directory under `api/` (not yet implemented). Applies to all API routes in that directory and below. Composable through directory nesting (same ancestry model as layouts). Contract: `val middleware : Dream.handler -> Dream.handler`.

## Rendering

### RSC (React Server Components)
The primary rendering model. Server components are the default. They render on the server, can perform async data fetching inline, and stream HTML to the client. Powered by `server-reason-react`.

### SSR (Server-Side Rendering)
Available alongside RSC via `server-reason-react.reactDom` APIs. Pages are rendered to HTML on every request.

### SSG (Static Site Generation)
Opt-in per page via a module-level declaration (e.g., `let static = true`). When enabled, the page is rendered at build time and served as a static HTML file. No server-side rendering occurs at request time. Not yet implemented.

### Remote_data (Draft)
An aspirational module similar to SWR or react-query for managing remote data fetching in client components. Provides loading/error/success states and caching. **Not yet designed or implemented** -- included as a placeholder for future work.

## CLI

### `utopia build`
Validates project structure, runs the compiler, then runs `dune build`. Emits a build report with route count and generated artifact paths.

### `utopia dev`
Development workflow: runs the compiler + initial build, starts `dune build -w` (watch mode with RPC), launches the generated per-project server executable at `_build/default/_utopia/server_main.exe` for root projects (or `_build/default/<project-path>/_utopia/server_main.exe` for nested projects), and streams structured build progress/diagnostics via dune RPC. Before spawning the server, the CLI probes the requested `PORT` and, if it is already occupied, increments upward until it finds an available port so dev startup does not fail on a busy default port.

### `utopia prod` (alias: `start`)
Verifies build artifacts exist, then starts the generated per-project server executable at `_build/default/_utopia/server_main.exe` for root projects (or `_build/default/<project-path>/_utopia/server_main.exe` for nested projects). Respects `PORT` and `HOST` environment variables, and probes upward from the requested `PORT` when that port is already occupied so startup can continue on the next free port.

### `utopia clean`
Removes `_build/`, `_utopia/`, and runs `dune clean`. Reports what was removed.

### `utopia info`
Prints tool versions (OCaml, dune, melange, reason, utopia), project paths, route count, and command implementation status.

### Environment Variables
- `PORT` -- Server listen port (default: 8080). Read by server and CLI. `utopia dev`, `utopia prod`, and the server runtime all treat it as the preferred starting port; if that port is already in use they retry on incrementing higher ports instead of failing immediately.
- `HOST` -- Server listen host (default: 127.0.0.1 dev, 0.0.0.0 prod). Read by both the CLI and the server.
- `NO_LOG` -- When set, disables Dream request logging. `dev` sets this by default unless `--verbose`.

## Server

### Server Library (`utopia.server_lib`)
The framework's server logic, extracted as a library at `lib/utopia_server/`. Contains: Dream-based HTTP routing, route manifest loading, generated-route loading, asset serving, streamed SSR response helpers, cache management, and request handling logic. Does NOT contain page-specific code.

### Server Executable (per-project)
A generated executable at `_utopia/server_main.ml` that calls `utopia.server_lib` with compiler-generated route descriptors. It links a generated project-scoped native pages library (for example `pages_demo_notes`) so multiple Utopia projects can coexist in the same Dune workspace without library-name collisions. Generated by the compiler for each project.

The built executable lives at `_build/default/_utopia/server_main.exe` for a root project, or `_build/default/<project-path>/_utopia/server_main.exe` for a nested project. CLI `dev` and `prod` flows should launch this executable instead of the standalone `utopia.server` binary so compiled routes, compiled page modules, and server-function registries are active. At runtime, the shared server startup path also retries upward from the requested `PORT` when bind fails with `EADDRINUSE`, which covers direct `server_main.exe` launches and races after the CLI's preflight port probe.

### Generated Route Descriptor
A typed OCaml value emitted into `_utopia/server_main.ml` for each route. It carries `route`, `matcher`, `params`, `source_file`, `layouts`, `kind`, and the generated router helpers (`router_shell`, `router_tree`, `router_subtree`) needed for SSR and diff navigation. The server library converts these descriptors into its runtime routing table without rereading the manifest.

### Generated Utopia Router API
A generated module named `Utopia` is compiled into each project's pages library. It exposes the public client router surface for user code, including `Utopia.useRouter()`, `Utopia.make`, `Utopia.callServer`, `Utopia.currentUrl`, `Utopia.browserPath`, `Utopia.Route`, `Utopia.Routes`, `Utopia.Router`, and `Utopia.PassThroughLayout`.

`Utopia.useRouter()` returns the current request path, the current `Utopia.Route.t`, a generated `current` match value (`option(Utopia.Routes.Current.t)`), and a `navigate(~history=?, ~freshness=?, route)` function that accepts typed route values for SPA-style navigation. `history` is an explicit variant (`Push` or `Replace`), and `freshness` is an explicit variant (`Use_cache` or `Revalidate`).

`Utopia.Router` mirrors the core router helpers under a namespace and provides `Utopia.Router.Navigate`, a client link component that renders a real same-origin anchor while upgrading clicks to router navigation using `Utopia.Route.t` values.

### Server Rendering
Current implementation renders compiled native page/layout modules when launched through the generated server executable, with manifest/source fallbacks still available through the standalone `utopia.server` binary. HTML, GET RSC payloads, and POST action responses stream through `ReactServerDOM` wrapped by Dream.

Before GET RSC payloads are serialized, Utopia normalizes route-tree and client-prop elements before passing them to `ReactServerDOM.render_model_value`. On the current `server-reason-react` API, `dangerouslySetInnerHTML` is already represented as a `React.JSX.DangerouslyInnerHtml` prop instead of a standalone `React.element`, so Utopia no longer needs a separate raw-inner-html wrapping pass. It still leaves client-component HTML fallback trees untouched so streamed `text/html` markup stays aligned with hydration.

Target rendering uses `DreamRSC` / `ReactServerDOM` from server-reason-react:
- **Initial page load**: `DreamRSC.stream_html(~bootstrapModules, document_element)` renders HTML with embedded RSC payload. Because that payload's root is the full document tree, the generated client entry hydrates the browser `document`, not just `#root`. The server only emits bootstrap `<script>` tags when the expected client bundle asset (currently `dist/client_entry_melange.js`) is actually available, emits stylesheet `<link>` tags for known CSS assets such as `output.css` when present, and when a generated executable sees both source and `_build/default/...` copies of an asset it prefers the source project's `_utopia/dist/` output for bundles while still being able to serve build-root artifacts like `output.css`.
- **Client navigation**: When `Accept: application/react.component` header is present, the generated Utopia router requests either a full route tree or a parent-relative diff tree. If the request also includes `X-Utopia-Current-Path`, the server can answer with a `("diff", parent_route, subtree)` payload when only a nested branch needs to change; otherwise it returns `("full", "", tree)`.
- **Server functions**: `DreamRSC.streamFunctionResponse` handles POST requests for server function invocations.

In the current implementation, initial HTML, GET RSC payloads, and POST server-function action responses are all live through the generated `_build/default/_utopia/server_main.exe` runtime.

### Dev Route Index
When the root path `/` is requested and no index page is registered, the server renders an auto-generated listing of all routes with links. This is a development convenience feature.

### Live Reload
In dev mode, the server injects a small script into HTML responses that opens an SSE connection to `/_utopia/live-reload`. When the CLI detects a successful rebuild via dune RPC, it signals the server which sends an SSE event. The client reloads the page. Full page reload for now (not HMR).

### Server Restart
In dev mode, the CLI monitors the mtime of the per-project server executable. When dune rebuilds it, the CLI kills the old process and spawns a new one. Polling interval: 500ms.

## Markdown

### Markdown Page
A `.md` file in `pages/`. Supports optional YAML frontmatter for metadata (not yet implemented). Converted to React HTML at build time via `utopia.markdown`, and the same shared renderer is reused by the standalone server markdown-page path. The rendered output is served as HTML rather than going through a second independent markdown-to-HTML implementation.

### Markdown Runtime Library (`utopia.markdown_runtime`)
A public native library under `markdown/` that holds Utopia's shared markdown rendering pipeline. It parses CommonMark with `cmarkit`, renders through the React-based markdown element renderer, and exposes string-to-HTML helpers used by both the `utopia.markdown` executable and the server runtime / generated native pages library.

### Ochre Markdown Highlighting
Fenced code blocks in the shared markdown renderer are highlighted natively with `ochre` plus curated `tm-grammars` grammars, not a Node-side highlighter. Highlighted blocks render as `pre.ochre.utopia-markdown-code-block`, while inline code spans carry the `utopia-inline-code` class so consumers such as the notes demo can style them distinctly.

### Custom Components
The markdown renderer uses a `Components.t` record defining overridable functions for HTML elements: `p`, `a`, `h1`-`h6`, `code`, `pre`, `img`, `ul`, `ol`, `li`, `blockquote`, `hr`, `div`, `math_span`, and inline elements (`strong`, `em`, `del`). Users can customize rendering via `lib/` or markdown settings in `utopia.ml`.

Note: `table` and footnote rendering are not yet implemented (will crash on input).

### Task Lists
Markdown task list items with checkboxes: `[ ]` (unchecked), `[x]` (checked), `[~]` (cancelled, rendered with strikethrough).

### Notes Demo Markdown Body (`body_markdown`)
The `demo/notes/` data model now persists note body content as markdown text in SQLite (`body_markdown`) instead of stored HTML. The note page renders that markdown to HTML on the server before passing the rendered string through the client-side note chrome, while previews are derived from the rendered markdown's plain text.

### Heading Anchors
Headings with IDs get auto-generated anchor links (`<a class="anchor">`). The renderer ensures unique IDs across the document.

## Testing

### Cram Tests
End-to-end tests in `bin/tests/` (CLI/compiler) and `markdown/tests/` (rendering). Create fixture directories, run commands, assert output. Primary test mechanism.

### Unit Tests
Core logic tests using alcotest (not yet implemented). Target: routing, segment parsing, manifest parsing, conflict detection.

### Integration Tests
HTTP request/response tests against a running server (not yet implemented).

### Coverage Rule
Every new feature must include at least one test for the happy path and one for an error case.

## Implementation Phases

Work is split into numbered plan documents in `plan/`. Each phase has explicit dependencies on prior phases:

- `00-cleanup` -- Remove dead code, deprecated features, legacy APIs
- `01-shared-types` -- Extract shared types into `utopia.types` library
- `02-compiler-rsc` -- Compiler generates RSC dune rules and new artifacts
- `03-server-rewrite` -- Server library extraction + DreamRSC rendering
- `04-client-components` -- Client component boundary + esbuild pipeline
- `05-api-routes` -- API route scanning, handlers, middleware
- `06-markdown-pipeline` -- Frontmatter, tables, footnotes, RSC integration
- `07-configuration` -- `utopia.ml` configuration module
- `08-ssg` -- Static site generation
- `09-dev-mode` -- Live reload, npm integration, server restart
