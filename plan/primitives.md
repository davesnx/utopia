# Primitives

Canonical glossary of concepts in utopia. Every term used in specs, plans, or code should have a single definition here. When a concept is added or modified anywhere in the project, this file must be updated.

## Core Concepts

### App Directory
The project-root `app/` directory is the canonical route root. It unifies page routes and API routes under one filesystem tree.

Route intent is inferred by filename:
- `page.re|.ml|.mlx` (and `page.md` for markdown) defines a page route
- `route.re|.ml|.mlx` defines an API route

`pages/` and `api/` are legacy roots planned for deprecation after app-directory migration. During the compatibility window, the compiler still reads legacy roots when `app/` is absent. If both models are present, `app/` wins and the compiler emits a warning that legacy roots were ignored.

### Page
A file named `page.re`, `page.ml`, `page.mlx`, or `page.md` inside `app/` that maps to a URL route. Pages are always **server components**. A page's minimal contract is a single `make` function that returns `React.element`. Path is inferred from the filesystem directory path, layout is inferred from the directory ancestry.

Supported file types: `.re`, `.ml`, `.mlx` (code pages) and `.md` (markdown pages).

A page may export an optional `metadata` function that returns `Utopia_types.metadata` (record with `title : string option` and `description : string option`). The function receives `(string * string) list` params and can compute metadata dynamically. Static pages ignore the params with `_params`. The compiler detects the export and the server uses it to render `<title>` and `<meta name="description">` in `<head>`. If a page also exports bare `let title` or `let description` alongside `metadata`, the compiler emits a warning.

Additional planned exports: `head` (custom head elements).

### Layout
A file named `layout.re`, `layout.ml`, or `layout.mlx` placed in any directory under `app/`. Layouts wrap all child pages and nested layouts within that directory. Layouts compose top-down: a root `app/layout.re` wraps `app/about/layout.re` which wraps `app/about/team/page.re`.

A layout receives `children` (the rendered child page or nested layout) as its primary prop, plus route context (path, params) for navigation-aware rendering.

Only code pages can be layouts (no `.md` layouts). Exactly one layout per directory; conflicts are compile-time errors.

### App-local Module
A non-reserved code module (`.re`, `.ml`, or `.mlx`) under `app/` whose basename is not one of `page`, `layout`, `route`, or `_middleware`. App-local modules do not define routes.

They are support modules available to `page.*` and `layout.*` files in the same directory scope (the module's directory and descendants). Example: `app/button.mlx` exposes `Button` to pages/layouts under `app/**`.

### Route
A URL path derived from a page's filesystem location. Routes are generated at compile time by the compiler and exposed through generated route registries.

### Route Value (`Utopia.Route.t`)
An opaque navigable route value exposed through the generated `Utopia` module. A route value carries a canonical pathname plus encoded query/hash state. User-facing navigation APIs such as `Utopia.Router.Link` and `Utopia.useRouter().navigate(...)` accept `Utopia.Route.t` instead of raw strings. The route API also exposes decoded `query_entries` and `hash` accessors for generated route matching.

### Generated Routes Module (`Utopia.Routes`)
A generated module tree emitted into `_utopia/Routes.ml`. It mirrors the collected page route hierarchy and is the canonical way for user code to construct `Utopia.Route.t` values. Static routes expose `route` values; dynamic routes expose typed `make` builders derived from filename params. The module also emits `type t` plus `Routes.of_route`, which decodes a `Utopia.Route.t` back into the matching route constructor and its typed params/query/hash payload.

For native server wiring, the generated `Routes` surface also includes metadata loaders such as `Routes.get_all` (pages) and `Routes.Api.get_all` (API), while keeping client-safe route construction APIs available for Melange builds.

### Route Schema
An optional code file under project-root `routes/` whose path mirrors a collected route path (for example `routes/index.re`, `routes/notes/index.re`, or `routes/users/[id].re`). A route schema can define `module Params`, `module Query`, and/or `module Hash`.

- `Params` customizes typed path-param encoding/decoding for dynamic routes. It must provide both `encode` and `decode`, using the low-level helper variants in `Utopia_route.Params` so the schema file does not create a module cycle back through `Utopia.Routes`.
- `Query` and `Hash` customize typed query/hash encoding/decoding. Each declared module must provide both `encode` and `decode`.

The generated `Utopia.Routes` builder/parser exposes these schema modules back to user code as `Route_params`, `Route_query`, and `Route_hash` nested modules, and `Utopia.Routes.of_route` uses the corresponding `decode` functions during typed route matching.

### Route Segment
A single component of a route path. Types:
- **Static**: literal path component (e.g., `about` from `app/about/page.re`). Matched case-insensitively at request time.
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

### Generated Route Registries
Generated OCaml route metadata loaders used by the runtime instead of TSV manifests. `Routes.get_all ()` returns page route metadata; `Routes.Api.get_all ()` returns API route metadata.

The generated server runtime joins these metadata values with compiled module registries and does not parse `_utopia/routes.manifest` or `_utopia/api.manifest`.

### Matcher Format
The server-side representation of route segments, used in generated route metadata (`matcher`). Differs from filesystem naming:
- Static: `about` (same)
- Single param: `:id` (filesystem: `[id]`)
- Catch-all: `*slug` (filesystem: `[...slug]`)
- Optional catch-all: `**slug` (filesystem: `[[...slug]]`)

### Conflict Key
A normalized route pattern (with param names stripped) used to detect ambiguous routes. Two pages that produce the same conflict key are a compile-time error.

### Frontmatter
YAML metadata block at the top of a markdown page, delimited by `---`. Frontmatter is generic metadata, not route/layout/SSG control in the current markdown plan scope.

Extraction semantics:
- candidate block only when file starts with `---` and has a closing `---`
- parse with `Yaml`
- only object/map roots are treated as frontmatter
- parse failure or non-object root triggers warning + fallback to unchanged markdown body
- duplicate keys are last-key-wins

Special keys:
- `title` and `description` are convenience keys for `<head>` metadata only when they are top-level string scalars
- all other keys are preserved as generic frontmatter metadata

The checked-in `demo/blog/` uses the shared `Utopia_markdown.extract_frontmatter` pipeline in `demo/blog/lib/blog_data.ml` for post metadata/body splitting.

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
Scans route sources recursively, preferring canonical `app/` classification (`page.*` and `route.*`) and falling back to legacy `pages/` + `api/` only when `app/` is absent. It parses directory names into route segments, detects conflicts, collects layouts/middleware, validates param accesses for pages, and generates:
- `_utopia/dune` (dune build rules)
- `_utopia/Routes.ml` (typed route builders)
- route metadata loaders (`Routes.get_all`, `Routes.Api.get_all`)
- `_utopia/client_entry.re` (RSC client shell)
- `_utopia/esbuild.config.mjs` (esbuild configuration)
- `_utopia/server_main.ml` (per-project server executable)

The compiler now builds `_utopia/dune` as structured `Sexplib0.Sexp` stanzas through the dedicated `dune_sexp` library, instead of hand-concatenating dune source strings.

Static project support sources are no longer embedded as source blobs in `bin/compiler.ml`; the compiler copies them from the dedicated `utopia_runtime` package files into `_utopia/` and `_utopia/native/`.

### Dune Sexp Library (`dune_sexp`)
An internal library under `lib/dune_sexp/` that exposes a narrow helper interface for constructing dune stanzas as structured `Sexplib0.Sexp` values and serializing them. The compiler uses it for `_utopia/dune` generation.

### `_utopia/`
The generated artifacts directory. Contains dune rules, generated route modules/registries, client entry, esbuild config, and server executable wiring -- all produced by the compiler. This directory is created and managed by the build system; users should not edit files here. Projects should include the generated rules with `(include _utopia/dune)` and mark `_utopia` as data-only with `(data_only_dirs _utopia)` so Dune does not parse `_utopia/dune` as a nested standalone project.

Generated files include:
- `dune` -- build rules (copy, melange.emit, library, esbuild, server exe)
- `Routes.ml` -- generated typed route tree plus route metadata loaders
- `client_entry.re` -- RSC client shell (boots React, calls createFromFetch)
- `esbuild.config.mjs` -- esbuild configuration with the SRR plugin
- `server_main.ml` -- per-project server executable (wires pages to server lib)

Generated build mirrors include:
- root `_utopia/Utopia_page__*.re|.ml|.mlx` app page/layout copies for the Melange build
- root `_utopia/Utopia_lib__*.re|.ml|.mlx` shared `lib/` copies for the Melange build
- `_utopia/native/Utopia_page__*` mirrored app page/layout copies for the project-scoped native pages library build
- `_utopia/native/Utopia_api__*` mirrored app API handler/middleware copies for the project-scoped native API library build
- `_utopia/native/Utopia_lib__*` mirrored shared `lib/` copies for the same native build

The compiler injects a small prelude into these mirrored sources so both build paths have `Melange_json.Primitives` in scope and page/layout mirrors can auto-open generated `Lib` aliases consistently.

### Client Entry (`_utopia/client_entry.re`)
A generated Reason file compiled via Melange that serves as the browser-side RSC shell. It imports React, ReactDOM, and ReactServerDOMEsbuild, hydrates the current document, and passes the standalone `Utopia_call_server.callServer` transport into the browser-side RSC runtime. Included in streamed HTML responses via `bootstrapModules` when the client bundle asset exists.

### Call Server Runtime (`_utopia/Utopia_call_server.re`)
A generated runtime support module that owns the browser-side server-action transport used by both the initial client entry and client-side RSC navigation. Keeping `callServer` separate from `Utopia_router` prevents the initial client entry from importing the full router runtime just to invoke server actions.

### Client Component Bridge Module (Planned)
A generated module under `_utopia/client_components/` that re-exports one discovered `[@react.client.component]` definition with a stable module identity for Melange/esbuild extraction. Bridges are used as client build entrypoints so server-only page modules do not become default Melange entrypoints.

### Client Reachability Closure (Planned)
The transitive local-module dependency set reachable from discovered client component entrypoints. Planned Melange optimization compiles only this closure (plus required runtime scaffolding) instead of compiling all page/layout mirrors.

### esbuild Config (`_utopia/esbuild.config.mjs`)
A static, runnable JavaScript file that imports `server-reason-react-esbuild-plugin`, configures entry points (the client entry), target directory (Melange output), and output directory. Executed by Node as a dune rule. The config is a static runtime file copied from `lib/utopia/esbuild.config.mjs` — not generated. It imports build metadata from a sibling `_utopia/paths.mjs` module, sets `process.env.NODE_ENV` internally for Node tooling, enables production minification when `buildMode = "production"`, and derives all build paths at runtime. This means the esbuild config is real, directly runnable JavaScript with full editor support, linting, and syntax highlighting.

### esbuild Paths (`_utopia/paths.mjs`)
A tiny generated ESM module containing build metadata exports including `projectPath`, `buildMode`, and `nodeEnv`. The compiler writes this file with the project's workspace-relative path (empty string for root projects, e.g. `"demo/notes"` for nested projects) plus the current Utopia build mode. Direct compiler invocation defaults to `development`; `utopia dev` writes `development`, while production build flows write `production`. The esbuild config imports these values and derives all build/source directory paths from them.

### Shared Types Library (`utopia.types`)
A small library containing types shared between the compiler and server: `page_kind`, `param_kind`, `route_segment`, and related utility functions. Eliminates type duplication across executables. Has zero external dependencies.

### `lib/` Folder
A shared code directory at project-root `lib/`. Modules here are automatically available in generated page/layout builds without manual imports. The current build/runtime path still mirrors shared `lib/` files into generated `_utopia/` build contexts under internal `Utopia_lib__*` module names, generates a public `Lib` alias module that re-exports them, and injects `open Lib` into generated page/layout mirrors so shared helpers stay available without exposing the internal build-module names.

In parallel, the compiler now also generates an optional source-ownership path for editor support inside the same generated `_utopia/dune` file used by the runtime build. Projects should opt into that single generated file by declaring `(include _utopia/dune)` in the root `dune` file and marking `_utopia` as data-only with `(data_only_dirs _utopia)` so Dune does not try to treat `_utopia/dune` as a standalone nested project.

That single generated `_utopia/dune` file defines:
- a generated `_utopia/support/` native library that copies the project-local `Utopia`/route/runtime surface from `_utopia/`, includes `FunctionReferences`, and uses a stub `Utopia_call_server.re`
- a real source-owned `lib/` native library (`source_lib_<project-scope>`) built directly from the user’s `lib/*.ml|*.re|*.mlx` files
- source-owned page libraries grouped by directory (`source_pages_<project-scope>_...`) for app page/layout files whose basenames are valid module names, such as `app/page.mlx`, `app/layout.mlx`, `app/notes/page.mlx`, or `app/notes/[tag]/page.mlx`
- ancestor source-page library links (for example `app/about` source stanzas depend on the `app/` root source-page library when present) so Merlin/ocamllsp can resolve app-local modules from parent directories
- the mirrored runtime/native/melange build stanzas for `_utopia/` itself, wrapped under `(subdir _utopia ...)`

This single-file `_utopia/dune` path exists to give Merlin/ocamllsp real Dune ownership over shared `lib/` source files and page/layout source files whose basenames are valid module names without maintaining a second generated dune file. Dynamic route segments should therefore live in directory names with a `page` file, such as `app/notes/[tag]/page.mlx` or `app/posts/[slug]/page.mlx`, rather than in invalid basenames like `app/notes/[tag].mlx`.

Generated code now depends directly on the shared public `utopia` library for reusable runtime modules (`Utopia`, `Utopia_call_server`, `Utopia_route`, `Utopia_router`, `Utopia_router_link`, `Utopia_router_route`, `Utopia_route_builder`, `Utopia_server`, `Utopia_types`, and `FunctionReferences`). Project-specific route code is emitted separately as top-level `Routes.ml` inside `_utopia/`.

### Generated Project Assets (`lib/utopia/`)
Static generated-project assets now live directly under `lib/utopia/`: `client_entry.re`, `esbuild.config.mjs`, and `ReactServerDOMEsbuild.re`. The compiler resolves and copies those files into `_utopia/` when generating a project.

The helper module `Utopia_runtime` also lives under `lib/utopia/`, but it is now just an internal module inside the shared `utopia` library rather than a separate library.

### `Utopia_path`
An internal native-only path helper backed by `Fpath`. It computes the current workspace root, current project root, project-relative `_utopia/` directory, and generated build artifact locations such as the built `server_main.exe`. CLI/compiler code should use it instead of hand-concatenating path strings, so root projects keep `_build/default/_utopia/server_main.exe` while nested projects resolve `_build/default/<project-path>/_utopia/server_main.exe` correctly.

Workspace detection is based on the nearest enclosing `dune-project`, not an accidental higher-level parent outside the current project. CLI build/dev/clean flows pass that resolved root to `dune --root ...` so nested projects continue to work even when the surrounding temp directory hierarchy contains unrelated Dune files.

### Param Access Validation
The compiler scans code page source files for `params.X` accesses and cross-references them against declared route parameters. Undeclared param accesses produce compile-time errors with fix suggestions.

### Bootstrap Requirement
The compiler creates `_utopia/` if missing, then removes and regenerates all artifacts. The project root `dune` file should include the generated rules with `(include _utopia/dune)` and mark `_utopia` as data-only with `(data_only_dirs _utopia)` so Dune does not parse `_utopia/dune` as a nested standalone project.

The workspace `dune-project` now declares `(lang dune 3.16)` so the `mlx` dialect can use `(merlin_reader mlx)`. That hook is required for Merlin/ocamllsp to read `.mlx` sources as the `mlx` dialect rather than plain OCaml syntax.

### Dune RPC
The dune watch-mode RPC socket used by `utopia dev` for terminal build progress and diagnostic streaming. `bin/cli/Build_rpc.ml` waits for the socket under `_build`, subscribes to dune's `progress` and `diagnostic` streams, keeps the active diagnostics in memory, prints them on build failures, and emits lifecycle hooks (`build_started`, `build_failed`, `build_succeeded`) for the dev-event bridge path. RPC connectivity is helpful but non-fatal: `utopia dev` continues running even if the RPC socket cannot be reached.

### Checked-in Demo Workspace
The notes demo also centralizes its repeated action-button styling in `demo/notes/lib/button.mlx`, which exposes shared `Action`, `Submit`, and `Link` components with an explicit `kind` variant (`Default` or `Accent`) while still emitting plain Tailwind utility classes.
The repository's primary checked-in example project lives at `demo/notes/`. Its route/layout files live under `demo/notes/app/` as `.mlx` sources, while shared data helpers stay under `demo/notes/lib/`. The notes demo's visual components are page-local and inline inside the route files that render them rather than collected in a shared `notes_ui` module. The demo now models a minimal Utopia Notes shell with one persistent sidebar, a dynamic `/notes/[tag]` tag route plus `/notes/new`, and a SQLite-backed `tags` + `notes` store. Tags persist a route slug, a separate display name, and an optional description. New tags are created from a small sidebar popup, while the note composer selects existing tags only through a custom autocomplete/fuzzy combobox instead of freeform tag creation. Created tags appear in the sidebar even before they contain notes, created notes persist into the selected tag route, and checklist items remain toggleable after creation from those tag-route note views through generated server actions plus router revalidation. The demo keeps its button, scrollbar, selection, and rich-text presentation in Tailwind utility strings directly in the layout/page code, so `demo/notes/styles.css` is now just the Tailwind entrypoint that builds `demo/notes/output.css`. Tag creation, note creation, and checklist toggling all use the generated server action path and return typed `Utopia.Route.t` values directly to the client, so requests flow through the compiler/ppx-registered server-function registry and the generated Dream POST handling without reconstructing raw route strings on the client. `demo/notes/package.json` now regenerates `_utopia/` with the current compiler before building the demo's native and Melange artifacts while capping Dune jobs for this workspace to avoid pathological auto-detected oversubscription. Root helper commands such as `make run-demo`, `make compile-demo`, `make build-generated`, and `bench/bench_http.sh` should target this workspace and launch the generated `server_main.exe` runtime rather than the standalone `utopia.server` source fallback.

Local notes demo automation now also lives in `demo/notes/Makefile`; `compile`, `build`, `run`, `dev`, `watch-compile`, and `export` are the canonical entrypoints, and the root helper targets plus `bench/bench_http.sh` delegate to that Makefile instead of npm scripts.

## Configuration

### `utopia.ml`
The project configuration file (not yet implemented). An OCaml module compiled and validated at build time. It records serializable compile-time settings and optional runtime hook keys.

Configures:
- **Build settings**: output directories, melange flags, dune profile, optimization level
- **Server settings**: port, host, middleware, static asset paths
- **Routing overrides**: custom rewrites, redirects, or middleware per-route
- **Markdown settings**: serializable options plus optional runtime component key
- **Page dependencies**: opam/npm packages per page or project-wide

### Config Runner
A generated helper executable that evaluates `utopia.ml` and emits only serializable configuration payloads for compiler consumption. It intentionally does not serialize first-class modules or callback functions.

### Runtime Hooks Registry
A linked runtime registry (planned as `utopia_runtime_registry`) that resolves non-serializable extensions by key at server startup (for example markdown component module overrides). `utopia.ml` stores the key, and runtime wiring resolves the implementation from the registry.

## API

### API Route
A file named `route.re`, `route.ml`, or `route.mlx` under `app/api/` that maps to an API endpoint. Uses the same directory-based routing conventions as pages (including `[param]`, `[...slug]`, `[[...slug]]`, route groups, and parallel slots).

The `/api/*` namespace is reserved for API routes. Page routes that normalize to `/api/*` are compile-time errors.

An API route exports a single `handler` function with signature: `Dream.request -> Dream.response Lwt.t`. The handler receives the raw Dream request and pattern-matches on HTTP method internally.

API handlers should normally return JSON responses using a helper like `Utopia.respond(~status, ~headers, json)`.

### API Middleware
A file named `_middleware.ml` (or `.re` / `.mlx`) placed in any directory under `app/api/`. Applies to all API routes in that directory and below. Composable through physical directory ancestry (same ancestry model as layouts). Contract: `val middleware : Dream.handler -> Dream.handler`.

Middleware composition order is outermost directory first.

### API Params Accessors (`Routes.Api.Params`)
Generated typed key accessors for API path params. They read matched params from request-local storage populated by API route matching.

Value shapes:
- `Single` -> `string`
- `Catch_all` -> `string list`
- `Optional_catch_all` -> `string list` (`[]` means absent)

### API Error Envelope
Framework-generated API errors are JSON with exactly three keys: `error`, `code`, and `path`.

Examples:
- not found: `code = "api_not_found"`
- internal error: `code = "api_internal_error"`

### JSON Response Helper (`Utopia.respond`)
A helper for API handlers to build JSON responses with explicit status and headers. Shape: `Utopia.respond(~status, ~headers, json)`.

## Rendering

### RSC (React Server Components)
The primary rendering model. Server components are the default. They render on the server, can perform async data fetching inline, and stream HTML to the client. Powered by `server-reason-react`.

### SSR (Server-Side Rendering)
Available alongside RSC via `server-reason-react.reactDom` APIs. Pages are rendered to HTML on every request.

### Rendering Mode
Derived from page exports, not declared explicitly. A page without `let before` is static (build-time rendering). A page with `let before` is dynamic (request-time rendering). Markdown pages are always static. See `plan/09-rendering-modes-and-before-hook.md`.

### Before Hook
A page-level export that opts a page into dynamic (request-time) rendering. Detection of `let before` in `Analysis.ml` (`before_export_origin`) determines whether a page is static or dynamic. Runtime wiring (passing request context to the hook and threading its return value to `make`) is a follow-up. Any page that does request-time work (database queries, user-specific data, etc.) should export `let before`.

```ocaml
let before _request = ()
```

### SSG (Static Site Generation)
Pages without `let before` are static by default. Static output is generated by running the generated server in `--ssg` mode (or `utopia export`), which writes HTML into `_utopia/static/` (`/about` -> `_utopia/static/about.html`, `/` -> `_utopia/static/index.html`). Runtime request handling prefers pre-rendered static HTML when available and falls back to SSR when static artifacts are missing. In dev mode (`--dev`), the server always falls back to SSR, bypassing pre-rendered static HTML.

### Remote_data (Draft)
An aspirational module similar to SWR or react-query for managing remote data fetching in client components. Provides loading/error/success states and caching. **Not yet designed or implemented** -- included as a placeholder for future work.

## CLI

### `utopia build`
Validates project structure (`app/` preferred, legacy `pages/` still accepted during compatibility), runs npm preflight (requires `package.json` + resolvable `react`, `react-dom`, `esbuild`, `server-reason-react-esbuild-plugin`, `server-reason-react-server-dom-esbuild`), runs the compiler in `production` mode, then runs `dune build` for the generated server executable. Emits a build report with route count and generated artifact paths.

### `utopia export`
Builds production artifacts and then runs static generation. Flow: project validation, npm preflight, `utopia.compiler --mode production`, `dune build` for the generated server executable, then generated `server_main.exe --ssg` to write `_utopia/static/*`.

### `utopia dev`
Development workflow: validates route source roots (`app/` preferred, legacy `pages/` still accepted during compatibility), runs npm preflight (same package requirements as `utopia build`), runs the compiler in `development` mode + initial generated-server build, starts `dune build -w` (watch mode with RPC), launches the generated per-project server executable at `_build/default/_utopia/server_main.exe` for root projects (or `_build/default/<project-path>/_utopia/server_main.exe` for nested projects), and streams structured build progress/diagnostics via dune RPC.

### Npm preflight
A shared CLI gate used by `utopia build` and `utopia dev` before compiler/build startup. It fails fast when `package.json` is missing or required npm packages are not resolvable, prints `npm install` remediation, and never auto-installs dependencies.

### `utopia prod` (alias: `start`)
Verifies build artifacts exist, then starts the generated per-project server executable at `_build/default/_utopia/server_main.exe` for root projects (or `_build/default/<project-path>/_utopia/server_main.exe` for nested projects). Respects `PORT` and `HOST` environment variables, and probes upward from the requested `PORT` when that port is already occupied so startup can continue on the next free port.

### `utopia clean`
By default, removes `_build/`, `_utopia/`, the project's Melange target output under `target/<project-path>/_utopia` (or `target/_utopia` at workspace root), then runs `dune clean`. Reports what was removed.

`utopia clean --build-outputs` is a narrower variant for transient project outputs only: it removes `_utopia/dist`, `_utopia/static`, and `target/<project-path>/_utopia` without deleting the checked-in/generated `_utopia/*` scaffold or running `dune clean`.

### `utopia info`
Prints tool versions (OCaml, dune, melange, reason, utopia), project paths, route count, and command implementation status.

### Environment Variables
- `PORT` -- Server listen port (default: 8080). Read by server and CLI. Runtime and `utopia dev` treat it as a preferred starting port and may retry upward on conflicts.
- `HOST` -- Server listen host (default: 127.0.0.1 dev, 0.0.0.0 prod). Read by both the CLI and the server.
- `NO_LOG` -- When set, disables Dream request logging. `dev` sets this by default unless `--verbose`.

## Server

### Server Library (`utopia.server_lib`)
The framework's server logic, now owned directly under `lib/utopia/Utopia_server.ml`. Contains: Dream-based HTTP routing, generated route/API registry loading, asset serving, streamed RSC/HTML/action response helpers, cache management, and request handling logic. Does NOT contain page-specific user route code.

### Server Executable (per-project)
A generated executable at `_utopia/server_main.ml` that wires compiler-generated route descriptors into the copied `Utopia_server` support module. It links generated project-scoped native page and API libraries (for example `app_pages_demo_notes` and `app_api_demo_notes`) so multiple Utopia projects can coexist in the same Dune workspace without library-name collisions. Generated by the compiler for each project.

The built executable lives at `_build/default/_utopia/server_main.exe` for a root project, or `_build/default/<project-path>/_utopia/server_main.exe` for a nested project. CLI `dev` and `prod` flows launch this executable directly. At runtime, the shared server startup path retries upward from the requested `PORT` when bind fails with `EADDRINUSE`, which covers direct `server_main.exe` launches and races after the CLI's preflight port probe.

### Generated Route Descriptor
A typed OCaml value emitted by generated route registries for each route. It carries `route`, `matcher`, `params`, `source_file`, `layouts`, `kind`, and the router helpers (`router_shell`, `router_tree`, `router_subtree`) needed for SSR and diff navigation. The router helpers are computed at server startup by `Utopia_route_builder.build_router` from the route's matcher and layout info, rather than generated as inline OCaml expressions. The server library converts these descriptors into its runtime routing table without manifest parsing.

### Route Builder (`Utopia_route_builder`)
A native-only runtime module at `lib/utopia/Utopia_route_builder.ml` that constructs the router tree, shell, and subtree functions from route metadata and layout render functions. The builder implements the boundary nesting algorithm (creating nested `Utopia.Router.Boundary` elements with `PassThroughLayout` defaults) as real, typechecked OCaml rather than string-based code generation. The generated `server_main.ml` passes each route's matcher, page render function, and layout info to `Utopia_route_builder.build_router`, which returns a record of `{ shell; tree; subtree }` functions.

### Generated Utopia Router API
A generated module named `Utopia` is compiled into each project's pages library. It exposes the public client router surface for user code, including `Utopia.useRouter()`, `Utopia.make`, `Utopia.callServer`, `Utopia.currentUrl`, `Utopia.browserPath`, `Utopia.Route`, `Utopia.Routes`, `Utopia.Router`, and `Utopia.PassThroughLayout`.

`Utopia.useRouter()` returns the current request path, the current `Utopia.Route.t`, and a `navigate(~history=?, ~freshness=?, route)` function that accepts typed route values for SPA-style navigation. Typed route matching now happens explicitly through `Utopia.Routes.of_route router.route`. `history` is an explicit variant (`Push` or `Replace`), and `freshness` is an explicit variant (`Use_cache` or `Revalidate`).

`Utopia.Router` mirrors the core router helpers under a namespace and provides `Utopia.Router.Link`, a client link component that renders a real same-origin anchor while upgrading clicks to router navigation using `Utopia.Route.t` values.

### Server Rendering
Current implementation renders compiled native page/layout modules through the generated server executable. HTML, GET RSC payloads, and POST action responses stream through `ReactServerDOM` wrapped by Dream.

Before GET RSC payloads are serialized, Utopia normalizes route-tree and client-prop elements before passing them to `ReactServerDOM.render_model_value`. On the current `server-reason-react` API, `dangerouslySetInnerHTML` is already represented as a `React.JSX.DangerouslyInnerHtml` prop instead of a standalone `React.element`, so Utopia no longer needs a separate raw-inner-html wrapping pass. It still leaves client-component HTML fallback trees untouched so streamed `text/html` markup stays aligned with hydration.

Target rendering uses `ReactServerDOM` from server-reason-react:
- **API requests**: requests under `/api/*` resolve through generated API route metadata + middleware/handler registries before server-action and page-route dispatch. Framework-generated API misses/exceptions return JSON envelopes.
- **Initial page load**: `ReactServerDOM.render_html` streams HTML with embedded RSC payload. Because that payload's root is the full document tree, the generated client entry hydrates the browser `document`, not just `#root`. The server only emits bootstrap `<script>` tags when the expected client bundle asset (currently `dist/client_entry_melange.js`) is actually available, emits stylesheet `<link>` tags for known CSS assets such as `output.css` when present, and when a generated executable sees both source and `_build/default/...` copies of an asset it prefers the source project's `_utopia/dist/` output for bundles while still being able to serve build-root artifacts like `output.css`.
- **Client navigation**: When `Accept: application/react.component` header is present, the generated Utopia router requests either a full route tree or a parent-relative diff tree. If the request also includes `X-Utopia-Current-Path`, the server can answer with a `("diff", parent_route, subtree)` payload when only a nested branch needs to change; otherwise it returns `("full", "", tree)`.
- **Server functions**: POST requests decode action arguments and stream `application/react.action` payloads via `ReactServerDOM.create_action_response`.

In the current implementation, initial HTML, GET RSC payloads, and POST server-function action responses are all live through the generated `_build/default/_utopia/server_main.exe` runtime.

### Dev Route Index
When the root path `/` is requested and no index page is registered, the server renders an auto-generated listing of all routes with links. This is a development convenience feature.

### Live Reload
Planned dev-mode browser behavior. Authoritative design is in `plan/11-dev-full-reload-and-browser-overlay.md`: full reload only (no HMR/state preservation/per-module swap), plus in-browser build/runtime diagnostics via a unified overlay. Current `utopia dev` restarts the generated server and streams dune RPC diagnostics to the terminal only; browser feedback wiring is still planned.

### Server Restart
In dev mode, the CLI restarts the generated per-project server executable after successful rebuilds (mtime change): send SIGTERM, wait with timeout, send SIGKILL if needed, then spawn replacement. Port selection uses preferred-port fallback semantics and may move to the next available port.

### Dev Event Channel
Planned development-only server channel for browser build-state observation. The intended design is an SSE subscription endpoint (`GET /_utopia/dev-events`) plus a CLI-authenticated publish endpoint (`POST /_utopia/dev-events`) guarded by a per-session token.

### Dev Publish Token
Planned per-session secret token for authenticating CLI-originated dev-event publishes. Not implemented yet.

### Build Diagnostic Overlay
Planned development-only in-browser overlay for dune/compiler build failures. Not implemented yet.

### Runtime Error Overlay
Planned development-only in-browser overlay slice for browser/runtime failures after the page has loaded, including hydration/bootstrap failures, client navigation failures, server-action failures, uncaught `window.onerror` exceptions, and `unhandledrejection` events. Not implemented yet.

## Static Site Generation (SSG)

### Static Page
A page that is rendered at build time. Code pages are static when they do **not** export `let before`. Markdown pages are always static. Detection uses the `Analysis` comment/string-safe lexical scanner. Static pages are rendered by the `--ssg` mode of the generated `server_main.exe`, producing HTML files in `_utopia/static/`.

### Compiler Analysis Scanner (`Analysis`)
A lightweight lexical scanner used by the compiler to tokenize code while ignoring comments and string/char literals. It powers `let before` export detection (determines dynamic vs static), `let paths` export detection, and reusable attribute scans such as `[@react.client.component]` discovery for Melange optimization planning.

### paths
An export required on static pages with dynamic segments (e.g., `app/posts/[slug]/page.mlx`). Returns `(string * string) list list` enumerating all param combinations to render. The compiler validates its presence and the SSG renderer iterates over the returned paths.

### SSG Mode (`--ssg`)
The generated `server_main.exe` accepts a `--ssg` CLI flag. When invoked with `--ssg`, instead of starting a web server it renders all static pages to HTML files in `_utopia/static/`, copies stylesheets, and exits. The dune alias `(alias ssg)` invokes this mode.

### Blog Demo (`demo/blog/`)
A static blog demo showcasing SSG. Renders 4 markdown files from `content/` using `Utopia_markdown.element_of_doc` with custom Tailwind-styled `Components`. Pages are static by default and the dynamic-segment page exports `paths`. Design inspired by shud.in: sidebar navigation, dot-leader blog list, clean typography. Local demo automation lives in `demo/blog/Makefile`; `build`, `run`, `export`, and `serve` are the canonical entrypoints instead of `package.json` scripts. `export` invokes `utopia export` (after building `output.css`) so static output lands in `demo/blog/_utopia/static/`. `serve` first regenerates that directory and then serves it as plain static files via `python3 -m http.server`, honoring `HOST` and `PORT`.

### Markdown Demo (`demo/md/`)
A focused markdown-routing demo with a minimal dark monospaced shell (`app/layout.mlx`) and a minimal index route (`app/page.mlx`) that only links to three markdown pages: `app/faq/page.md`, `app/progress/page.md`, and `app/manifest/page.md`. The palette uses dark/light tones with opacity variance plus a single accent color (`#FFA759`). Local automation lives in `demo/md/Makefile`; `compile`, `build`, `run`, `export`, and `serve` are the canonical entrypoints.

## Markdown

### Markdown Page
A `page.md` file in `app/`. Markdown pages render through the same React/RSC pipeline as code pages rather than a separate HTML-only markdown path.

Frontmatter on markdown pages is generic YAML metadata. The compiler parses and embeds frontmatter data plus stripped markdown body into generated metadata tables, and runtime rendering consumes those embedded payloads instead of re-reading markdown source files.

### Frontmatter Value Tree (`Utopia.Markdown.frontmatter_value`)
A Utopia-owned value type used to expose parsed YAML frontmatter without leaking parser-library AST types:
- `Null`
- `Bool`
- `Number`
- `String`
- `List`
- `Object`

Frontmatter API root values are object/map only.

### Markdown Frontmatter Registry (`Utopia.Markdown.frontmatter`)
A server-only lookup API keyed by concrete request path. Shape: `frontmatter(~path) : frontmatter_object option`.

Lookup behavior:
- accepts concrete request paths (e.g. `/posts/hello`)
- resolves dynamic markdown routes by matching compiled route patterns internally
- returns `None` when path is not a markdown route or no valid frontmatter object exists

### Markdown Runtime Library (`utopia.markdown_runtime`)
A public native library under `markdown/` that holds Utopia's shared markdown rendering pipeline. It parses CommonMark with `cmarkit`, renders through the React-based markdown element renderer, and exposes string-to-HTML helpers used by both the `utopia.markdown` executable and the server runtime / generated native pages library.

### Ochre Markdown Highlighting
Fenced code blocks in the shared markdown renderer are highlighted natively with `ochre` plus curated `tm-grammars` grammars, not a Node-side highlighter. Highlighted blocks render as `pre.ochre.utopia-markdown-code-block`, while inline code spans carry the `utopia-inline-code` class so consumers such as the notes demo can style them distinctly.

### Custom Components
The markdown renderer uses a `Components.t` record defining overridable functions for HTML elements: `p`, `a`, `h1`-`h6`, `code`, `pre`, `img`, `ul`, `ol`, `li`, `blockquote`, `hr`, `div`, `math_span`, and inline elements (`strong`, `em`, `del`). Users can customize rendering via `lib/` or markdown settings in `utopia.ml`.

The markdown pipeline plan extends this surface with granular table hooks (`table`, `thead`, `tbody`, `tr`, `th`, `td`) and granular footnote hooks (`footnotes_section`, `footnotes_list`, `footnotes_item`, `footnote_ref`, `footnote_backref`).

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
Core logic tests using alcotest (not yet implemented). Target: routing, segment parsing, generated route metadata loading, conflict detection.

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
- `07-ssg` -- Static site generation
- `08-dev-mode` -- Live reload, npm integration, server restart
- `09-rendering-modes-and-before-hook` -- Explicit rendering mode + request-time before hook model
- `10-client-error-overlay` -- Deprecated; runtime overlay slice merged into `11-dev-full-reload-and-browser-overlay`
- `11-dev-full-reload-and-browser-overlay` -- Authoritative dev overlay + full reload flow
- `12-optimization-for-melange-pages` -- Melange reachability/entrypoint optimization
- `13-not-found-page` -- Explicit not-found page routing model
- `14-app-directory-unification` -- unify `pages/` + `api/` under `app/` with `page.*`/`route.*`
- `15-configuration` -- `utopia.ml` configuration module
