# Utopia Specification

North-star feature specification for utopia. This document describes the complete target state of the framework. All terminology follows `plan/primitives.md`.

Primary audience: AI coding agents. Decisions are final -- no "maybe" or "TBD".

Each feature is marked: **(implemented)**, **(partial)**, or **(not implemented)**.

## Overview

Utopia is a static site generator and web framework for OCaml/Reason built on `server-reason-react`. It follows a file-based routing convention with React Server Components (RSC) as the primary rendering model. It wraps dune, generates build rules, and provides a CLI for development and production workflows.

Node.js and npm are required as build dependencies. esbuild handles client-side JS bundling with per-page code splitting.

## Project Structure

A utopia project has this layout:

```
my-project/
  utopia.ml              # Project configuration (OCaml module)
  package.json           # npm dependencies (react, esbuild, etc.)
  pages/                 # Page files (routes)
    layout.re            # Root layout (optional)
    index.re             # Root page (/)
    about.re             # /about
    about/
      layout.re          # Layout for /about/* (optional)
      team.re            # /about/team
      [id].re            # /about/:id (dynamic)
    blog/
      [...slug].re       # /blog/* (catch-all)
    guide.md             # /guide (markdown page)
    lib/                 # Shared code (auto-opened everywhere)
      counter.re         # [@react.client.component] - interactive widget
      db.ml              # Server-only database access
      auth.ml
  api/                   # API route handlers
    _middleware.ml        # Global API middleware
    health.ml            # /api/health
    users/
      _middleware.ml      # Middleware for /api/users/*
      [id].ml            # /api/users/:id
  _utopia/               # Generated (do not edit)
    dune                 # Copy rules + melange.emit + native library + esbuild rule + server exe
    routes.manifest      # Route -> page mapping
    client_entry.re      # Generated RSC client shell
    esbuild.config.mjs   # Generated esbuild config with plugin
    server_main.ml       # Generated server executable (wires pages to server lib)
```

## Configuration (`utopia.ml`) (not implemented)

The project configuration is an OCaml module compiled and validated at build time.

### Configurable sections

**Build settings**
- Output directories for compiled artifacts
- Melange compiler flags
- Dune profile selection (dev/release)
- Optimization level

**Server settings**
- Default port and host
- Static asset serving paths
- Response headers

**Routing overrides**
- Custom rewrites (map one URL pattern to another)
- Redirects (permanent/temporary)
- Per-route middleware (for pages, applied before layout)

**Markdown settings**
- Custom component overrides (replace default HTML elements)
- Syntax extensions (e.g., math, footnotes)
- Plugin hooks for pre/post processing

**Page dependencies**
- Project-wide opam library dependencies
- Per-page or per-directory dependency overrides
- npm package dependencies (for melange/client code)

## Pages

### File types (implemented)

| Extension | Kind | Compilation target |
|-----------|------|--------------------|
| `.re` | Code page (Reason) | Native (server) |
| `.ml` | Code page (OCaml) | Native (server) |
| `.mlx` | Code page (OCaml + JSX) | Native (server) |
| `.md` | Markdown page | HTML via cmarkit |

### Page contract (not implemented)

A page module must export:

```ocaml
val make : unit -> React.element
```

That is the minimal contract. Path and layout are inferred from the filesystem.

Optional exports:

```ocaml
val title : string               (* Page title for <head> *)
val description : string         (* Meta description *)
val static : bool                (* true = SSG at build time, default false *)
val head : unit -> React.element (* Custom <head> elements *)
```

Note: the current codebase uses a legacy `Loader_page` contract (`path` + `loader` + `make : data -> React.element` + `layout`) in `Page.ml` and the demo. This will be replaced by the filesystem-inferred contract above.

### Pages are server components (partial)

Every page is a server component. It renders on the server and can perform async data fetching inline -- the component IS the loader. There is no separate `getServerSideProps` or `loader` function.

To create a fully interactive page, the page's `make` function returns a client component:

```reason
/* pages/dashboard.re -- server component */
[@react.component]
let make = () => {
  <Dashboard_client initial_data={fetch_data()} />
};
```

Where `Dashboard_client` is annotated with `[@react.client.component]`.

Note: the current server does not execute compiled React components. It reads source files and renders them as `<pre>` text. Full server-reason-react rendering is the target.

## Client Components (not implemented)

A module becomes a client component by adding the `[@react.client.component]` attribute. The compiler detects this and routes the module through melange for JavaScript compilation.

```reason
/* components/counter.re */
[@react.client.component]
[@react.component]
let make = (~initial_count) => {
  let (count, set_count) = React.useState(() => initial_count);
  <button onClick={_ => set_count(c => c + 1)}>
    {React.string(string_of_int(count))}
  </button>
};
```

Note: the compiler currently does not inspect module attributes. All code pages are blindly copied to both melange and native targets. Client component boundary detection is not yet implemented.

### Client component compilation flow (not implemented)

When a module has `[@react.client.component]`:

**Native PPX** (`server-reason-react.ppx`) transforms the component to emit `React.Client_component { import_module; props; client }`, so the server's RSC renderer includes a client component reference in the RSC payload instead of rendering the component's body.

**Melange PPX** (`reason-react-ppx` + `browser_ppx -js`) compiles the component to JS normally. It also emits `// extract-client <path> <module>` comments and a `make_client` function in the compiled JS output.

**`extract_client_components`** (OCaml binary from server-reason-react) scans the melange `target/` directory for `// extract-client` markers and generates `bootstrap.js`:

```javascript
import React from "react";
window.__client_manifest_map = window.__client_manifest_map || {};
window.__client_manifest_map["path/to/module"] = React.lazy(() =>
  import("./target/Module.js").then(m => ({ default: m.make_client }))
);
```

**esbuild** bundles the client entry + bootstrap.js. Dynamic `import()` calls create separate chunks per client component.

**On the client**: `ReactServerDOMEsbuild.createFromFetch` reads the RSC stream and resolves client component references via `window.__client_manifest_map`.

### RSC boundary (not implemented)

Props crossing the server-to-client boundary must be JSON-serializable. Serialization is handled by `melange-json` (client) and `melange-json-native` (server), which must be installed as project dependencies. The framework ensures both libraries are available as ppxes and libraries for all compilation targets.

Note: `melange-json` and `melange-json-native` are not yet in the dependency list.

### Server Functions (not implemented)

A function annotated with `[@react.server.function]` executes on the server but can be called from client components. The PPX generates a unique ID and registers the function in a server-side registry. On the client side, a proxy is created via `ReactServerDOMEsbuild.createServerReference`. Server functions enable progressive enhancement: forms work without JavaScript (POST to the same page), and with JavaScript, the client calls the server function directly and receives an RSC response.

## Layouts (partial)

### Discovery (implemented)

A `layout.re` or `layout.ml` file in any `pages/` subdirectory defines a layout for that directory and all descendants.

### Nesting (implemented)

Layouts compose top-down by directory ancestry:

```
pages/layout.re          -> wraps everything
pages/about/layout.re    -> wraps pages/about/* (nested inside root layout)
```

### Contract (partial)

A layout receives:

```reason
[@react.component]
let make = (~children: React.element) => {
  <div className="layout">
    <nav> ... </nav>
    children
  </div>
};
```

Layouts receive `children` as the rendered child page or nested layout. They also receive route context (current path, params) via React context provided by the framework.

Note: the current layout contract in `Page.ml` is `?key -> ~title -> ~scripts -> ~children -> unit -> React.element`. The spec contract above is the target.

### Rules (implemented)

- Only code pages (`.re`, `.ml`, `.mlx`) can be layouts. No markdown layouts.
- Exactly one layout per directory. Two layout files in the same directory is a compile-time error.
- Layouts handle page-level concerns: auth checks, redirects, custom headers.

## Routing

### File-based routing (implemented)

The filesystem is the router. No programmatic route definitions. The `ppx_deriving_router` experiment is abandoned.

Note: `Page.ml` still contains a programmatic `register`/`page` API. This is legacy code to be removed.

### Segment conventions (Next.js-compatible) (implemented)

| Pattern | Example file | Matches |
|---------|-------------|---------|
| Static | `pages/about.re` | `/about` |
| Index | `pages/about/index.re` | `/about` |
| Dynamic | `pages/users/[id].re` | `/users/123` |
| Catch-all | `pages/blog/[...slug].re` | `/blog/a/b/c` (1+ segments) |
| Optional catch-all | `pages/docs/[[...slug]].re` | `/docs` or `/docs/a/b` (0+ segments) |
| Route group | `pages/(marketing)/pricing.re` | `/pricing` (group invisible in URL) |
| Parallel slot | `pages/@sidebar/nav.re` | Ignored for URL path |

### Route matching (implemented)

At request time, the server matches URL segments against routes ordered by specificity:
1. Static segments (highest priority, score 4)
2. Single dynamic params (score 3)
3. Catch-all params (score 2)
4. Optional catch-all params (lowest priority, score 1)

Static segment matching is case-insensitive.

### Conflict detection (implemented)

The compiler normalizes routes into conflict keys (param names stripped). Two pages producing the same conflict key are a compile-time error. The compiler reports which files conflict and suggests a canonical file.

### Param access validation (implemented)

The compiler scans code page source files for `params.X` accesses and cross-references them against declared route parameters. Undeclared param accesses produce compile-time errors.

## API Routes (not implemented)

### Location

API routes live in the `api/` directory. The compiler scans `api/` using the same recursive traversal and segment parsing as `pages/`.

### Routing conventions

Identical to page routing: `[param]`, `[...slug]`, `[[...slug]]`, route groups, all work the same way.

### Handler contract

An API route exports a single handler function:

```ocaml
val handler : Dream.request -> Dream.response Lwt.t
```

The handler receives the raw Dream request. HTTP method dispatch is handled by the user inside the handler via pattern matching on `Dream.method_`.

### API middleware

A file named `_middleware.ml` in any `api/` subdirectory applies to all routes in that directory and descendants. Middleware composes by directory ancestry (same model as layouts).

Middleware contract:

```ocaml
val middleware : Dream.handler -> Dream.handler
```

A middleware wraps the downstream handler, enabling pre/post processing (auth, logging, CORS, rate limiting).

## Rendering Models

### RSC (Primary) (partial)

React Server Components are the default. Every page and layout is a server component. Components render on the server, can perform async operations inline, and stream output to the client. Powered by `server-reason-react`.

Server rendering uses `DreamRSC` from server-reason-react:

**Initial page load** (GET without RSC header):
```
Browser -> GET /about -> Server renders React tree -> DreamRSC.stream_html
  -> HTML with <script> tags for esbuild output
  -> Browser receives HTML, boots client shell, hydrates
```

**Client-side navigation** (GET with `Accept: application/react.component`):
```
Client JS -> fetch("/about", {headers: {"Accept": "application/react.component"}})
  -> Server renders React tree -> DreamRSC.stream_model_value
  -> RSC payload (binary flight stream)
  -> Client reads stream via createFromFetch, updates UI without full page reload
```

**Server function** (POST):
```
Client JS -> POST /about (with action ID + args)
  -> DreamRSC.streamFunctionResponse
  -> Executes server function, returns response
```

Note: the server currently does not run compiled React components. Code pages are rendered as escaped source text in `<pre>` tags. Full RSC rendering is the target.

### SSR (partial)

Available via `server-reason-react.reactDom` APIs alongside RSC. Server-side rendering happens on every request. This is the default behavior for pages that don't opt into SSG.

### SSG (Opt-in) (not implemented)

A page opts into static generation by exporting:

```ocaml
let static = true
```

When `static = true`, the page is rendered once at build time. The resulting HTML is served directly without server-side rendering at request time.

### Remote_data (Draft -- not implemented)

An aspirational module for client-side data fetching, similar to SWR or react-query. Would provide:
- Loading/error/success state management
- Request caching and deduplication
- Stale-while-revalidate semantics
- Integration with RSC for initial data hydration

This is a concept only. No design or implementation exists yet.

## Page Scripts (`@utopia.script`) (deprecated -- replaced by RSC)

The `@utopia.script` directive is deprecated and will be removed. Client-side interactivity is now handled through React Server Components and the RSC pipeline.

### Client-side code (RSC pipeline)

Client components are marked with `[@react.client.component]`. The PPX handles server/client separation. esbuild bundles client components with code splitting. The RSC protocol handles hydration. Server functions (`[@react.server.function]`) enable server-side logic callable from client components.

## Shared Code (`lib/`) (implemented)

The `pages/lib/` directory contains shared modules available everywhere:
- Pages (code and markdown custom components)
- API routes
- Layouts
- Scripts

The compiler generates namespace modules (`Lib_melange`, `Lib_native`) that re-export all `lib/` modules. These are automatically opened via `-open` flags in both the melange and native compilation stanzas.

## Markdown Pipeline

### Parsing (implemented)

Markdown pages are parsed with `cmarkit` (CommonMark). Supports:
- Standard CommonMark syntax
- Extensions: math spans, strikethrough
- Task list items with checkboxes (`[ ]` unchecked, `[x]` checked, `[~]` cancelled with strikethrough)
- Heading anchor links with auto-generated unique IDs
- Unsafe HTML pass-through

Note: footnotes are tracked in renderer state but will crash (`assert false`) if encountered. Table rendering is also unimplemented and will crash. The `safe` parameter is accepted but silently ignored (always `true`).

### Frontmatter (not implemented)

Markdown pages support YAML frontmatter delimited by `---`:

```markdown
---
title: My Guide
description: A comprehensive guide
path: custom-route
layout: pages/docs/layout.re
---

# Content starts here
```

Supported fields:
- `title` -- Page title for `<head>`
- `description` -- Meta description
- `path` -- Override the filesystem-inferred route path
- `layout` -- Explicitly select a layout file (override directory ancestry)

The compiler parses frontmatter, strips it before markdown rendering, and records metadata in the route manifest.

### Rendering (implemented)

The `utopia.markdown` executable converts markdown to server-rendered React HTML using `server-reason-react`. Every HTML element is rendered through a customizable component function (defined in `Components.t`).

Note: the server also has a separate markdown rendering path using `Cmarkit_html.of_doc` directly (plain HTML, not React). These two paths should be unified.

### Customization (partial)

Users override default markdown rendering by providing custom component implementations. The `Components.t` record defines functions for: `p`, `a`, `h1`-`h6`, `code`, `pre`, `img`, `ul`, `ol`, `li`, `blockquote`, `hr`, `div`, `math_span`, and inline elements (`strong`, `em`, `del`).

Custom components are provided via:
1. Shared modules in `lib/` (available at build time)
2. Markdown settings in `utopia.ml`

## CLI

### Commands (implemented)

| Command | Alias | Description |
|---------|-------|-------------|
| `utopia build` | -- | Validate structure, run compiler, run `dune build`, emit report |
| `utopia dev` | -- | Bootstrap build, start `dune -w`, start server, stream RPC diagnostics |
| `utopia prod` | `start` | Verify artifacts, start production server |
| `utopia clean` | -- | Remove `_build/`, `_utopia/`, run `dune clean` |
| `utopia info` | -- | Print versions, paths, route count, command status |

### `build` flow (implemented)

1. Validate project shape (`pages/` must exist)
2. Run `utopia.compiler` (generate manifests + dune rules)
3. Run `dune build .` (compile native server + melange client)
4. Emit build report (route count, generated files, output dirs)
5. Fail fast on route conflicts, invalid segments, script errors

### `dev` flow (implemented)

1. Validate project shape
2. Run initial `utopia.compiler` + `dune build`
3. Start `dune build -w .` (watch mode with RPC server)
4. Start `utopia.server` (dev server)
5. Connect to dune RPC, subscribe to progress and diagnostics
6. Stream structured build status to terminal
7. Handle SIGINT/SIGTERM for clean teardown

**RSC dev mode** (not implemented): `dune build -w` watches source files and recompiles native + melange. The esbuild dune rule reruns automatically when melange output changes (dependency-tracked). Client-side: live reload (full page reload on rebuild) for first version, not HMR. Server executable restarts on recompilation (CLI manages subprocess lifecycle).

### `dev` flags (implemented)

| Flag | Default | Description |
|------|---------|-------------|
| `--port` | `$PORT` or `8080` | Server port |
| `--host` | `$HOST` or `127.0.0.1` | Server host |
| `--no-watch` | `false` | Disable dune watch (no RPC) |
| `--verbose` | `false` | Show request logs and RPC debug output |

### `prod` flow (implemented)

1. Verify `_utopia/routes.manifest` and `_utopia/dune` exist
2. Resolve `PORT` and `HOST` from environment
3. Start `utopia.server` as subprocess
4. Forward exit code

### Environment variables (partial)

| Variable | Used by | Description |
|----------|---------|-------------|
| `PORT` | server, CLI | Server listen port (default: 8080) |
| `HOST` | CLI only | Server listen host (default: 127.0.0.1 dev, 0.0.0.0 prod) |
| `NO_LOG` | server | When set, disables Dream request logging. `dev` sets this by default unless `--verbose`. |

Note: the server reads `PORT` but does NOT read `HOST`. The `Dream.run` call lacks `~interface`. HOST is passed in the environment by the CLI but ignored by the server. This is a bug.

### Executable aliasing (implemented)

The CLI supports executable aliases: `utopia-build` is equivalent to `utopia build`. The binary inspects `argv[0]` and extracts the subcommand from the `utopia-` prefix.

## Server

### Architecture (not implemented)

Server framework logic stays in `bin/` as a **library** (`utopia.server_lib`). The compiler generates a per-project **executable** in `_utopia/server_main.ml` that:
- Depends on `utopia.server_lib` (framework: routing, RSC rendering, asset serving)
- Depends on `pages` library (user page modules)
- Wires route definitions to page components and starts the Dream server

This separation means the framework server logic is reusable and the user's page code is linked in at build time.

### Request handling (partial)

1. Parse request target into URL segments
2. If target starts with `target/` or `dist/`, serve as static asset from `_utopia/` or `_build/default/_utopia/`
3. If segments are empty, serve a dev route index page (auto-generated listing of all routes with links)
4. Match segments against routes (specificity-ordered)
5. For code pages: read source file and render as escaped text in `<pre>` tags (placeholder -- target is server-reason-react rendering)
6. For markdown pages: render via cmarkit HTML (not the React pipeline)
7. Wrap content in layout chain (as HTML string wrapping, not React composition)
8. Inject script tags from scripts manifest
9. Return 404 for unmatched routes

**Target request handling (RSC)**:
- **GET** (no RSC header): `DreamRSC.stream_html(~bootstrapModules, document_element)` returns full HTML page with embedded RSC payload and `<script>` tags for esbuild output
- **GET** with `Accept: application/react.component`: `DreamRSC.stream_model_value(~location, element)` returns RSC payload for client-side navigation
- **POST**: `DreamRSC.streamFunctionResponse` handles server function invocations
- **GET /dist/\***: Serve bundled JS assets from esbuild output

### Caching (implemented)

The server uses an mtime-based page cache. Each cache entry stores `(mtime, rendered_html)`. On request, a `stat()` call (~1us) checks if the source file changed. If mtime matches, the cached HTML is returned directly. Cache keys combine source_file + route + params to handle the same file at different param values.

If a source file disappears between requests, the server renders without caching (graceful degradation).

### Asset serving (implemented)

Static assets are resolved from two roots in order:
1. `_utopia/` (generated artifacts)
2. `_build/default/_utopia/` (dune build output)

Content types are inferred from file extension (`.js`, `.css`, `.json`, `.map`). Path traversal (`..`) is rejected with 400.

## Manifest Wire Format (implemented)

### Route manifest (`_utopia/routes.manifest`)

Tab-separated values, one route per line:

```
<route>\t<kind>\t<source_file>\t<matcher>\t<params>\t<layouts>
```

| Field | Format | Example |
|-------|--------|---------|
| route | Filesystem-style path | `users/[id]` |
| kind | `code` or `markdown` | `code` |
| source_file | Relative path from project root | `pages/users/[id].re` |
| matcher | Server-style pattern | `users/:id` |
| params | Comma-separated `name:kind` pairs | `id:single` |
| layouts | Semicolon-separated source paths | `pages/layout.re;pages/users/layout.re` |

Matcher segment format (differs from filesystem):

| Filesystem | Matcher | Meaning |
|-----------|---------|---------|
| `about` | `about` | Static segment |
| `[id]` | `:id` | Single dynamic param |
| `[...slug]` | `*slug` | Catch-all param |
| `[[...slug]]` | `**slug` | Optional catch-all param |

Param kind values: `single`, `catch_all`, `optional_catch_all`.

### Scripts manifest (`_utopia/scripts.manifest`)

Tab-separated values, one route per line:

```
<route>\t<asset_paths>
```

Asset paths are semicolon-separated: `target/Script__counter.js;target/Script__utils.js`

## Error Catalog

### Compiler errors

**Segment parsing**
- Invalid segment syntax (malformed brackets)
- Invalid parameter name (not a valid OCaml identifier)
- Catch-all/optional catch-all in non-terminal position
- Duplicate parameter names within a route

**Route conflicts**
- Two or more pages produce the same conflict key. Reports competing files, suggests canonical file, recommends naming convention.

**Script errors**
- Missing path after `@utopia.script` directive
- Empty script path
- Path traversal (`..` segments)
- Absolute path (must be relative)
- Script file not found
- Script path points to directory
- Script has non-code extension (must be `.ml`, `.mlx`, `.re`)
- Duplicate `@utopia.script` for same source in one page
- Script module name collision across pages

**Layout errors**
- Two layout files in the same directory

**Param access errors**
- Source code references `params.X` where `X` is not a declared route parameter

**Project structure errors**
- `pages/` directory does not exist

### Server errors

- Route manifest file not found
- Invalid manifest entry (wrong field count, unknown kind, malformed params)
- Invalid `PORT` environment variable (non-integer, falls back to 8080)

### HTTP errors

| Status | Condition |
|--------|-----------|
| 400 | Asset path contains `..` traversal |
| 404 | Asset not found in any asset root |
| 404 | No route matches request path |

### CLI errors

- Missing `pages/` directory (build, dev)
- Compiler failed (build, dev)
- `dune build` failed (build, dev)
- Missing build artifacts (prod)
- Unknown dev flag
- Unknown command
- Server/watch process exited unexpectedly (dev)
- Dune RPC connection failure (warning, non-fatal)

### Exit codes

| Code | Meaning |
|------|---------|
| 0 | Success or clean shutdown via signal |
| 1 | Validation failure, missing prerequisites, unknown command |
| 128 + N | Subprocess killed by signal N |
| Propagated | Forwarded exit code from compiler or dune |

## Testing Strategy

### Test layers

1. **Cram tests** (`bin/tests/`) -- End-to-end CLI and compiler behavior. Create fixture `pages/` directories, run commands, assert output. (implemented, 14 tests)
2. **Cram tests** (`markdown/tests/`) -- Markdown rendering pipeline. (implemented, 2 tests)
3. **Unit tests** -- Core logic: routing, segment parsing, manifest parsing, conflict detection. Using alcotest. (not implemented)
4. **Integration tests** -- HTTP request/response against a running server. (not implemented)

### Coverage rule

Every new feature must include at least one test covering the happy path and one test covering an error case. No feature lands without tests.

### Fixture conventions

Tests create minimal fixture directories (temporary `pages/`, `api/`, etc.) and clean up after themselves. Fixture files should be minimal -- only what's needed to exercise the behavior under test.

## Performance

Performance is a feature. The `bench/` directory contains:

- **Routing micro-benchmarks** (`bench/bench_routing.ml`): `normalize_target`, `target_segments`, `match_segments`, `find_match` (scaling 10-500 routes), `escape_html`, `parse_matcher`, `render_code_page`. (implemented)
- **HTTP benchmarks** (`bench/bench_http.sh`): End-to-end request throughput via `wrk` against all manifest routes plus 404 handling. (implemented)

Performance-sensitive changes should run benchmarks before and after to verify no regressions. No specific targets are set yet -- the current server layer (Dream) is a known bottleneck.

## Code Quality

### Dead code policy

Commented-out code should be removed. Git history preserves it. Currently pending removal:
- `compiler.ml` lines 1-135: commented-out Eio-based implementation
- `Ppx_deriving_router_runtime.ml`: entirely commented out ppx_deriving_router experiment
- `Makefile` pin target for `ppx_deriving_router`

### Generated library naming

The generated `library` stanza in `_utopia/dune` must use a private name (no `public_name`). The current code hardcodes `(public_name utopia)` which causes conflicts. The library is internal to the project build.

## Tech Stack

| Layer | Technology |
|-------|-----------|
| Language | OCaml (>= 5.0.0) + Reason (>= 3.10.0) |
| Build system | Dune 3.8 with melange integration |
| Client JS | Melange (OCaml-to-JS), reason-react |
| JS bundling | esbuild (via Node.js) with `server-reason-react-esbuild-plugin` |
| JS runtime | Node.js + npm (build dependency) |
| Server rendering | server-reason-react (SSR + RSC via DreamRSC) |
| RSC serialization | melange-json + melange-json-native |
| RSC client runtime | `server-reason-react-server-dom-esbuild` (npm) |
| Web server | Dream |
| Markdown | cmarkit (CommonMark) |
| Async | Lwt (>= 5.6.0) |
| Dune RPC | dune-rpc + dune-rpc-lwt (watch mode diagnostics) |
| RPC serialization | csexp (canonical S-expressions) |
| Diagnostics rendering | pp (pretty-printing) |
| Logging | logs + fmt |
| OS interface | unix |
| Testing | dune cram tests, alcotest |

### npm dependencies (not implemented)

Required in `package.json`:
- `react`, `react-dom` -- React runtime
- `esbuild` -- JS bundler
- `server-reason-react-esbuild-plugin` -- esbuild plugin for RSC client component extraction
- `server-reason-react-server-dom-esbuild` -- RSC client runtime (createFromFetch, createServerReference)

## Build Pipeline

### Compilation targets (implemented)

Every code module exists in two compilation contexts:

| Target | Compiler | Suffix | Purpose |
|--------|----------|--------|---------|
| Native | OCaml native | `_native` | Server-side rendering |
| Melange | Melange | `_melange` | Client-side JavaScript |

The compiler generates copy rules that duplicate each page file into `<Name>_melange.<ext>` and `<Name>_native.<ext>` in `_utopia/`.

### PPX configuration (not implemented)

**Native (server) stanza:**
- PPXes: `server-reason-react.ppx`, `server-reason-react.melange_ppx`, `melange-json-native.ppx`
- Libraries: `server-reason-react.react`, `server-reason-react.reactDom`

**Melange (client) stanza:**
- PPXes: `server-reason-react.browser_ppx -js`, `reason-react-ppx`, `server-reason-react.melange_ppx`
- Libraries: `reason-react`, `server-reason-react.react-server-dom-esbuild`

### esbuild integration (not implemented)

esbuild runs as a dune rule (not a separate process). It uses the official `server-reason-react-esbuild-plugin` (Node-based). The plugin runs `server-reason-react.extract_client_components` to scan Melange output for `// extract-client` markers and generates `bootstrap.js`. bootstrap.js populates `window.__client_manifest_map` with `React.lazy(() => import(...))` entries. esbuild code splitting creates separate chunks per client component; each page only loads what it uses.

### Client entry (not implemented)

Utopia generates a shared client shell (`_utopia/client_entry.re`) compiled via Melange. The shell boots React, calls `ReactServerDOMEsbuild.createFromFetch()` to consume the RSC stream, and hydrates into the DOM. Included in all pages via `bootstrapModules` in `DreamRSC.stream_html`.

### Generated dune rules (implemented)

The `_utopia/dune` file currently contains:
1. **Copy rules**: duplicate page sources into melange and native variants
2. **Script copy rules**: copy declared script sources into the build directory
3. **Shared lib rules**: copy `lib/` modules and generate namespace re-export modules
4. **`melange.emit` stanza**: compile all melange modules (pages + scripts + lib) to JS
5. **Markdown rules**: convert `.md` pages to HTML via `utopia.markdown`
6. **`library` stanza**: compile all native modules (pages + lib) as a private dune library

### Generated dune rules -- RSC target (not implemented)

The RSC pipeline changes the generated dune rules to include new PPXes, the esbuild bundling rule, and a per-project server executable:

```scheme
;; --- Copy rules (dual compilation, same pattern as today) ---
(rule
 (deps ../pages/home.re)
 (targets home_melange.re home_native.re)
 (action
  (progn
   (run cp %{deps} home_melange.re)
   (run cp %{deps} home_native.re))))

;; --- Melange stanza (NEW PPXes and libraries) ---
(melange.emit
 (target target)
 (modules home_melange about_melange Lib__counter_melange Lib_melange client_entry_melange)
 (libraries reason-react server-reason-react.react-server-dom-esbuild)
 (preprocess
  (pps server-reason-react.browser_ppx -js
       reason-react-ppx
       server-reason-react.melange_ppx)))

;; --- Native library (NEW PPXes) ---
(library
 (name pages)
 (modules home_native about_native Lib__counter_native Lib_native)
 (libraries server-reason-react.react server-reason-react.reactDom)
 (preprocess
  (pps server-reason-react.ppx
       server-reason-react.melange_ppx
       melange-json-native.ppx)))

;; --- esbuild bundling (NEW) ---
(rule
 (alias esbuild)
 (deps (alias melange) esbuild.config.mjs package.json)
 (action (run node esbuild.config.mjs)))

;; --- Generated server executable (NEW) ---
(executable
 (name server_main)
 (libraries utopia.server_lib pages dream lwt lwt.unix)
 (preprocess
  (pps server-reason-react.ppx
       server-reason-react.melange_ppx
       melange-json-native.ppx)))

;; --- Markdown rules (unchanged) ---
(rule
 (deps ../pages/guide.md)
 (target guide.html)
 (action
  (with-stdout-to %{target}
  (with-stdin-from %{deps}
   (run %{bin:utopia.markdown})))))
```

### Bootstrap requirement (implemented)

The compiler creates `_utopia/` if missing, then removes and regenerates `_utopia/dune`, `_utopia/routes.manifest`, and `_utopia/scripts.manifest`. Projects require `(dirs :standard _utopia)` in their root `dune` file for dune to traverse into the generated directory.

## RSC Architecture

### Build pipeline flow (not implemented)

```
pages/                  User source files (.re, .ml, .mlx, .md)
  layout.re             Root layout (wraps everything, provides <html> structure)
  index.re              Root page (/)
  about.re              /about
  lib/                  Shared code (auto-opened everywhere)
    counter.re          [@react.client.component] - interactive widget
    db.ml               Server-only database access
api/                    API route handlers
  health.ml             /api/health
package.json            npm dependencies (react, esbuild, etc.)
utopia.ml               Project configuration
        |
        v
utopia.compiler         Reads pages/, api/, generates _utopia/
        |
        v
_utopia/
  dune                  Copy rules + melange.emit + native library + esbuild rule + server exe
  routes.manifest       Route -> page mapping
  client_entry.re       Generated RSC client shell
  esbuild.config.mjs    Generated esbuild config with plugin
  server_main.ml        Generated server executable (wires pages to server lib)
        |
        v
dune build              Orchestrates all compilation
        |
        +-> Native: pages -> library (server-reason-react SSR/RSC)
        +-> Melange: pages -> target/*.js (reason-react client)
        +-> esbuild: target/ -> dist/ (bundled, code-split JS + bootstrap.js)
        +-> server_main: links utopia.server_lib + pages -> executable
        |
        v
Server (DreamRSC)
  GET /page                            -> HTML (stream_html, initial load)
  GET /page [Accept: react.component]  -> RSC payload (stream_model_value, navigation)
  POST /page                           -> Server function response (streamFunctionResponse)
  GET /dist/*                          -> Bundled JS assets
```

## Implementation Roadmap

### Phase 1: Compiler changes
1. Update `generate_dune_rules` to emit new PPXes and libraries
2. Remove `@utopia.script` parsing and script manifest generation
3. Generate `client_entry.re` (RSC client shell)
4. Generate `esbuild.config.mjs` (esbuild config with plugin)
5. Generate `server_main.ml` (server executable wiring)
6. Generate esbuild dune rule
7. Generate server executable dune stanza
8. Update `routes.manifest` format if needed for RSC

### Phase 2: Server rewrite
1. Extract server.ml logic into a library (`utopia.server_lib`)
2. Replace HTML string rendering with `DreamRSC.stream_html`
3. Add RSC payload endpoint (check `Accept` header)
4. Add server function POST handler (`DreamRSC.streamFunctionResponse`)
5. Wire route matching to actual React component rendering
6. Asset serving for esbuild `dist/` output
7. Layout nesting via server-reason-react's component tree

### Phase 3: npm / package.json
1. Define expected `package.json` structure
2. Either generate it or validate it exists during `utopia build`
3. Ensure `node_modules/` is available to dune rules

### Phase 4: API routes
1. Compiler scans `api/` directory
2. Generates Dream handlers from API route modules
3. Wires into the generated server executable
4. Middleware support (`_middleware.ml`)

### Phase 5: SSG support
1. Pages with `let static = true` are rendered at build time
2. Output is static HTML served without server rendering
3. Build-time rendering uses the same RSC pipeline

### Phase 6: Markdown RSC integration
1. Markdown pages participate in the layout system
2. Markdown content can include client components (via custom components)
3. Markdown pages are wrapped in the RSC rendering pipeline

### Phase 7: Testing
1. Update existing cram tests for new compiler output
2. Add cram tests for RSC-specific scenarios (client components, server functions)
3. Create RSC demo project (`demo/rsc/`) for end-to-end validation
4. Test dev mode workflow (watch + rebuild + live reload)

### Phase 8: Dev mode
1. Update CLI to manage the per-project server executable
2. Live reload: detect dune rebuild completion, signal browser
3. Verify esbuild dune rule reruns correctly in watch mode
