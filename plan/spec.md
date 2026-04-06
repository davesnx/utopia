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
  lib/                   # Shared code (auto-opened everywhere)
    counter.re           # [@react.client.component] - interactive widget
    db.ml                # Server-only database access
    auth.ml
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
  routes/                # Optional typed query/hash route schemas
    search.re            # schema for /search
    users/
      [id].re            # schema for /users/:id
  api/                   # API route handlers
    _middleware.ml        # Global API middleware
    health.ml            # /api/health
    users/
      _middleware.ml      # Middleware for /api/users/*
      [id].ml            # /api/users/:id
  _utopia/               # Generated (do not edit)
    dune                 # Generated dune graph for melange/native/esbuild/server/ssg
    paths.mjs            # Generated build metadata for esbuild
    Routes.ml            # Generated typed route tree + route metadata loaders
    client_entry.re      # Generated browser RSC entry
    esbuild.config.mjs   # Copied esbuild runtime config
    server_main.ml       # Generated per-project server executable wiring
    native/              # Native-only generated support and mirrored sources
      FunctionReferences.re
      Utopia_route_builder.ml
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
| `.md` | Markdown page | Native markdown pipeline |

### Page contract

A page module must export:

```ocaml
val make : unit -> React.element
```

### Pages are server component

Every page is a server component. It renders on the server and can perform async data fetching inline -- the component IS the loader.

To create a fully interactive page, the page's `make` function can eventually contain a client component (a module with `[@react.client.component]`)

```reason
/* pages/dashboard.re -- server component */
[@react.component]
let make = () => {
  <Dashboard_client initial_data={fetch_data()} />
};
```

Where `Dashboard_client` is annotated with `[@react.client.component]`.

## Client Components

A module becomes a client component by adding the `[@react.client.component]` attribute. The compiler detects this and routes the module through melange for JavaScript compilation.

```reason
/* components/counter.re */
[@react.client.component]
let make = (~initial_count) => {
  let (count, set_count) = React.useState(() => initial_count);
  <button onClick={_ => set_count(c => c + 1)}>
    {React.string(string_of_int(count))}
  </button>
};
```

Note: the compiler currently does not inspect module attributes. All code pages are blindly copied to both melange and native targets.

### Client component compilation flow

Works because we use server-reason-react-ppx, so when a module has `[@react.client.component]`:

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

### RSC boundary

Props crossing the server-to-client boundary must be JSON-serializable. Serialization is handled by `melange-json` (client) and `melange-json-native` (server), which must be installed as project dependencies. The framework ensures both libraries are available as ppxes and libraries for all compilation targets.

### Server Functions (implemented)

A function annotated with `[@react.server.function]` executes on the server but can be called from client components. The PPX generates a unique ID and registers the function in a server-side registry. On the client side, a proxy is created via `ReactServerDOMEsbuild.createServerReference`. Server functions enable progressive enhancement: forms work without JavaScript (POST to the same page), and with JavaScript, the client calls the server function directly and receives an RSC response.

Because page modules compile through both native and Melange build paths, page-level form actions use SRR's explicit action encoding rather than passing a bare function value:

```reason
let submit_action =
  switch%platform () {
  | Server => `Function(save_note)
  | Client => ""
  };
```

Client-side direct calls use the generated `Utopia_call_server.callServer` transport plus `ReactServerDOMEsbuild.encodeReply` to choose the POST body format. Plain argument lists travel as encoded request bodies, while `Js.FormData.t` arguments travel as multipart form-data so the server can decode them through `ReactServerDOM.decodeFormDataReply`.

On the server, Utopia resolves the action ID from `X-Action-ID` (and accepts legacy `ACTION_ID` for compatibility), looks up the callback in generated `_utopia/native/FunctionReferences.re`, and streams `application/react.action` payloads with `ReactServerDOM.create_action_response`.

When a server function returns `Utopia.Route.t`, the action payload serializes that route as a typed object carrying `pathname`, `request_path`, and `href`, so client components can pass the returned value straight into `Utopia.useRouter().navigate(...)`.

## Layouts (partial)

### Discovery (implemented)

A `layout.re`, `layout.ml` or `layout.mlx` file in any `pages/` subdirectory defines a layout for that directory and all descendants.

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

### Typed route construction (implemented)

The generated pages library exposes two route-related modules:

- `Utopia.Route` -- the opaque route value API (`href`, `pathname`, `request_path`, equality helpers, and a `Nonempty` helper for catch-all builders)
- `Utopia.Routes` -- a generated module tree that mirrors collected route paths and constructs `Utopia.Route.t` values
- `Utopia.Routes.of_route` -- a generated parser plus `Utopia.Routes.t` sum type for decoding a `Utopia.Route.t` back into the matching route constructor with typed params/query/hash payloads

Examples:

```ocaml
Utopia.Routes.Notes.route
Utopia.Routes.Notes.New.route
Utopia.Routes.Users.Param_id.make ~id:"42" ()
Utopia.Routes.Docs.Optional_catch_all_slug.make ~slug:["api"; "intro"] ()
```

`Utopia.Router.Link` and `Utopia.useRouter().navigate(...)` accept `Utopia.Route.t`, not raw strings.

When code needs to inspect the current route in a typed way, it uses `Utopia.Routes.of_route(route)`:

```ocaml
match Utopia.Routes.current route with
| Some (Utopia.Routes.Search { query = Some query; hash = Some hash }) -> ...
| _ -> ...
```

### Route schema modules (implemented)

Projects may optionally define mirrored files under `routes/` to add typed query/hash support for a collected route. A route schema file matches the normalized route path, not necessarily the page source filename casing. For example, `pages/Search.re` maps to `/search`, so its route schema lives at `routes/search.re`.

Supported optional nested modules inside a route schema file:

```reason
module Params = {
  type t = {id: int};
  let encode = value => [("id", Utopia_route.Params.one(string_of_int(value.id)))];
  let decode = values => ...;
};

module Query = {
  type t = {q: string};
  let encode = value => [("q", value.q)];
  let decode = entries => ...;
};

module Hash = {
  type t = Details | Overview;
  let encode = value =>
    switch (value) {
    | Details => "details"
    | Overview => "overview"
    };
  let decode = value => ...;
};
```

When present, the generated route builder exposes those types via `Route_params`, `Route_query`, and `Route_hash` nested modules. `Route_params` switches path construction from compiler-derived string args to `~params:Route_params.t`, while `Route_query` / `Route_hash` add optional `?query` / `?hash` arguments. Each declared schema module must provide both `encode` and `decode`: `encode` is required because the generated builders call it when constructing typed routes, and `decode` is required so the generated current-route matcher can reconstruct typed params/query/hash values from `Utopia.Route.t`.

Route schema files should use `Utopia_route.Params` for path-param helpers rather than `Utopia.Route.Params`, because `Utopia` itself depends on the generated `Utopia.Routes` module and would otherwise create a compile-time module cycle.

## API Routes (implemented)

### Location

API routes live in the `api/` directory. The compiler scans `api/` using the same recursive traversal and segment parsing as `pages/`.

The `/api/*` namespace is reserved for API endpoints. Any page route that normalizes to `/api/*` is a compile-time error.

### Routing conventions

Identical to page routing: `[param]`, `[...slug]`, `[[...slug]]`, route groups, and parallel slots.

Supported API source extensions: `.ml`, `.re`, `.mlx`.

### Handler contract

An API route exports a single handler function:

```ocaml
val handler : Dream.request -> Dream.response Lwt.t
```

The handler receives the raw Dream request. HTTP method dispatch is handled by user code.

### API middleware

A file named `_middleware.ml` (or `.re` / `.mlx`) in any `api/` subdirectory applies to all routes in that directory and descendants. Middleware composition follows physical directory ancestry, outermost first.

Middleware contract:

```ocaml
val middleware : Dream.handler -> Dream.handler
```

### Generated API route metadata and params helpers

Generated native route metadata is exposed under `Routes.Api.get_all ()`.

Generated typed param accessors are exposed under `Routes.Api.Params` and read matched params from request-local storage. Shapes are:

- single -> `string`
- catch-all -> `string list`
- optional catch-all -> `string list` (`[]` means absent)

### API response policy

API handlers are expected to return JSON responses, typically through a helper like `Utopia.respond(~status, ~headers, json)`.

Framework-generated API errors are always JSON with exact keys `error`, `code`, and `path`:

- 404: `{ "error": "API route not found", "code": "api_not_found", "path": "..." }`
- 500: `{ "error": "Internal API error", "code": "api_internal_error", "path": "..." }`

## Rendering Models

### RSC (Primary) (implemented)

React Server Components are the default. Every page and layout is a server component. Components render on the server, can perform async operations inline, and stream output to the client. Powered by `server-reason-react`.

Server rendering uses Dream as the HTTP layer and `ReactServerDOM` as the streaming renderer:

**Initial page load** (GET without RSC header):
```
Browser -> GET /about -> Server renders route shell -> ReactServerDOM.render_html
  -> HTML stream with bootstrap modules only when `/dist/client_entry_melange.js` exists
  -> Browser receives HTML, boots generated client entry, hydrates `document`
```

**Client-side navigation** (GET with `Accept: application/react.component`):
```
Client JS -> fetch("/about", {headers: {"Accept": "application/react.component", "X-Utopia-Current-Path": currentPath}})
  -> Server renders either a full route tree or a parent-relative diff tree -> ReactServerDOM.render_model_value
  -> RSC payload (`["full", "", tree]` or `["diff", parentRoute, subtree]`)
  -> Client reads stream via createFromFetch, updates the whole page or only the nested branch without a full page reload
```

**Server function** (POST):
```
Client JS -> POST /about (with action ID + args)
  -> Server decodes args, resolves generated FunctionReferences, streams `application/react.action`
  -> Executes server function, returns response
```

The generated pages library now also exposes a public `Utopia.useRouter()` hook, an opaque `Utopia.Route.t`, a generated `Utopia.Routes` module tree with `type t` plus `of_route`, and a `Utopia.Router.Link` client link component for user code. `Utopia.useRouter()` returns the current request path and the raw `Utopia.Route.t`; typed route inspection happens explicitly through `Utopia.Routes.of_route router.route`. Utopia's generated client shell intercepts same-origin `.js-route-link` anchors for SPA navigation and uses the hook for programmatic navigation.

The generated runtime surface is now split between a real shared library and project-specific generated routes. The shared public `utopia` library owns the reusable runtime modules (`Utopia`, `Utopia_call_server`, `Utopia_route`, `Utopia_router`, `Utopia_router_link`, `Utopia_router_route`, `Utopia_route_builder`, `Utopia_server`, `Utopia_types`, and `FunctionReferences`), while generated projects only emit `_utopia/Routes.ml` plus their page/lib mirrors and runtime entry files.

### SSR (partial)

Available via `server-reason-react.reactDom` APIs alongside RSC. Server-side rendering happens on every request. This is the default behavior for pages that don't opt into SSG.

### SSG (Opt-in) (implemented)

A page opts into static generation by exporting:

```ocaml
let static = true
```

Dynamic static pages must also export:

```ocaml
val static_paths : unit -> (string * string) list list
```

Static output is produced by running the generated `server_main.exe --ssg` executable or the generated dune alias `@_utopia/ssg`. The SSG pass renders HTML into `_utopia/static/` and copies bootstrap/stylesheet assets when present.

Normal server mode still renders dynamically. `let static = true` does not switch request handling over to serving `_utopia/static/` automatically.

### Remote_data (Draft -- not implemented)

An aspirational module for client-side data fetching, similar to SWR or react-query. Would provide:
- Loading/error/success state management
- Request caching and deduplication
- Stale-while-revalidate semantics
- Integration with RSC for initial data hydration

This is a concept only. No design or implementation exists yet.

### Client-side code (RSC pipeline)

Client components are marked with `[@react.client.component]`. The PPX handles server/client separation. esbuild bundles client components with code splitting. The RSC protocol handles hydration. Server functions (`[@react.server.function]`) enable server-side logic callable from client components, including returning typed route values for post-action navigation.

## Shared Code (`lib/`) (implemented)

The project-root `lib/` directory contains shared modules available everywhere:
- Pages (code and markdown custom components)
- API routes
- Layouts

The compiler mirrors shared `lib/` files into generated build contexts under internal `Utopia_lib__*` module names, emits a public `Lib` alias module that re-exports them, and auto-opens that alias in generated page/layout mirrors.

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

The compiler parses frontmatter, strips it before markdown rendering, and records metadata in generated route metadata.

### Rendering (implemented)

The `utopia.markdown` executable converts markdown to server-rendered React HTML using `server-reason-react`. Every HTML element is rendered through a customizable component function (defined in `Components.t`).

The generated server runtime reuses the same shared markdown runtime when it needs HTML output from markdown pages; there is no second independent markdown-to-HTML implementation in the generated server path.

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
| `utopia clean` | -- | Remove `_build/`, `_utopia/`, project `target/.../_utopia`, run `dune clean` |
| `utopia info` | -- | Print versions, paths, route count, command status |

### `build` flow (implemented)

1. Validate project shape (`pages/` must exist)
2. Run `utopia.compiler --mode production` (generate route modules/metadata + dune rules + build metadata)
3. Run `dune build --root <workspace> --no-print-directory .`
4. Emit build report (route count, generated files, output dirs)
5. Fail fast on route conflicts, invalid segments, undeclared params, and missing `static_paths`

Note: current CLI build does not yet fail fast on missing npm packages or explicitly build the generated `@_utopia/esbuild` alias.

### `dev` flow (implemented)

1. Validate project shape
2. Run `utopia.compiler --mode development`
3. Run an initial `dune build --root <workspace> --no-print-directory .`
4. Start `dune build -w --root <workspace> --no-print-directory .` unless `--no-watch`
5. Start the generated per-project server executable at `_build/default/_utopia/server_main.exe` for root projects, or `_build/default/<project-path>/_utopia/server_main.exe` for nested projects
6. Connect to dune RPC, subscribe to progress and diagnostics, and stream those events to the terminal
7. Restart the generated server whenever the built `server_main.exe` mtime changes
8. Handle SIGINT/SIGTERM for clean teardown

Current dev mode does not yet implement browser reload, SSE dev events, or the in-browser build/runtime overlay described in `tasks/dev-full-reload-and-browser-overlay.md`.

### Dune RPC In `dev` (implemented)

`utopia dev` opens a separate client connection to dune's RPC socket after watch mode starts:

1. Wait for the RPC socket under the workspace `_build` directory
2. Initialize a dune RPC client session
3. Subscribe to `progress` and `diagnostic` streams
4. Maintain an in-memory table of active diagnostics keyed by dune diagnostic ID
5. Print build status transitions (`waiting`, `in progress`, `failed`, `done`) and dump the active diagnostics on failed builds
6. Treat RPC connection/subscription failures as warnings rather than fatal errors

If the requested port is already in use, `utopia dev` currently selects the next available port before each server start/restart instead of pinning the original origin for the full session.

### `clean` flags (implemented)

- `--build-outputs`: remove only transient project outputs (`_utopia/dist`, `_utopia/static`, `target/<project>/_utopia`) without deleting `_utopia/*` scaffolding or running `dune clean`

### `dev` flags (implemented)

| Flag | Default | Description |
|------|---------|-------------|
| `--port` | `$PORT` or `8080` | Server port |
| `--host` | `$HOST` or `127.0.0.1` | Server host |
| `--no-watch` | `false` | Disable dune watch (no RPC) |
| `--verbose` | `false` | Show request logs and RPC debug output |

### `prod` flow (implemented)

1. Verify `_utopia/dune`, generated route modules, and the built generated `server_main.exe` exist
2. Resolve `PORT` and `HOST` from environment
3. Start the generated per-project server executable at `_build/default/_utopia/server_main.exe` for root projects, or `_build/default/<project-path>/_utopia/server_main.exe` for nested projects
4. Forward the child exit code

### Environment variables (implemented)

| Variable | Used by | Description |
|----------|---------|-------------|
| `PORT` | server, CLI | Server listen port (default: 8080) |
| `HOST` | server, CLI | Server listen host (default: 127.0.0.1 dev, 0.0.0.0 prod) |
| `NO_LOG` | server | When set, disables Dream request logging. `dev` sets this by default unless `--verbose`. |

Both the CLI and the shared server runtime treat `PORT` as a preferred starting port and retry on higher ports when bind fails with `EADDRINUSE`.

### Executable aliasing (implemented)

The CLI supports executable aliases: `utopia-build` is equivalent to `utopia build`. The binary inspects `argv[0]` and extracts the subcommand from the `utopia-` prefix.

## Server

### Architecture (implemented)

Framework server logic lives in `lib/utopia/Utopia_server.ml`, and generated projects link against it through the shared `utopia` library. The compiler also generates a per-project executable in `_utopia/server_main.ml` that:
- Depends on the copied `Utopia_server` support module (framework: routing, RSC rendering, asset serving, SSG)
- Depends on private, project-scoped generated native libraries for pages and APIs
- Wires generated route metadata loaders plus compiled module registries and starts the Dream server

This separation means the framework server logic is reusable and the user's page code is linked in at build time.

### Request handling (implemented)

1. Parse request target into URL segments
2. If the target points at generated assets (`target/`, `dist/`, or known direct assets such as `output.css`), serve it from source `_utopia/` or built `_build/default/.../_utopia/`
3. If the target is `/api/*`, match API routes first
4. For matched API routes, run middleware chain and handler; framework-generated API errors return JSON envelopes
5. If the request is `POST` and not handled by API routing, resolve the server function from generated `FunctionReferences` and stream an `application/react.action` response
6. Match URL segments against generated page routes ordered by specificity
7. For ordinary `GET`, render the compiled route shell/document and stream HTML with `ReactServerDOM.render_html`
8. For `GET` with `Accept: application/react.component`, render either a full router tree or a parent-relative diff tree and stream it with `ReactServerDOM.render_model_value`
9. If no page route matches and the request is `/`, render the dev route index page when no index page exists
10. Return `404` for unmatched page routes

**Target request handling (RSC)**:
- **GET** (no RSC header): stream HTML with `ReactServerDOM.render_html`
- **GET** with `Accept: application/react.component`: stream either a full-tree or diff payload with `ReactServerDOM.render_model_value`
- **/api/**: route to API middleware/handler chain before server-action and page routing
- **POST** (non-API): decode action arguments, resolve `FunctionReferences`, and stream `application/react.action`
- **GET /dist/\***: serve bundled JS assets from esbuild output

Generated runtime renders compiled native page/layout/API modules through `_utopia/server_main.exe`.

### Caching (implemented)

The server uses an mtime-based page cache. Each cache entry stores `(mtime, rendered_element)`. On request, a `stat()` call checks whether the source file changed. If mtime matches, the cached React element is reused. Cache keys combine `source_file + route + params` so the same file can be cached separately for different param values.

If a source file disappears between requests, the server renders without caching (graceful degradation).

### Asset serving (implemented)

Static assets are resolved from generated-project roots first, then build-output roots. When running through a generated executable, lookup prefers the source project's `_utopia/` artifacts when available and falls back to `_build/default/.../_utopia/` copies and build-root outputs such as `output.css`.

Content types are inferred from file extension (`.js`, `.css`, `.json`, `.map`, `.wasm`, `.svg`, `.png`, `.ico`, `.woff`, `.woff2`). Path traversal (`..`) is rejected with 400.

## Generated Route Metadata (implemented)

Runtime route loading is module-based, not TSV-manifest-based.

- `Routes.get_all ()` returns page route metadata.
- `Routes.Api.get_all ()` returns API route metadata.
- Native module registries resolve metadata entries to compiled page/layout/API modules.

Matcher segment format used in generated metadata:

| Filesystem | Matcher | Meaning |
|-----------|---------|---------|
| `about` | `about` | Static segment |
| `[id]` | `:id` | Single dynamic param |
| `[...slug]` | `*slug` | Catch-all param |
| `[[...slug]]` | `**slug` | Optional catch-all param |

Param kind values remain `single`, `catch_all`, `optional_catch_all`.

## Error Catalog

### Compiler errors

**Segment parsing**
- Invalid segment syntax (malformed brackets)
- Invalid parameter name (not a valid OCaml identifier)
- Catch-all/optional catch-all in non-terminal position
- Duplicate parameter names within a route

**Route conflicts**
- Two or more pages produce the same conflict key. Reports competing files, suggests canonical file, recommends naming convention.

**Static generation errors**
- A dynamic page exports `let static = true` but does not also export `static_paths`

**Layout errors**
- Two layout files in the same directory

**Param access errors**
- Source code references `params.X` where `X` is not a declared route parameter

**Project structure errors**
- `pages/` directory does not exist
- Page route normalizes under reserved `/api/*` namespace

**API compilation errors**
- Duplicate API route conflict key
- Multiple `_middleware` files in the same `api/` directory

### Server errors

- Invalid generated route metadata entry
- Invalid `PORT` environment variable (non-integer, falls back to 8080)

### HTTP errors

| Status | Condition |
|--------|-----------|
| 400 | Asset path contains `..` traversal |
| 404 | Asset not found in any asset root |
| 404 | No page route matches request path |
| 404 | API route not found (`/api/*`, JSON envelope) |
| 500 | Unhandled API exception (JSON envelope) |

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

1. **Cram tests** (`bin/tests/`) -- End-to-end CLI and compiler behavior. Create fixture `pages/` directories, run commands, assert output. (implemented)
2. **Cram tests** (`markdown/tests/`) -- Markdown rendering pipeline. (implemented)
3. **Unit tests** -- Core logic: routing, segment parsing, generated route metadata loading, conflict detection. Using alcotest. (not implemented)
4. **Integration tests** -- HTTP request/response against a running server. (not implemented)

### Coverage rule

Every new feature must include at least one test covering the happy path and one test covering an error case. No feature lands without tests.

### Fixture conventions

Tests create minimal fixture directories (temporary `pages/`, `api/`, etc.) and clean up after themselves. Fixture files should be minimal -- only what's needed to exercise the behavior under test.

## Performance

Performance is a feature. The `bench/` directory contains:

- **Routing micro-benchmarks** (`bench/bench_routing.ml`): `normalize_target`, `target_segments`, `match_segments`, `find_match` (scaling 10-500 routes), `escape_html`, `parse_matcher`, `render_code_page`. (implemented)
- **HTTP benchmarks** (`bench/bench_http.sh`): End-to-end request throughput via `wrk` against all generated routes plus 404 handling. (implemented)

Performance-sensitive changes should run benchmarks before and after to verify no regressions. No specific targets are set yet -- the current server layer (Dream) is a known bottleneck.

## Code Quality

### Dead code policy

Commented-out code should be removed. Git history preserves it. Currently pending removal:
- `compiler.ml` lines 1-135: commented-out Eio-based implementation
- `Ppx_deriving_router_runtime.ml`: entirely commented out ppx_deriving_router experiment
- `Makefile` pin target for `ppx_deriving_router`

### Generated library naming

The generated native pages library in `_utopia/dune` uses a private, project-scoped name such as `pages_demo_notes`. No `public_name` is emitted, so multiple generated Utopia projects can coexist in the same Dune workspace without library collisions.

## Tech Stack

| Layer | Technology |
|-------|-----------|
| Language | OCaml (>= 5.0.0) + Reason (>= 3.10.0) |
| Build system | Dune 3.8 with melange integration |
| Client JS | Melange (OCaml-to-JS), reason-react |
| JS bundling | esbuild (via Node.js) with `server-reason-react-esbuild-plugin` |
| JS runtime | Node.js + npm (build dependency) |
| Server rendering | server-reason-react (`ReactServerDOM` streamed through Dream) |
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

### npm dependencies (partial)

Required in `package.json`:
- `react`, `react-dom` -- React runtime
- `esbuild` -- JS bundler
- `server-reason-react-esbuild-plugin` -- esbuild plugin for RSC client component extraction
- `server-reason-react-server-dom-esbuild` -- RSC client runtime (createFromFetch, createServerReference)

Current CLI flows rely on these packages being present but do not yet perform fail-fast resolution checks before `utopia build` / `utopia dev`.

## Build Pipeline

### Compilation contexts (implemented)

Each generated project builds through three related outputs:

| Output | Generated location | Naming | Purpose |
|--------|--------------------|--------|---------|
| Melange sources | `_utopia/` | `Utopia_page__*`, `Utopia_lib__*`, `Lib__*`, runtime support | Compile browser-facing JS into `target/<project>/_utopia/...` |
| Native sources | `_utopia/native/` | same mirrored modules plus native-only support | Build private generated pages and API libraries |
| Bundled browser assets | `_utopia/dist/` | esbuild output | Ship `client_entry_melange.js`, `bootstrap.js`, and code-split chunks |

Markdown pages also get per-page `.html` build rules so the markdown pipeline can participate in Dune's dependency graph.

### How Dune Generation Works (implemented)

`utopia.compiler` generates `_utopia/dune` in four stages:

1. Ensure `_utopia/` and `_utopia/native/` exist
2. Copy static generated-project asset files from `lib/utopia/` into those directories
3. Scan `pages/`, `api/`, `lib/`, and optional `routes/` schemas; compute route entries, API entries, metadata/static flags, layouts/middleware, and diagnostics
4. Build structured dune stanzas with the internal `Dune_sexp` library and write `_utopia/dune`, `_utopia/paths.mjs`, `_utopia/Routes.ml`, and `_utopia/server_main.ml`

The compiler no longer hand-concatenates dune source strings. `bin/compiler/Generated_dune.ml` constructs typed sexps and serializes them into the final file.

For editor ownership of source files, the compiler now emits a single generated `_utopia/dune` file that serves both the runtime build and the optional source-ownership path. Projects opt into that file from the root `dune` file with `(include _utopia/dune)` and should mark `_utopia` as data-only with `(data_only_dirs _utopia)` so Dune does not parse `_utopia/dune` as a separate nested project.

That single `_utopia/dune` file provides:
- a generated `_utopia/support/` native library that copies the project-local `Utopia`/route/runtime surface from `_utopia/`, includes `FunctionReferences`, and stubs `Utopia_call_server`
- a real source-owned native library for `lib/*.ml|*.re|*.mlx` files
- source-owned native page libraries grouped by directory for page/layout files whose basenames are valid module names
- the mirrored runtime/native/melange build stanzas for `_utopia/` itself, wrapped in `(subdir _utopia ...)`

This improves LSP behavior for shared `.ml` modules such as `demo/notes/lib/notes_data.ml` and for source pages whose basenames are valid modules, including dynamic-directory routes like `demo/notes/pages/notes/[tag]/index.mlx` and `demo/blog/pages/posts/[slug]/index.mlx`. Dynamic route segments should therefore live in directory names with an `index` page rather than in invalid basenames like `pages/notes/[tag].mlx`.

The workspace `mlx` dialect also now uses `(merlin_reader mlx)` in `dune-project`, which requires `lang dune >= 3.16`. That removes one configuration mismatch relative to known-good `mlx` projects such as `html_of_jsx`.

### Generated Dune Structure (implemented)

The generated `_utopia/dune` file contains:

1. Root copy rules for page/layout mirrors under `Utopia_page__*`
2. Root copy rules for shared `lib/` mirrors under `Utopia_lib__*` plus wrapped `Lib__*` modules
3. A generated `Lib.re` alias file that re-exports shared modules by their original names
4. Copy rules for optional route-schema modules
5. A root `melange.emit` stanza that compiles the mirrored user modules plus generated runtime support (`Utopia.re`, `Utopia_call_server.re`, `Utopia_router.re`, `client_entry_melange.re`, etc.)
6. Markdown build rules that turn `pages/*.md` into `.html` via `utopia.markdown`
7. A `subdir native` block containing native mirrors and private project-scoped pages/API libraries
8. An `esbuild` alias rule that runs `node _utopia/esbuild.config.mjs` from the project root
9. A generated `server_main` executable stanza
10. An `ssg` alias that runs `./server_main.exe --ssg`

### PPX And Library Configuration (implemented)

**Melange stanza**
- Libraries: `reason-react`, `melange-webapi`, `melange-fetch`, `server-reason-react.runtime`, `server-reason-react.url_js`, `melange-json`
- PPX stack: `server-reason-react.browser_ppx -js`, `server-reason-react.ppx -melange`, `melange.ppx`, `reason-react-ppx`, `melange-json.ppx`
- Shared-folder prefixes are emitted per-module: source-relative mirrors use `../`, generated runtime sources use `_utopia/`

**Native pages library**
- Libraries: `utopia.markdown_runtime`, `server-reason-react.runtime`, `server-reason-react.react`, `server-reason-react.reactDom`, `server-reason-react.fetch`, `server-reason-react.url_native`, `server-reason-react.webapi`, `melange-json`
- PPX stack: `server-reason-react.ppx`, `server-reason-react.browser_ppx`, `server-reason-react.melange_ppx`, `melange-json-native.ppx`
- Shared-folder prefixes are emitted per-module: source-relative mirrors use `../../`, generated native support uses `_utopia/native/`

**Native API library**
- Separate project-scoped native API library for `api/` handlers and middleware modules
- Uses the same native PPX stack and shared `Lib` ergonomics as server-side page modules

### esbuild integration (partial)

esbuild is integrated as a generated dune alias rather than a long-running sidecar process. The rule depends on `(alias melange)`, `esbuild.config.mjs`, `paths.mjs`, and `package.json`, then executes `node _utopia/esbuild.config.mjs` from the project root.

The generated config imports `_utopia/paths.mjs`, derives all source/output paths from `projectPath`, sets `process.env.NODE_ENV`, enables minification when `buildMode = "production"`, and uses `server-reason-react-esbuild-plugin` to generate `bootstrap.js` from Melange's extracted client-component markers.

Current caveat: the generated alias exists and works, but `utopia build` / `utopia dev` do not yet explicitly request `@_utopia/esbuild`.

### Client entry (implemented)

Utopia generates a shared client shell at `_utopia/client_entry.re`, compiles it through Melange as `client_entry_melange.re`, and bundles the resulting JS with esbuild. The entry reads the initial RSC stream from `window.srr_stream`, calls `ReactServerDOMEsbuild.createFromReadableStream`, and hydrates the full browser `document` rather than a nested `#root` element.

The browser-side server-action transport lives in the separate generated `Utopia_call_server.re` support module so the initial client entry does not need to import the full router runtime just to invoke actions.

### Bootstrap requirement (implemented)

The compiler creates `_utopia/` if missing, copies static support files into `_utopia/` and `_utopia/native/`, then rewrites the dynamic generated files on every compiler run. Projects should include the generated rules with `(include _utopia/dune)` and mark `_utopia` as data-only with `(data_only_dirs _utopia)` so Dune does not parse `_utopia/dune` as a nested standalone project.

### End-to-end build graph (implemented)

```
pages/ + api/ + lib/ + routes/ schemas
        |
        v
utopia.compiler
  - copies runtime support into _utopia/ and _utopia/native/
  - writes dune + paths.mjs + Routes.ml + server_main.ml
        |
        v
dune build
  - melange.emit -> target/<project>/_utopia/*.js
  - native libraries -> pages_<project-scope> + api_<project-scope>
  - executable -> _build/default/<project>/_utopia/server_main.exe
  - alias esbuild -> _utopia/dist/*.js + bootstrap.js
  - alias ssg -> _utopia/static/**
        |
        v
Runtime
  /api/* -> API middleware + handler (JSON errors for framework-generated 404/500)
  GET -> `ReactServerDOM.render_html`
  GET Accept: application/react.component -> `ReactServerDOM.render_model_value`
  POST (non-API) -> `ReactServerDOM.create_action_response`
```
