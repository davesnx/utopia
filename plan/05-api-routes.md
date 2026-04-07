# API routes

> Note: filesystem-root assumptions in this phase (`pages/` + `api/`) are superseded by `plan/14-app-directory-unification.md`. Keep this document as historical implementation context for API runtime behavior and metadata generation.

Add file-based API routing and, in the same phase, refactor page route loading away from manifest files to generated module registries.

---

## Goal

Files in `api/` map to API endpoints using the same segment conventions as pages (`[param]`, `[...slug]`, `[[...slug]]`, route groups, parallel slots).

This phase also replaces manifest-driven runtime loading with generated module-driven loading:

- Page metadata comes from `Routes.get_all ()`
- API metadata comes from `Routes.Api.get_all ()`
- Server wiring resolves compiled modules via generated native registries
- `_utopia/routes.manifest` and `_utopia/api.manifest` are removed

---

## Locked decisions

- Canonical runtime is generated `_utopia/server_main.exe`; standalone `utopia.server` is removed.
- Request order is: assets -> API -> server actions -> pages.
- `/api/*` is reserved for API routes; any `pages/**` route normalizing to `/api/*` is a compile-time error.
- API handlers keep raw Dream contract: `Dream.request -> Dream.response Lwt.t`.
- API middleware contract is `Dream.handler -> Dream.handler`.
- API responses are JSON-by-convention with best-effort enforcement:
  - `Utopia.respond(~status, ~headers, json)` helper is provided.
  - Framework-generated API errors are always JSON and always include exactly `error`, `code`, `path`.
- API handler source extensions are `.ml`, `.re`, `.mlx`.
- Middleware inheritance uses physical directory ancestry, outermost first.
- API params are exposed through generated typed key accessors under `Routes.Api.Params`.

---

## Dependencies

- `plan/01-shared-types.md` -- shared route segment and param kinds
- `plan/02-compiler-rsc.md` -- compiler and generated artifacts pipeline
- `plan/03-server-rewrite.md` -- server library request routing
- `plan/04-client-components.md` -- keep page/client route APIs compatible while splitting route modules

---

## Route loading refactor (pages + APIs)

### Remove manifest-based runtime loading

- Stop generating `_utopia/routes.manifest`.
- Do not add `_utopia/api.manifest`.
- Remove server runtime startup paths that parse route manifests.

### Generate route registries instead

Route loading is module-driven:

1. `Routes.get_all ()` returns page route metadata (no Dream dependency).
2. `Routes.Api.get_all ()` returns API route metadata (no Dream dependency).
3. Native-only generated registries map metadata entries to compiled page/layout/api modules.
4. `server_main.ml` joins metadata + registries and starts the server.

---

## Split `Routes` generation: shared base + native extension

`Routes` must compile in both Melange and native builds. API request accessors depend on `Dream.request`, so the generator must split outputs:

- Shared base: route constructors/parsers and metadata-safe types usable by both Melange and native.
- Native extension: server-only additions including:
  - `Routes.get_all : unit -> page_route_meta list`
  - `Routes.Api.get_all : unit -> api_route_meta list`
  - `Routes.Api.Params` typed accessors backed by request-local matched params

The final public API path for server-side code is:

- `Routes.get_all ()`
- `Routes.Api.get_all ()`
- `Routes.Api.Params.<...>`

---

## Extend compiler scan to `api/`

Add an API scanning pass in compiler modules (same traversal model as pages):

- Read `api/` recursively via existing filesystem helpers.
- `api/` is optional; missing directory is not an error.
- Reuse segment parsing and normalization (`parse_param_segment`, `normalize_path_segments`).
- Reuse conflict-key detection rules inside the API namespace.
- API and page conflicts are separate except for reserved prefix rule (`/api/*` owned by API).

---

## Reserved `/api` namespace rule

During page route collection, if a page route normalizes to `api/...`, emit a compile-time error.

Examples that must fail:

- `pages/api/users.re`
- `pages/(x)/api/index.mlx` (if normalized visible path starts with `api`)

Reason: `/api/*` is exclusively served by API routing.

---

## API route and middleware discovery

### API route files

- Supported: `.ml`, `.re`, `.mlx`
- Unsupported/ignored for handlers: `.md` and unknown extensions
- `api/index.*` maps to `/api`

### Middleware files

- Middleware basename is `_middleware`.
- Supported middleware extensions are `.ml`, `.re`, `.mlx`.
- Middleware files are excluded from endpoint generation.
- Middleware applies to descendant API routes by physical directory ancestry.
- Order in composed chain: outermost directory first.
- If multiple `_middleware` files exist in the same directory across extensions, emit a compile-time conflict error.

---

## API metadata and typed param access

### API metadata record

Add server-usable API metadata type(s) to shared compiler/server interfaces (exact module placement can follow existing architecture):

```ocaml
type api_route_meta = {
  route : string;
  matcher : string;
  conflict_key : string;
  params : (string * param_kind) list;
  middlewares : string list;
  source_file : string;
  module_name : string;
}
```

### Typed key accessors

Generate typed key accessors under `Routes.Api.Params` that read matched params from request-local storage.

Value shapes:

- `Single` -> `string`
- `Catch_all` -> `string list`
- `Optional_catch_all` -> `string list` (`[]` means absent)

For required params (`Single`, `Catch_all`), missing values raise an internal error (indicates matcher/registration bug).

This replaces API-side `params.X` source scanning.

---

## API handler contract and JSON helper

In server/runtime library surface:

```ocaml
module type Api_handler = sig
  val handler : Dream.request -> Dream.response Lwt.t
end

module type Api_middleware = sig
  val middleware : Dream.handler -> Dream.handler
end
```

Provide a helper for JSON responses:

```ocaml
val respond :
  ?status:Dream.status ->
  ?headers:(string * string) list ->
  string ->
  Dream.response Lwt.t
```

`respond` sets JSON content-type and merges custom headers.

---

## Server request routing changes

Update server request dispatcher to:

1. Serve assets (`dist/`, `target/`, direct known assets)
2. If target is `/api/*`, route through API table
3. Else if method is `POST`, run server-action path
4. Else route through pages

For API path handling:

- Match route by API matcher rules
- Store matched params in request-local field for accessor reads
- Wrap handler by middleware chain (outermost first)
- Catch unhandled exceptions and return JSON 500 envelope
- On missing API route return JSON 404 envelope

JSON envelopes (exact keys only):

```json
{"error":"API route not found","code":"api_not_found","path":"/api/users"}
```

```json
{"error":"Internal API error","code":"api_internal_error","path":"/api/users"}
```

`path` uses raw `Dream.target request`.

---

## Generated module registries and `server_main.ml`

Generate native registries (names can follow existing conventions) for:

- Page module render/metadata/layout resolvers
- API handler module resolvers
- API middleware module resolvers

`server_main.ml` should wire runtime via module calls, not inline giant lists, for both pages and APIs.

Conceptually:

```ocaml
let page_meta = Routes.get_all ()
let api_meta = Routes.Api.get_all ()
let pages = Route_modules.resolve_pages page_meta
let api_routes = Route_modules.resolve_api api_meta
let () = Utopia_server.start_generated ~pages ~api_routes ...
```

---

## Dune generation changes

### API native library

Generate a separate native API library (project-scoped name) containing API handlers and middleware modules.

- No Melange API compilation.
- Apply same native PPX stack policy as pages unless explicitly unnecessary.
- Auto-open generated `Lib` alias so API modules can use shared `lib/` modules consistently.

### Server executable links

Generated `server_main` executable must link:

- generated pages native library
- generated API native library
- shared `utopia` runtime library

---

## Remove standalone `utopia.server`

This phase removes the standalone server command/binary path.

- Delete/retire standalone `bin/server/*` executable target.
- CLI `dev`/`prod` always serve using generated `server_main.exe`.
- Docs/spec/primitives must no longer describe manifest-mode standalone fallback.

---

## Testing

### Cram tests

Create/update tests covering:

- compiler scans optional `api/` and generates route metadata modules
- page routes under `/api/*` fail at compile time
- API conflicts (`api/users.ml` vs `api/users/index.ml`)
- middleware collection by ancestry and extension conflict errors
- generated dune includes separate API native library and server linkage
- no `_utopia/routes.manifest` and no `_utopia/api.manifest` outputs
- generated `Routes` split behavior works in melange + native builds
- generated runtime serves API routes through `server_main.exe`
- API not found returns JSON 404 with exact `error/code/path`
- API exception returns JSON 500 with exact `error/code/path`

### Server tests

Add routing/runtime tests for:

- dispatch order assets -> API -> actions -> pages
- API params storage in request-local field
- typed accessor value shape (`Single`, `Catch_all`, `Optional_catch_all`)
- middleware order (outermost first)
- middleware short-circuit behavior

---

## Edge cases

- `api/` exists but has only middleware files
- empty `api/` directory
- deeply nested dynamic API routes
- route groups and parallel slots in `api/` path normalization
- optional catch-all root behavior (`api/[[...path]].ml`)
- API middleware in root `api/_middleware.*`
- large API route sets (100+)
- mixed page+API projects with no route manifest files

---

## Performance

API and page matchers continue using linear specificity-ordered scans in this phase. Keep implementation simple and correct; optimize only with profiling evidence.

---

## Files changed

| Action | File |
|--------|------|
| Modify | `bin/compiler/compiler.ml` (scan `api/`, remove manifest writes, generate route registries) |
| Modify | `bin/compiler/Routes.ml` (shared + native split generation inputs, reserved `/api` check) |
| Modify | `bin/compiler/Generated_dune.ml` (separate API native library + server linkage) |
| Modify | `bin/compiler/Server_main.ml` (load registries via `get_all`) |
| Modify | `lib/server/server.ml` (API runtime wiring, JSON error envelopes, dispatch order) |
| Modify | `lib/utopia/Utopia.re` and/or server runtime surface (add `respond` helper exposure) |
| Delete/Modify | `bin/server/*` (remove standalone server command path) |
| Create/Modify | compiler and server tests for API and registry-based loading |
| Modify | `plan/spec.md` |
| Modify | `plan/primitives.md` |

---

## Acceptance criteria

- API routes are collected from `api/` with page-equivalent segment conventions.
- `/api/*` is API-only; `pages` routes under `/api/*` fail compile-time.
- Middleware is collected by physical ancestry and composes outermost first.
- `Routes.get_all ()` and `Routes.Api.get_all ()` drive runtime loading.
- Route manifest files are not generated or read at runtime.
- `server_main.exe` is the only supported serving entrypoint.
- API 404/500 framework errors are JSON with exactly `error`, `code`, `path`.
- `Routes.Api.Params` accessors read request-local matched params with agreed value shapes.
- Generated builds still pass melange + native compilation for pages while supporting native API libs.
- All updated tests pass.
