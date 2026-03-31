# API routes

Add file-based API routing with the same conventions as page routing.

---

## Goal

Files in `api/` map to API endpoints. Same segment parsing as pages (`[param]`, `[...slug]`, `[[...slug]]`). Handlers receive raw Dream requests and return Dream responses. Middleware composes by directory ancestry.

---

## Dependencies

- `plan/01-shared-types.md` -- shared types
- `plan/02-compiler-rsc.md` -- compiler infrastructure
- `plan/03-server-rewrite.md` -- server library to wire handlers into

---

## Extend the compiler to scan api/

Add a new scanning pass in `compiler.ml` that reads the `api/` directory using the same `read_files_recursive` function. The `api/` directory is optional -- if it doesn't exist, skip silently.

Reuse the same segment parsing logic (`parse_param_segment`, `normalize_path_segments`, etc.) for API routes. API routes use the same conflict detection and param validation.

---

## Define API route types

Add to shared types:

```ocaml
type api_route_entry = {
  route : string;
  matcher : string;
  conflict_key : string;
  params : (string * param_kind) list;
  middlewares : string list;
  source_file : string;
  module_name : string;
}
```

---

## Handle _middleware.ml files

A file named `_middleware.ml` (or `_middleware.re`) in any `api/` subdirectory is collected as middleware for that directory. The compiler:

1. Identifies middleware files by their basename (`_middleware`)
2. Excludes them from route generation (they are not endpoints)
3. Records them in a middleware-by-directory table (same pattern as layouts)
4. Each API route carries a list of middleware paths, ordered by directory ancestry (outermost first)

---

## Generate API route manifest

Create a new manifest file `_utopia/api.manifest` with format:

```
<route>\t<source_file>\t<module>\t<matcher>\t<params>\t<middlewares>
```

This is separate from `routes.manifest` to keep concerns clean.

---

## Generate API handler wiring in server_main.ml

The compiler extends the generated `server_main.ml` to include API route wiring:

```ocaml
let api_routes = [
  ("/api/health", (module Api_health_native : Utopia_server.Api_handler), []);
  ("/api/users/:id", (module Api_users_id_native : Utopia_server.Api_handler),
   [(module Api_middleware_native : Utopia_server.Api_middleware);
    (module Api_users_middleware_native : Utopia_server.Api_middleware)]);
]
```

---

## Generate dune rules for API modules

API route files get the same dual-compilation treatment as pages:

```scheme
(rule
 (deps ../api/health.ml)
 (targets Api_health_melange.ml Api_health_native.ml)
 (action
  (progn
   (run cp %{deps} Api_health_melange.ml)
   (run cp %{deps} Api_health_native.ml))))
```

API modules are included in the native library stanza. They may or may not need melange compilation (API routes are server-only), but for consistency with the dual-compilation model, include them in both.

Actually, API routes are server-only. Skip melange compilation for API modules. Only generate native variants.

---

## Define handler contract in server library

In `utopia_server.ml`:

```ocaml
module type Api_handler = sig
  val handler : Dream.request -> Dream.response Lwt.t
end

module type Api_middleware = sig
  val middleware : Dream.handler -> Dream.handler
end
```

---

## Wire API routes into request handling

Update `route_request` in the server library to check API routes before page routes:

```ocaml
let route_request ~pages ~api_routes request =
  let target = Dream.target request |> normalize_target in
  (* 1. Asset serving *)
  if starts_with target "dist/" || starts_with target "target/" then
    serve_asset target
  (* 2. API routes *)
  else if starts_with target "api/" then
    match find_api_match api_routes (target_segments target) with
    | None -> Dream.respond ~status:`Not_Found "API route not found"
    | Some (handler, middlewares, params) ->
        let wrapped = List.fold_right
          (fun (module M : Api_middleware) h -> M.middleware h)
          middlewares handler.handler
        in
        wrapped request
  (* 3. Page routes *)
  else
    handle_page_routes ~pages request
```

---

## API route conflict detection

API routes and page routes occupy separate namespaces. An API route `/api/users` does not conflict with a page route `/api/users` because they're served from different directories. However, two API files producing the same conflict key within `api/` is an error.

---

## Testing

### Cram tests

**`compiler_scans_api_directory.t`**
- Create `api/health.ml` and `api/users/[id].ml`
- Run the compiler
- Assert `_utopia/api.manifest` contains both routes
- Assert generated dune has API module rules

**`compiler_api_routes_conflict_detection.t`**
- Create `api/users.ml` and `api/users/index.ml`
- Run the compiler
- Assert conflict error is reported

**`compiler_api_middleware_collection.t`**
- Create `api/_middleware.ml`, `api/users/_middleware.ml`, `api/users/[id].ml`
- Run the compiler
- Assert manifest shows both middlewares for the `[id]` route

**`compiler_api_optional_directory.t`**
- Create only `pages/index.re` (no `api/` directory)
- Run the compiler
- Assert success (no error about missing `api/`)

**`compiler_api_param_validation.t`**
- Create `api/[id].ml` with `params.name` access
- Run the compiler
- Assert undeclared param error

### Unit tests (alcotest)

**`test_api_routing.ml`**
- API route matching with all segment types
- Middleware composition order (outermost first)
- API route specificity ordering
- API route with no middleware
- API route with multiple middleware layers

### Edge cases

- API route with catch-all segment (`api/[...path].ml`)
- API route with optional catch-all (`api/[[...path]].ml`)
- API route in deeply nested directory (`api/v1/users/[id]/posts/[post_id].ml`)
- Middleware file with `.re` extension instead of `.ml`
- Middleware file in root `api/` directory
- Empty `api/` directory
- `api/` directory with only middleware files (no handlers)
- API route file that is also a middleware (should error or be clear about precedence)
- API route with route group: `api/(v1)/users.ml`
- Very large number of API routes (100+)
- API route handler that throws an exception
- Middleware that short-circuits (returns response without calling downstream)
- Middleware that modifies the request

---

## Performance

API route matching uses the same linear scan as page routes. For typical API sizes (< 100 endpoints), this is fine. The middleware composition happens once per request and involves a small number of function wraps.

---

## Files changed

| Action | File |
|--------|------|
| Modify | `bin/compiler.ml` (add API scanning, manifest generation, dune rules) |
| Modify | `lib/utopia_server/utopia_server.ml` (add API handler types, routing) |
| Modify | `lib/utopia_server/utopia_server.mli` (expose API types) |
| Modify | `lib/utopia_types/utopia_types.ml` (add api_route_entry if needed) |
| Create | `bin/tests/compiler_scans_api_directory.t` |
| Create | `bin/tests/compiler_api_routes_conflict_detection.t` |
| Create | `bin/tests/compiler_api_middleware_collection.t` |
| Create | `bin/tests/compiler_api_optional_directory.t` |
| Create | `bin/tests/compiler_api_param_validation.t` |
| Create | `lib/utopia_server/test/test_api_routing.ml` |

---

## Acceptance criteria

- `api/` directory is scanned and API routes are generated
- API routes use the same segment conventions as pages
- Middleware files are collected and compose correctly
- API route conflicts are detected at compile time
- Param accesses are validated for API routes
- API requests are routed correctly by the server
- Middleware runs in the correct order
- Missing `api/` directory does not cause errors
- All tests pass
