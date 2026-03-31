# Server rewrite

Extract the server into a library and replace placeholder rendering with DreamRSC.

---

## Goal

The current server reads source files and renders them as escaped text in `<pre>` tags. Rewrite it to use `server-reason-react`'s `DreamRSC` for actual React Server Component rendering. Extract reusable framework logic into a library so the generated per-project executable can link against it.

---

## Dependencies

- `plan/00-cleanup.md` -- dead code removed
- `plan/01-shared-types.md` -- shared types available
- `plan/02-compiler-rsc.md` -- compiler generates `server_main.ml` that imports this library

---

## Extract server library

Split `bin/server.ml` into two parts:

**`lib/utopia_server/utopia_server.ml`** (the library):
- Route manifest loading and parsing
- Route matching (`match_segments`, `find_match`, `compare_route_specificity`)
- Asset serving (`serve_asset`, `content_type_for_asset`)
- Request routing (`route_request`)
- DreamRSC rendering integration
- Page cache
- Dev route index
- Utility functions (`normalize_target`, `target_segments`, `escape_html`)

**`_utopia/server_main.ml`** (generated per-project executable):
- Imports `Utopia_server` and the `Pages` library
- Passes page modules to the server
- Calls `Utopia_server.start`

The current `bin/server.ml` standalone executable is replaced by this library + generated executable pattern.

---

## Create the library dune stanza

```
lib/
  utopia_server/
    dune
    utopia_server.ml
    utopia_server.mli
```

```scheme
(library
 (name utopia_server)
 (public_name utopia.server_lib)
 (libraries
  utopia.types
  server-reason-react.react
  server-reason-react.reactDom
  dream
  lwt
  lwt.unix
  unix
  cmarkit
  logs
  fmt))
```

---

## Define the Page module type

The library defines what a page module must expose:

```ocaml
module type Page = sig
  val make : unit -> React.element
end
```

Optional exports are detected at the module level:

```ocaml
module type Page_with_meta = sig
  include Page
  val title : string
  val description : string
end
```

The generated `server_main.ml` uses first-class modules to pass pages to the server.

---

## Replace HTML string rendering with DreamRSC

### Initial page load (GET without RSC header)

Replace `render_code_page` and `render_markdown_page` with:

```ocaml
let handle_page_request ~page ~layouts ~bootstrap_modules request =
  let element = compose_with_layouts layouts (page ()) in
  DreamRSC.stream_html ~bootstrapModules:bootstrap_modules element request
```

Where `compose_with_layouts` nests the page element inside layout components:

```ocaml
let compose_with_layouts layouts page_element =
  List.fold_right
    (fun layout_module element ->
       let (module Layout : Layout) = layout_module in
       Layout.make ~children:element ())
    layouts
    page_element
```

### Client-side navigation (GET with RSC header)

```ocaml
let handle_rsc_request ~page ~layouts request =
  let element = compose_with_layouts layouts (page ()) in
  let location = Dream.target request in
  DreamRSC.stream_model_value ~location element request
```

Detect RSC requests by checking the `Accept` header for `application/react.component`.

### Server function invocation (POST)

```ocaml
let handle_server_function request =
  DreamRSC.streamFunctionResponse request
```

---

## Update request routing

The new `route_request` function:

```ocaml
let route_request ~pages ~layouts ~bootstrap_modules request =
  let target = Dream.target request |> normalize_target in
  (* 1. Asset serving *)
  if starts_with target "dist/" then serve_asset target
  else if starts_with target "target/" then serve_asset target
  else
    let segments = target_segments target in
    (* 2. Dev index *)
    if segments = [] then Dream.html index_html
    else
      match find_match routes segments with
      | None -> Dream.respond ~status:`Not_Found "Route not found"
      | Some (route, params) ->
          let accept = Dream.header request "Accept" in
          let method_ = Dream.method_ request in
          match (method_, accept) with
          | `POST, _ ->
              handle_server_function request
          | `GET, Some "application/react.component" ->
              handle_rsc_request ~page:route.page ~layouts:route.layouts request
          | `GET, _ ->
              handle_page_request ~page:route.page ~layouts:route.layouts
                ~bootstrap_modules request
          | _ ->
              Dream.respond ~status:`Method_Not_Allowed "Method not allowed"
```

---

## Update the page cache

The mtime-based cache still works but now caches the rendered React element tree rather than HTML strings. Actually, since DreamRSC streams responses, caching at the HTML level is no longer appropriate. The cache should move to caching the React element tree or be removed entirely (DreamRSC may handle its own caching).

For the initial implementation, remove the page cache. DreamRSC streaming is the primary optimization. Re-add caching later if profiling shows it's needed.

---

## Add dist/ asset serving

Add `dist/` as an asset root for esbuild output. Update `asset_roots`:

```ocaml
let asset_roots = [
  "_utopia/dist";
  "_build/default/_utopia/dist";
  "_utopia";
  "_build/default/_utopia";
]
```

Add content types for additional asset extensions:

```ocaml
| ".wasm" -> "application/wasm"
| ".svg" -> "image/svg+xml"
| ".png" -> "image/png"
| ".ico" -> "image/x-icon"
| ".woff2" -> "font/woff2"
| ".woff" -> "font/woff"
```

---

## Fix the HOST bug

The server currently ignores the `HOST` environment variable. Add `~interface` to the `Dream.run` call:

```ocaml
let host_from_env () =
  match Sys.getenv_opt "HOST" with
  | None -> "127.0.0.1"
  | Some h -> h

Dream.run ~port:(port_from_env ()) ~interface:(host_from_env ()) pipeline
```

---

## Layout composition

Layouts are no longer HTML string wrappers. They are React components composed via the component tree:

```ocaml
module type Layout = sig
  val make : children:React.element -> unit -> React.element
end
```

The server loads layout modules from the `pages` library (they are compiled as `Layout_native` modules). The route manifest tells the server which layouts apply to each route. The generated `server_main.ml` maps layout source paths to actual modules.

---

## Testing

### New cram tests

**`server_handles_rsc_header.t`** (integration test, may need a test harness)
- Start server with a test page
- Send GET with `Accept: application/react.component`
- Assert response content type is not HTML

**`server_serves_dist_assets.t`**
- Create a `_utopia/dist/test.js` file
- Start server
- GET `/dist/test.js`
- Assert 200 with correct content type

**`server_rejects_post_without_action.t`**
- POST to a page route
- Assert appropriate error response

### Unit tests (alcotest)

Create `lib/utopia_server/test/` with:

**`test_routing.ml`**
- `match_segments` with static paths
- `match_segments` with single params
- `match_segments` with catch-all params
- `match_segments` with optional catch-all params
- `match_segments` with mixed segments
- `match_segments` with empty segments (root)
- `match_segments` mismatch cases
- `find_match` with specificity ordering (static wins over param)
- `find_match` with no match
- `compare_route_specificity` ordering

**`test_manifest.ml`**
- `load_routes` with valid manifest
- `load_routes` with missing file
- `load_routes` with malformed entries
- `load_routes` with empty file
- `parse_matcher` with all segment types
- `parse_params` with all param kinds
- `parse_params` with empty params

**`test_assets.ml`**
- `content_type_for_asset` for all known extensions
- `content_type_for_asset` for unknown extension
- `contains_path_traversal` with various attack patterns
- `first_existing_asset` with multiple roots
- `serve_asset` with path traversal (400)
- `serve_asset` with missing file (404)

**`test_target.ml`**
- `normalize_target` with `/`, `/about`, empty string, no leading slash
- `target_segments` with root, single segment, multiple segments, trailing slash

### Edge cases

- Route with zero layouts
- Route with multiple nested layouts
- Request to route with all param types simultaneously
- Very long URL (1000+ segments)
- URL with encoded characters
- URL with query string (should be stripped before matching)
- Concurrent requests to the same route
- Server startup with empty manifest
- Server startup with manifest containing only markdown pages
- Asset request with `..` in various positions (`../etc/passwd`, `foo/../../bar`)
- Asset request with URL-encoded `..` (`%2e%2e`)

---

## Performance

### Benchmarks to update

Update `bench/bench_routing.ml` to use the shared types from `utopia.types`. The benchmarks test routing hot paths and should continue to work after the server rewrite.

### Streaming

DreamRSC streams HTML to the client. This means Time to First Byte (TTFB) is much better than buffering the entire page. No additional optimization needed for the initial implementation.

### Keep the linear route scan

The current O(n) route matching is fine for typical project sizes (< 500 routes). The benchmarks already show acceptable performance at 500 routes. If profiling later shows this is a bottleneck, a trie-based router can be added.

---

## Files changed

| Action | File |
|--------|------|
| Create | `lib/utopia_server/dune` |
| Create | `lib/utopia_server/utopia_server.ml` |
| Create | `lib/utopia_server/utopia_server.mli` |
| Delete | `bin/server.ml` (replaced by the library) |
| Modify | `bin/dune` (remove Server executable, or keep as a thin wrapper during transition) |
| Create | `lib/utopia_server/test/dune` |
| Create | `lib/utopia_server/test/test_routing.ml` |
| Create | `lib/utopia_server/test/test_manifest.ml` |
| Create | `lib/utopia_server/test/test_assets.ml` |
| Create | `lib/utopia_server/test/test_target.ml` |
| Modify | `bench/bench_routing.ml` (add comment about intentional duplication) |

---

## Acceptance criteria

- Server renders actual React component trees via DreamRSC (not `<pre>` escaped source)
- GET requests return streamed HTML with embedded RSC payload
- GET with `Accept: application/react.component` returns RSC payload
- Layouts compose as React component nesting, not HTML string wrapping
- `HOST` environment variable is respected
- `dist/` assets are served correctly
- All unit tests pass (alcotest)
- All cram tests pass
- No performance regression in routing benchmarks
