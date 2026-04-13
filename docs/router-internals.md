# Router Internals

## Architecture Overview

The router spans three layers: the **compiler** (route discovery and code generation), the **server** (request matching, RSC streaming, diff computation), and the **client** (history management, navigation, layout diffing).

```
Compiler                    Server                      Client
─────────────               ─────────────               ─────────────
app/ filesystem             Dream request handler       React component
    │                           │                           │
    ▼                           ▼                           ▼
Routes.ml (generated)       route_request()             <Utopia_router>
    │                           │                           │
    ▼                           ▼                           ▼
server_main.ml (generated)  stream_html / stream_model  navigate()
                                │                           │
                                ▼                           ▼
                            RSC wire format              VirtualHistory
                                                        HistoryCache
```

### Key source files

| File | Layer | Role |
|------|-------|------|
| `bin/compiler/Routes.ml` | Compiler | Filesystem scanning, route signature parsing |
| `bin/compiler/Generated_routes.ml` | Compiler | Generates `Routes.ml` with typed route tree |
| `bin/compiler/Server_main.ml` | Compiler | Generates `server_main.ml` wiring routes to server |
| `lib/utopia/Utopia_route.ml` | Shared | Core `Route.t` type, URL parsing, constructors |
| `lib/utopia/Utopia_server.ml` | Server | Request dispatch, route matching, diff computation |
| `lib/utopia/Utopia_router.re` | Client | Router component, navigation, history, SSE |
| `lib/utopia/Utopia_router_route.re` | Client | Layout boundary component |
| `lib/utopia/Utopia_router_link.re` | Client | `<Link>` component |
| `lib/utopia/Utopia_route_builder.mlx` | Shared | Builds router tree from route metadata + layouts |
| `lib/utopia/Utopia.re` | Shared | Public API re-exports |

## Route Discovery and Code Generation

### Filesystem scanning

The compiler (`bin/compiler/compiler.ml`) scans the `app/` directory for page and layout files. Route segments are derived from directory names:

- `app/about/page.re` produces segments `["about"]`, matching `/about`
- `app/users/[id]/page.re` produces segments `["users", Param("id", Single)]`
- `app/blog/[...slug]/page.re` produces segments `["blog", Param("slug", Catch_all)]`

Dynamic segments are parsed in `Routes.parse_param_segment`:
- `[name]` -- single param
- `[...name]` -- catch-all (one or more segments)
- `[[...name]]` -- optional catch-all (zero or more segments)

### Layout collection

`Routes.collect_layouts` walks the directory tree for `layout.*` files. For each page, `layouts_for_file` collects all ancestor layouts from the root down. A page at `app/about/team/page.re` gets layouts: `[app/layout.re, app/about/layout.re]` (if both exist).

### Generated `Routes.ml`

`Generated_routes.generate` produces a module with:

1. **Typed route tree** -- nested modules mirroring the filesystem. Each leaf has a `make` function returning `Utopia_route.t`:

   ```ocaml
   module About = struct
     let make () = Utopia_route.from_segments ~segments:["about"] ()
     let route = make ()
   end

   module Users = struct
     module Param_id = struct
       let make ~id () = Utopia_route.from_segments ~segments:["users"; id] ()
     end
   end
   ```

2. **`Current` module** -- a variant type for exhaustive matching:

   ```ocaml
   module Current = struct
     type t =
       | Home_page
       | About_page
       | Users_param_id of { id : string }

     let of_route route =
       let segments = Utopia_route.path_segments route in
       match List.map String.lowercase_ascii segments with
       | [] -> Some Home_page
       | ["about"] -> Some About_page
       | ["users"; id] -> Some (Users_param_id { id })
       | _ -> None
   end
   ```

   Matching is case-insensitive and ordered by specificity (static segments first).

3. **Page metadata** -- route entries with their segment patterns, used by the server for request matching.

### Generated `Routes_server.ml`

`Routes` preserves the public generated route API, including native-only metadata/API helpers. `Routes_server` layers the unavoidable raw page/layout/API registries and not-found wiring on top. The checked-in runtime `server_main.ml` passes `Routes_server` to `Utopia_server`, which calls `Utopia_route_builder.build_router` to produce three closures per route:
- `shell(location)` -- full document wrapped in `<Utopia_router initialPath=location>`
- `tree()` -- the complete nested layout/page tree
- `subtree(parent_route)` -- partial tree below a given layout boundary

## Server-Side Route Handling

### Request dispatch (`route_request`)

`Utopia_server.route_request` processes each incoming request:

1. Dev endpoints (`/_utopia/dev-events`) -- SSE for hot reload
2. Static assets (`/target/`, `/dist/`, stylesheets) -- served directly
3. API routes (paths starting with `api/`) -- dispatched to API handlers
4. POST requests -- handled as server function calls
5. Page routes -- matched against registered route entries

### Route matching (`match_segments`)

Routes are tried in specificity order. The matching algorithm walks route segments against URL path segments:

```
Static "about" :: rest  matches  "about" :: rest     (exact string match, case-insensitive)
Param("id", Single)     matches  any single segment  (captures as param)
Param("slug", Catch_all)         matches  one or more remaining segments
Param("slug", Optional_catch_all) matches zero or more remaining segments
```

Specificity weights: `Static` (4) > `Single` (3) > `Catch_all` (2) > `Optional_catch_all` (1). Routes with higher total specificity are tried first, so `/users/settings` always takes priority over `/users/[id]`.

### Full HTML vs RSC response

The server checks the `Accept` header to decide the response format:

- **No special header** (initial page load): `stream_html` renders a full HTML document with embedded RSC data for hydration. The RSC stream is inlined so the client can hydrate without an extra round-trip.
- **`Accept: application/react.component`** (client navigation): `stream_model` returns the RSC wire format directly. The client decodes this progressively via `React_server_dom_esbuild.createFromFetch`.

### Diff computation (`route_navigation_model`)

When the client navigates, it may send an `X-Utopia-Current-Path` header containing the current route. The server uses this to compute a partial response:

1. **Same path** -- returns the full tree (needed for revalidation).
2. **Different path, shared prefix exists** -- computes `diff_parent_route` (the longest shared layout boundary path), renders only the subtree below it, and returns `["diff", parentRoute, subtreeElement]`.
3. **No shared prefix or subtree render fails** -- falls back to `["full", "", fullTreeElement]`.

`diff_parent_route` finds the common prefix of URL segments between current and target paths, capped by the route definition's segment count. For example, navigating from `/about/team` to `/about/contact` where the route has segments `["about", Param("page", Single)]` yields a parent route of `/about`.

## Client-Side Router

### Component tree

```
<Utopia_router initialPath children>
  └─ React.Context.Provider (router state)
      └─ <Utopia_router_route path="/" layout={rootLayout}>
           └─ <Utopia_router_route path="/about" layout={aboutLayout}>
                └─ <Utopia_router_route path="/about/team" layout={pageElement}>
```

`<Utopia_router>` is a `@react.client.component` that manages all navigation state. It wraps `children` in a context provider exposing `{ path, route, navigate }`.

### Navigation flow

When `navigate(~history, ~freshness, targetRoute)` is called:

```
1. Compute current and target paths
2. Same browser path and not revalidating?     -> no-op
3. Same request path (only hash changed)?      -> update history + state
4. Otherwise:
   a. Determine if diff is possible (different pathname + not revalidating)
   b. Fetch from server with appropriate headers
   c. Decode RSC response -> (mode, parentRoute, element)
   d. If "diff" mode:
      - Find layout boundary via VirtualHistory
      - Swap page content in-place
      - Cache as DiffPage
   e. If "full" mode:
      - Replace entire page tree
      - Cache as FullPage
   f. Update browser history (push or replace)
   g. Update React state (route + path)
```

On error, the router reports to the dev overlay via `window.__utopia_dev_report_error`.

### `commitNavigation` helper

Inside `navigate`, a local `commitNavigation` function handles the common pushState/replaceState + state update sequence. This is called from the hash-only branch, the diff branch, and the full-page branch, avoiding duplicated history manipulation code.

## VirtualHistory

`VirtualHistory` tracks active layout boundaries on the client. It's a mutable list of `{ path, renderPage }` entries.

### How entries are registered

Each `<Utopia_router_route>` component registers itself on first render (client only) via `VirtualHistory.push(~path, ~renderPage)`. The `renderPage` callback is the layout boundary's state setter -- calling it swaps the page content inside that layout without unmounting the layout itself.

### How entries are used during navigation

When a diff response arrives with `parentRoute = "/about"`:

1. `VirtualHistory.find("/about")` looks up the layout boundary at that path
2. `cleanPathState("/about")` removes entries for deeper paths (they'll be recreated when the new subtree renders)
3. `route.renderPage(subtreeElement)` calls the boundary's setter, swapping in the new page

### Cleanup

- `VirtualHistory.cleanup()` -- called by `renderFullPage`, clears all entries (a full page replace destroys all layout boundaries; new ones register on render)
- `VirtualHistory.cleanPathState(path)` -- called by `renderDiffPage`, removes entries deeper than `path` (they'll re-register when the new subtree mounts)

### Data structure

The backing store is a mutable `ref(list)`. Operations are O(n) where n is the nesting depth (typically 2-5 levels). `push` uses a single-pass recursive traversal that replaces-or-appends in one allocation.

## HistoryCache

`HistoryCache` is an LRU cache (max 16 entries) keyed by `request_path`. It stores the React element tree for previously visited pages so browser back/forward can restore them instantly.

### Cache entry types

```reason
type page =
  | FullPage(React.element)              /* complete page tree */
  | DiffPage(string, React.element)      /* (parentRoute, subtree element) */
```

### Population

- **Initial load**: the current page is cached as `FullPage`
- **Full navigation**: the fetched element is cached as `FullPage`
- **Diff navigation**: the subtree is cached as `DiffPage(parentRoute, element)`

### Consumption on popstate (back/forward)

When the user hits back/forward, the `handlePopState` handler fires:

1. **`FullPage(page)` in cache** -- calls `renderFullPage(page)` for instant restore
2. **`DiffPage(parentRoute, page)` in cache** -- attempts `renderDiffPage`. If the layout boundary still exists in VirtualHistory, the page swaps in instantly. If not (e.g., after a full page render destroyed the boundaries), falls back to a fresh fetch with `Replace + Revalidate`.
3. **Not in cache** (evicted from LRU) -- triggers a fresh navigation with `Replace + Revalidate`

### Eviction

When the cache reaches 16 entries, the oldest entry (FIFO) is evicted. The `keyQueue` maintains insertion order; `Hashtbl` provides O(1) lookup.

## Platform Splits (`switch%platform`)

The router uses Melange's platform-conditional compilation to handle server vs client differences:

| Function | Client | Server |
|----------|--------|--------|
| `browserWindow()` | `window` | `failwith(...)` |
| `browserHistory()` | `window.history` | `failwith(...)` |
| `browserEventTarget()` | `window` as EventTarget | `failwith(...)` |
| `currentUrl()` | `new URL(location.href)` | `failwith(...)` |
| `findRouteAnchor(event)` | DOM traversal | `None` |
| Context provider | `React.createElement(provider, ...)` | `provider(...)` |

Functions annotated `let%browser_only` (`reportNavigationError`, `fetchNavigation`, `navigate`) are compiled away entirely on the server.

The server-side provider supplies a `navigate` function that always throws -- navigation only happens on the client.

## SSE Connection (Dev Mode)

In development, the router's companion module `Utopia_dev_client` maintains a Server-Sent Events connection to `/_utopia/dev-events`. This drives:

- **Build state updates** -- the overlay shows a progress bar during rebuilds and error modals for build failures
- **Auto-reload** -- when a build transitions from non-healthy to healthy, the page reloads automatically via `window.location.reload()`
- **Reconnection** -- when the SSE connection drops (server restart), the client shows a progress bar and reloads once reconnected

The SSE lifecycle is independent of the router's navigation system but shares the same page context.

## React Server Components Integration

### Initial page load

The server renders the page to HTML via `ReactServerDOM.render_html`, embedding the RSC data stream inline. On the client, `client_entry.re` reads this stream from `window.srr_stream.readable_stream` and passes it to `React_server_dom_esbuild.createFromReadableStream` to hydrate the React tree.

### Client navigation

`fetchNavigation` sends a `fetch()` request with `Accept: application/react.component`. The response promise is passed directly to `React_server_dom_esbuild.createFromFetch`, which progressively decodes the RSC wire format. The resolved value is a `(mode, parentRoute, element)` tuple that the router uses to update the page.

### Server functions

Server functions (RPCs) are transported via `Utopia_call_server.callServer`, which is passed as the `~callServer` option to both `createFromReadableStream` (hydration) and `createFromFetch` (navigation). This allows server components to reference server functions that the client can invoke transparently.

## Layout Boundary Component (`Utopia_router_route`)

Each node in the router tree is a `<Utopia_router_route>` component that:

1. Holds a `pageconsumer` state (the page content rendered inside this layout)
2. On first client render, registers with `VirtualHistory.push(~path, ~renderPage)` where `renderPage` is its state setter
3. Renders: `layout(pageconsumer)` -- the layout wrapping the current page

This is the mechanism that makes partial navigation possible: the server can send just a subtree, and the router can inject it at the right layout boundary without re-rendering anything above it.

When no user-defined layout exists at a directory level, `PassThroughLayout` (an identity component: `children => children`) fills the gap. This preserves the nesting structure so diff navigation can still target intermediate boundaries.
