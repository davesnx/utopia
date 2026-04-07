# SSG

Add opt-in static site generation for code pages that opt into static mode (`let static = true`).

---

## Goal

Pages that opt into static mode are rendered at build time. The resulting HTML is served directly without server-side rendering at request time. This is useful for content-heavy pages that don't need per-request data.

---

## Dependencies

- `plan/02-compiler-rsc.md` -- compiler generates server_main.ml
- `plan/03-server-rewrite.md` -- server library with DreamRSC rendering

---

## Detect static pages in the compiler

The compiler already scans page source files for `params.X` accesses. Extend this flow with a lightweight lexical scanner that detects `let static = true` (or `let static=true;`) while ignoring comments and string literals.

Do not use regex-only matching. The scanner should satisfy edge cases such as:

- `let static = true` inside `(* ... *)` comments
- `let static = true` inside `"..."` strings
- escaped quotes and nested comment blocks

Suggested implementation shape:

```ocaml
type mode = Code | String | Char | Line_comment | Block_comment of int

let detect_static_export source =
  let tokens = scan_code_tokens_ignoring_comments_and_strings source in
  token_sequence_exists tokens ["let"; "static"; "="; "true"]
```

This keeps implementation small and deterministic without pulling in a full parser.

---

## Record static flag in manifest

Extend the route manifest format to include a static flag:

```
<route>\t<kind>\t<source_file>\t<module>\t<matcher>\t<params>\t<layouts>\t<static>
```

Where `<static>` is `true` or `false`.

Also record static origin in generated metadata (`code_export`) so diagnostics can point to the source of invalid static configuration.

---

## Build-time rendering

During `dune build`, static pages are rendered to HTML files. The build pipeline:

1. Compiler marks pages as static in the manifest
2. The generated `server_main.ml` includes a `--ssg` mode that renders static pages through the same server/RSC HTML rendering path used for normal requests (not `renderToStaticMarkup`):

```ocaml
let () =
  match Sys.argv with
  | [| _; "--ssg" |] ->
      (* Render all static pages to HTML files using the standard HTML pipeline *)
      List.iter (fun static_route ->
        let html = Utopia_server.render_route_html_for_ssg static_route in
        write_to_file (ssg_output_path static_route.route) html)
      static_routes
  | _ ->
      (* Normal server mode *)
      start_server ()
```

3. A dune rule runs the server executable in SSG mode:

```scheme
(rule
 (alias ssg)
 (deps (alias all))
 (action (run ./_utopia/server_main.exe --ssg)))
```

Using the shared HTML path ensures layout composition, head metadata, and client bootstrap behavior stay consistent between SSR and SSG.

---

## Serve static pages

The server checks the static flag when handling requests. For static pages:

1. Check if the pre-rendered HTML file exists in `_utopia/static/`
2. If yes, serve it directly (no React rendering)
3. If no, fall back to server-side rendering (graceful degradation in dev mode)

```ocaml
let handle_static_page route =
  let static_path = Printf.sprintf "_utopia/static%s.html" (pp_route route) in
  if Sys.file_exists static_path then
    let html = read_file static_path in
    Dream.html html
  else
    (* Fallback to SSR *)
    handle_page_request ~page ~layouts ~bootstrap_modules request
```

---

## Static pages with dynamic segments

A page with `let static = true` and dynamic segments (e.g., `pages/blog/[slug].re`) requires an additional export to enumerate the paths:

```ocaml
let static = true
let static_paths () = [
  [("slug", "hello-world")];
  [("slug", "second-post")];
]
```

The SSG renderer calls `static_paths` to get all param combinations and renders each one.

If `static = true` but no `static_paths` is provided for a dynamic page, the compiler emits an error.

---

## Testing

### Cram tests

**`ssg_static_page_detected.t`**
- Create `pages/about.re` with `let static = true`
- Run the compiler
- Assert manifest marks the page as static

**`ssg_static_page_rendered.t`**
- Create a static page
- Run the full build (including SSG)
- Assert `_utopia/static/about.html` exists with rendered content

**`ssg_dynamic_page_requires_paths.t`**
- Create `pages/blog/[slug].re` with `let static = true` but no `static_paths`
- Run the compiler
- Assert error about missing `static_paths`

**`ssg_dynamic_page_with_paths.t`**
- Create a dynamic static page with `static_paths`
- Run the full build
- Assert multiple HTML files are generated

**`ssg_non_static_page_ignored.t`**
- Create a page without `let static = true`
- Run the build
- Assert no static HTML file is generated for it

**`ssg_static_detection_ignores_comments_and_strings.t`**
- Create a page where `let static = true` appears only in comments/strings
- Run the compiler
- Assert page is not marked static

### Edge cases

- `let static = false` explicitly (should not be treated as static)
- `let static = true` in a comment (should not be detected)
- `let static = true` in a string literal (should not be detected)
- `let static = true` in a char literal (should not be detected)
- Static page with layouts (layouts should be rendered into the static HTML)
- Static page with client components (client JS should be included)
- Static page with no content (empty `make` function)
- Very large number of static paths (1000+ for a dynamic page)
- Static page that throws an exception during rendering
- Re-rendering static pages when source changes (incremental SSG)

---

## Performance

Static pages are rendered once at build time. Serving them is a simple file read -- the fastest possible response path. For sites with many static pages, the SSG step adds build time proportional to the number of pages. Consider parallelizing the rendering if build times become an issue.

---

## Files changed

| Action | File |
|--------|------|
| Modify | `bin/compiler.ml` (detect static flag and validate static_paths for code pages) |
| Create | `bin/static_detector.ml` (comment/string-safe lexical scanner for `let static = true`) |
| Modify | `lib/utopia_server/utopia_server.ml` (serve static pages, SSG mode) |
| Modify | `lib/utopia_types/utopia_types.ml` (add static field to route types) |
| Create | `bin/tests/ssg_static_page_detected.t` |
| Create | `bin/tests/ssg_static_page_rendered.t` |
| Create | `bin/tests/ssg_dynamic_page_requires_paths.t` |
| Create | `bin/tests/ssg_dynamic_page_with_paths.t` |
| Create | `bin/tests/ssg_non_static_page_ignored.t` |
| Create | `bin/tests/ssg_static_detection_ignores_comments_and_strings.t` |

---

## Acceptance criteria

- `let static = true` is detected in page source files using a comment/string-safe scanner
- Static pages are rendered at build time to HTML files
- Static HTML is served directly without server-side rendering
- Dynamic static pages require `static_paths` export
- Static pages include layouts and client component scripts
- Fallback to SSR works when static HTML is missing
- All tests pass
