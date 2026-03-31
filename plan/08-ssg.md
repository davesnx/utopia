# SSG

Add opt-in static site generation for pages that declare `let static = true`.

---

## Goal

Pages that export `let static = true` are rendered at build time. The resulting HTML is served directly without server-side rendering at request time. This is useful for content-heavy pages that don't need per-request data.

---

## Dependencies

- `plan/02-compiler-rsc.md` -- compiler generates server_main.ml
- `plan/03-server-rewrite.md` -- server library with DreamRSC rendering

---

## Detect static pages in the compiler

The compiler already scans page source files for `params.X` accesses. Extend this to detect `let static = true` (or `let static = true;` in Reason) exports.

Use simple source text scanning (not AST parsing):

```ocaml
let detect_static_export source =
  (* Match "let static = true" with flexible whitespace *)
  Str.string_match
    (Str.regexp {|.*let[ \t]+static[ \t]*=[ \t]*true|})
    source 0
```

This is intentionally simple. False positives are unlikely in practice. If a more robust approach is needed later, the compiler can use an OCaml parser.

---

## Record static flag in manifest

Extend the route manifest format to include a static flag:

```
<route>\t<kind>\t<source_file>\t<module>\t<matcher>\t<params>\t<layouts>\t<static>
```

Where `<static>` is `true` or `false`.

---

## Build-time rendering

During `dune build`, static pages are rendered to HTML files. The build pipeline:

1. Compiler marks pages as static in the manifest
2. The generated `server_main.ml` includes a `--ssg` mode that renders static pages:

```ocaml
let () =
  match Sys.argv with
  | [| _; "--ssg" |] ->
      (* Render all static pages to HTML files *)
      List.iter (fun (route, page, layouts) ->
        let element = compose_with_layouts layouts (page.make ()) in
        let html = ReactDOM.renderToStaticMarkup element in
        write_to_file (ssg_output_path route) html)
      static_pages
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

### Edge cases

- `let static = false` explicitly (should not be treated as static)
- `let static = true` in a comment (should not be detected)
- `let static = true` in a string literal (should not be detected)
- Static page with layouts (layouts should be rendered into the static HTML)
- Static page with client components (client JS should be included)
- Static page with no content (empty `make` function)
- Static markdown page (markdown pages with frontmatter `static: true`)
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
| Modify | `bin/compiler.ml` (detect static flag, validate static_paths) |
| Modify | `lib/utopia_server/utopia_server.ml` (serve static pages, SSG mode) |
| Modify | `lib/utopia_types/utopia_types.ml` (add static field to route types) |
| Create | `bin/tests/ssg_static_page_detected.t` |
| Create | `bin/tests/ssg_static_page_rendered.t` |
| Create | `bin/tests/ssg_dynamic_page_requires_paths.t` |
| Create | `bin/tests/ssg_dynamic_page_with_paths.t` |
| Create | `bin/tests/ssg_non_static_page_ignored.t` |

---

## Acceptance criteria

- `let static = true` is detected in page source files
- Static pages are rendered at build time to HTML files
- Static HTML is served directly without server-side rendering
- Dynamic static pages require `static_paths` export
- Static pages include layouts and client component scripts
- Fallback to SSR works when static HTML is missing
- All tests pass
