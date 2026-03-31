# Markdown pipeline

Add frontmatter support, fix crashes, unify rendering paths, and integrate markdown into RSC.

---

## Goal

Markdown pages should be first-class citizens in the RSC pipeline. They participate in layouts, support frontmatter metadata, and render through the same React component tree as code pages. Fix the known crashes (tables, footnotes) and unify the two separate markdown rendering paths.

---

## Dependencies

- `plan/03-server-rewrite.md` -- server renders via DreamRSC

---

## Add frontmatter parsing

Parse YAML frontmatter at the top of markdown files, delimited by `---`:

```markdown
---
title: My Guide
description: A comprehensive guide
path: custom-route
layout: pages/docs/layout.re
---

# Content starts here
```

The frontmatter parser:

1. Checks if the file starts with `---\n`
2. Finds the closing `---\n`
3. Extracts the YAML between them
4. Strips the frontmatter before passing content to cmarkit

Implement a minimal YAML parser (only flat key-value strings are needed, no nested structures). Do not add a full YAML library dependency for four string fields.

Supported fields:
- `title` -- string, page title for `<head>`
- `description` -- string, meta description
- `path` -- string, override the filesystem-inferred route
- `layout` -- string, explicit layout file path

---

## Update compiler for frontmatter

The compiler reads frontmatter from markdown files to:

1. Apply `path` overrides when generating routes
2. Record `title` and `description` in an extended manifest format
3. Apply `layout` overrides (replace the directory-ancestry-inferred layouts)

Add frontmatter parsing to the compiler's page processing pipeline. This runs before route generation so path overrides take effect.

---

## Fix table rendering

The current renderer has `assert false` for tables. Implement table rendering:

```ocaml
let render_table ~header ~rows =
  let render_cell cell =
    let content = render_inline cell in
    React.createElement "td" [] [content]
  in
  let render_header_cell cell =
    let content = render_inline cell in
    React.createElement "th" [] [content]
  in
  let thead = React.createElement "thead" []
    [React.createElement "tr" [] (List.map render_header_cell header)] in
  let tbody = React.createElement "tbody" []
    (List.map (fun row ->
       React.createElement "tr" [] (List.map render_cell row))
     rows) in
  React.createElement "table" [] [thead; tbody]
```

Handle column alignment (left, center, right) via `style` or `class` attributes.

---

## Fix footnote rendering

The current renderer has `assert false` for footnotes. Implement footnote rendering:

1. Collect footnotes during rendering (they are inline references)
2. Generate a `<sup><a href="#fn-N">[N]</a></sup>` at the reference site
3. Append a `<section class="footnotes">` at the end with the footnote content
4. Each footnote has an `id="fn-N"` and a back-link

---

## Unify rendering paths

The server currently has two markdown rendering paths:

1. `utopia.markdown` executable: uses `Render.of_doc` which goes through React components via `server-reason-react`
2. Server's `render_markdown_page`: uses `Cmarkit_html.of_doc` directly (plain HTML)

Unify on the React path. All markdown rendering should go through `Render.of_doc` so that:
- Custom components work everywhere
- Markdown pages participate in the RSC pipeline
- The output is consistent

---

## Integrate markdown into RSC pipeline

After the server rewrite, markdown pages should render as React elements:

```ocaml
let render_markdown_page source_file =
  let markdown = read_file source_file in
  let doc = Cmarkit.Doc.of_string ~layout:true ~strict:false markdown in
  Render.of_doc ~components:default_components doc
```

The resulting React element is wrapped in layouts and rendered via DreamRSC just like code pages.

---

## Add custom component support via lib/

Users place custom component modules in `lib/` and reference them in `utopia.ml` (or a future configuration mechanism). For now, the default components from `markdown/components.ml` are used.

The `Components.t` record is already extensible. The integration point is passing user-defined components to `Render.of_doc`.

---

## Testing

### Cram tests

**`markdown_frontmatter_basic.t`**
- Markdown with title and description frontmatter
- Assert frontmatter is stripped from rendered output
- Assert title is available in metadata

**`markdown_frontmatter_path_override.t`**
- Markdown with `path: custom-path` frontmatter
- Run the compiler
- Assert route manifest uses the overridden path instead of the filename

**`markdown_frontmatter_layout_override.t`**
- Markdown with `layout: pages/docs/layout.re`
- Run the compiler
- Assert route manifest uses the specified layout

**`markdown_frontmatter_missing.t`**
- Markdown without frontmatter
- Assert renders normally with no errors

**`markdown_table_rendering.t`**
- Markdown with a simple table
- Assert rendered output contains `<table>`, `<thead>`, `<tbody>`, `<th>`, `<td>`

**`markdown_table_with_alignment.t`**
- Markdown table with left, center, right alignment
- Assert alignment attributes are present

**`markdown_footnote_rendering.t`**
- Markdown with footnote references and definitions
- Assert `<sup>` links and footnote section are rendered

### Update existing markdown tests

Promote `markdown/tests/main.t` and `markdown/tests/simple.t` expected output after any changes to the rendering pipeline.

### Edge cases

- Frontmatter with unknown fields (should be silently ignored)
- Frontmatter with empty values (`title: `)
- Frontmatter with no closing `---` (treat entire file as content, no frontmatter)
- Frontmatter that is only `---\n---\n` (empty frontmatter)
- Frontmatter with multiline values (should error or handle gracefully)
- Frontmatter with special characters in values (colons, quotes)
- Markdown file that starts with `---` but is actually a thematic break
- Table with mismatched column counts
- Table with empty cells
- Table nested inside a list (edge case in CommonMark)
- Footnote with multiple paragraphs
- Footnote referenced but not defined
- Footnote defined but not referenced
- Very large markdown file (100KB+)
- Markdown with custom component overrides
- Markdown with client components in custom overrides (the interesting RSC case)

---

## Performance

Frontmatter parsing adds minimal overhead (string scanning for `---` delimiters). Table and footnote rendering are proportional to content size. No performance concerns.

---

## Files changed

| Action | File |
|--------|------|
| Modify | `markdown/render.ml` (add table rendering, footnote rendering) |
| Modify | `markdown/components.ml` (add table/footnote component types) |
| Modify | `markdown/elements.re` (add table/footnote default elements) |
| Create | `markdown/frontmatter.ml` (frontmatter parser) |
| Modify | `markdown/markdown.ml` (use frontmatter parser) |
| Modify | `bin/compiler.ml` (read frontmatter, apply path/layout overrides) |
| Modify | `lib/utopia_server/utopia_server.ml` (use React rendering for markdown) |
| Create | `markdown/tests/frontmatter.t` |
| Create | `markdown/tests/tables.t` |
| Create | `markdown/tests/footnotes.t` |
| Modify | `markdown/tests/main.t` (promote if needed) |
| Modify | `markdown/tests/simple.t` (promote if needed) |

---

## Acceptance criteria

- Frontmatter is parsed and stripped before rendering
- `path` overrides change the route in the manifest
- `layout` overrides change the layout chain
- `title` and `description` are available as metadata
- Tables render correctly with alignment support
- Footnotes render with links and back-links
- No `assert false` crashes remain in the renderer
- Markdown pages render through the React pipeline (not Cmarkit_html)
- All markdown cram tests pass
- The demo markdown page (`lola.md`) renders correctly
