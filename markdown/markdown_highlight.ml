let theme = Ochre.Theme.gruvbox

let options =
  Ochre.Html_options.make ~pre_class:"utopia-markdown-code-block"
    ~code_class:"utopia-markdown-code" ()

let grammar_json = function
  | "c" -> Some Tm_grammars.c
  | "cpp" -> Some Tm_grammars.cpp
  | "css" -> Some Tm_grammars.css
  | "diff" -> Some Tm_grammars.diff
  | "dockerfile" -> Some Tm_grammars.dockerfile
  | "dune" -> Some Tm_grammars.dune
  | "go" -> Some Tm_grammars.go
  | "graphql" -> Some Tm_grammars.graphql
  | "html" -> Some Tm_grammars.html
  | "ini" -> Some Tm_grammars.ini
  | "java" -> Some Tm_grammars.java
  | "javascript" -> Some Tm_grammars.javascript
  | "json" -> Some Tm_grammars.json
  | "jsonc" -> Some Tm_grammars.jsonc
  | "jsx" -> Some Tm_grammars.jsx
  | "markdown" -> Some Tm_grammars.markdown
  | "mlx" -> Some Tm_grammars.mlx
  | "ocaml" -> Some Tm_grammars.ocaml
  | "opam" -> Some Tm_grammars.opam
  | "php" -> Some Tm_grammars.php
  | "python" -> Some Tm_grammars.python
  | "reason" -> Some Tm_grammars.reason
  | "ruby" -> Some Tm_grammars.ruby
  | "rust" -> Some Tm_grammars.rust
  | "scss" -> Some Tm_grammars.scss
  | "shellscript" -> Some Tm_grammars.shellscript
  | "sql" -> Some Tm_grammars.sql
  | "swift" -> Some Tm_grammars.swift
  | "toml" -> Some Tm_grammars.toml
  | "tsx" -> Some Tm_grammars.tsx
  | "typescript" -> Some Tm_grammars.typescript
  | "xml" -> Some Tm_grammars.xml
  | "yaml" -> Some Tm_grammars.yaml
  | _ -> None

let highlighters : (string, Ochre.t option) Hashtbl.t = Hashtbl.create 17

let highlighter_for_lang lang =
  match Hashtbl.find_opt highlighters lang with
  | Some highlighter -> highlighter
  | None ->
      let highlighter =
        match grammar_json lang with
        | None -> None
        | Some grammar -> (
            try Some (Ochre.create_from_json ~grammars:[ (lang, grammar) ] ())
            with _ -> None)
      in
      Hashtbl.replace highlighters lang highlighter;
      highlighter

let normalized_lang lang =
  match String.lowercase_ascii (String.trim lang) with
  | "" | "plain" | "plaintext" | "text" -> None
  | "bash" | "console" | "shell" | "sh" | "zsh" -> Some "shellscript"
  | "c++" -> Some "cpp"
  | "js" | "node" -> Some "javascript"
  | "md" -> Some "markdown"
  | "ml" | "mli" -> Some "ocaml"
  | "rb" -> Some "ruby"
  | "re" | "reasonml" -> Some "reason"
  | "ts" -> Some "typescript"
  | "yml" -> Some "yaml"
  | lang -> Some lang

let highlight_html ~lang code =
  match normalized_lang lang with
  | None -> None
  | Some lang -> (
      match highlighter_for_lang lang with
      | None -> None
      | Some highlighter -> (
          try Some (Ochre.to_html highlighter ~options ~theme ~lang code)
          with _ -> None))
