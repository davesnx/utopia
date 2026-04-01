(lang dune 3.9)
; (using directory-targets 0.1)
(using melange 0.1)

(dialect
 (name mlx)
 (implementation
  (extension mlx)
  (preprocess
   (run mlx-pp %{input-file}))))

(cram enable)

(name utopia)

(license MIT)

(maintainers "David Sancho <dsnxmoreno@gmail.com>")

(authors "David Sancho <dsnxmoreno@gmail.com>")

(source
 (github davesnx/utopia))

(generate_opam_files true)

(implicit_transitive_deps true)

(package
 (name utopia)
 (synopsis "The SSG for server-reason-react")
 (description "A static site generator for server-reason-react")
  (depends
   ;; General system dependencies
   (ocaml (>= 5.0.0))
   (reason (>= 3.17.2))

   ;; Library dependencies
    cmarkit
    ochre
    tm-grammars
    dream
     server-reason-react
     mlx
    fpath
    (reason-react (>= 0.16.0))
   (reason-react-ppx (>= 0.16.0))
   (melange-json (>= 2.0.0))
   (melange-json-native (>= 2.0.0))
    dune-rpc
    dune-rpc-lwt
    (csexp (>= 1.5.0))
    sexplib0
    (lwt (>= 5.6.0))
    pp

  ;; Test dependencies
  (alcotest :with-test)
  (alcotest-lwt :with-test)

  ;; Documentation
  (odoc :with-doc)

  ;; Dev dependencies, using with-test so that consumers don't install them (until package is released in opam)
   (ocamlformat
    (and
     (= 0.28.1)
     :with-test))
  (ocaml-lsp-server :with-test)
))
