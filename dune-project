(lang dune 3.8)
(using directory-targets 0.1)

(cram enable)

(name utopia)

(license MIT)

(maintainers "David Sancho <dsnxmoreno@gmail.com>")

(authors "David Sancho <dsnxmoreno@gmail.com>")

(source
 (github davesnx/utopia))

(generate_opam_files true)

(implicit_transitive_deps false)

(package
 (name utopia)
 (synopsis "")
 (depends
  ;; General system dependencies
  (ocaml (>= 5.0.0))
  (reason (>= 3.10.0))

  ;; Library dependencies
  (pcre (<= 7.5.0))

  ;; Test dependencies
  (alcotest :with-test)
  (alcotest-lwt :with-test)

  ;; Documentation
  (odoc :with-doc)

  ;; Dev dependencies, using with-test so that consumers don't install them (until package is released in opam)
  (ocamlformat
   (and
    (= 0.26.1)
    :with-test))
  (ocaml-lsp-server :with-test)
))
