(lang dune 2.7.0)
(name diff)
(implicit_transitive_deps false)

(generate_opam_files true)
(source (github CraigFe/diff))
(maintainers "Craig Ferguson <me@craigfe.io>")
(authors "Craig Ferguson <me@craigfe.io>")

(package
 (name diff)
 (synopsis "Tree diffing and edit-distance algorithms")
 (description "Tree diffing and edit-distance algorithms")
 (documentation https://CraigFe.github.io/diff/)
 (depends sink (alcotest :with-test) (fmt :with-test)))
