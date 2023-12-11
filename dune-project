(lang dune 3.6)

(name interpreter)

(generate_opam_files true)

(source
 (github ianhoffman/interpreter))

(authors "Ian Hoffman")

(maintainers "Ian Hoffman")

(license MIT)

(package
 (name interpreter)
 (synopsis "Interpeter for Brooklyn College CISC 7120X")
 (depends ocaml dune))