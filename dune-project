(lang dune 3.13)

(name grep_cmt)

(generate_opam_files true)

(source
 (github LexiFi/grep_cmt))

(authors "Author Name")

(maintainers "Maintainer Name")

(license MIT)

(package
 (name grep_cmt)
 (synopsis "Advanced structural search tool for OCaml projects")
 (description "The grep_cmt tool provides advanced structural search capabilities for OCaml projects by analyzing compiled module type files (.cmt).")
 (depends ocaml dune)
 (tags
  (topics "OCaml" "developer tools" "code search" "static analysis")))
