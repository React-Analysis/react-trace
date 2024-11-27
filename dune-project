(lang dune 3.15)

(using menhir 3.0)

(name react_trace)

(generate_opam_files true)

(source
 (github React-Analysis/ReacttRace))

(authors "Jay Lee")

(maintainers "Jay Lee")

(license LICENSE)

; (documentation https://url/to/documentation)

(package
 (name react_trace)
 (synopsis "React Analyzer")
 (description "ReacttRace is a tool for analyzing React programs.")
 (depends
  ocaml
  dune
  (ocaml-base-compiler
   (= 5.2.0))
  core
  js_of_ocaml
  js_of_ocaml-ppx
  flow_parser
  fmt
  logs
  menhir
  ppx_jane
  printbox
  printbox-text
  stdio
  (alcotest :with-test))
 (tags
  (React "Static Analysis")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
