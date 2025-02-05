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
  (ocaml-base-compiler
   (= 5.3.0))
  menhir
  dune
  base
  stdio
  ppx_jane
  js_of_ocaml
  js_of_ocaml-ppx
  (flow_parser (>= 0.257.0))
  (printbox
   (>= 0.12))
  (printbox-text
   (>= 0.12))
  fmt
  logs
  yojson
  ppx_yojson_conv
  (alcotest :with-test))
 (tags
  (React "Static Analysis")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
