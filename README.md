# React-tRace

## Developing

[OCaml 5.2.0](https://ocaml.org/releases/5.2.0) should be installed.

```sh
opam update
opam switch create . ocaml-base-compiler.5.2.0
```

Confirm that the new opam switch is activated using `opam switch` and install the dependencies:

```sh
opam install --deps-only --with-test .
```

Then build with dune:

```sh
dune build
```

You can run React-tRace with dune as well:

```sh
dune exec react_trace -- samples/simple.ml
```

Tests can be run with

```sh
dune runtest
```
