# 🛤️ React-tRace

[![Builds, tests & co](https://github.com/React-Analysis/react-trace/actions/workflows/ci.yml/badge.svg)](https://github.com/React-Analysis/react-trace/actions/workflows/ci.yml)

React-tRace is a React hooks reference interpreter based on a formal semantics[^1].
It focuses on detecting inefficient re-renders.

[^1]: The Coq formalization is currently being developed [here](https://github.com/React-Analysis/formal-react-trace)

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
