# React-tRace

## Developing

We use the experimental effect syntax, so [OCaml 5.1.1 with effect syntax support](https://ocaml.org/p/ocaml-variants/5.1.1%2Beffect-syntax) should be installed.

```sh
opam update
opam switch create . 5.1.1+effect-syntax
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

Note that the LSP server does not support OCaml 5.1.1 with effect syntax, so it is convenient to run `dune build --watch` while developing.

Formatting is available with [the fork that supports the effect syntax](https://github.com/Zeta611/ocamlformat/tree/effect-syntax).
Pin the repo and install it with opam.
