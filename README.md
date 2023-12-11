# Instructions

Steps to run this code:

1. Install [Opam](https://opam.ocaml.org/doc/Install.html).
1. Run `opam init`
1. Install [Dune](https://dune.readthedocs.io/en/stable/quick-start.html)
1. Open your terminal in this directory.
1. Run `dune exec interpreter "<INPUT>"`. For example:

```
> dune exec interpreter "foo = 1;
bar = foo + 1;"
```