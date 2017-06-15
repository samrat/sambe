# sambe

`sambe` is a compiler backend. It takes [QBE][qbe] IR as input, and emits
x86-64 assembly as output.

## Usage

You'll need the OCaml package manager `opam` installed. Using `opam`,
you should install `utop`, `extlib` and `ounit`:

```shell
    opam install utop ounit extlib
```

Then, to build `sambe`:

```shell
    make
    ./sambe.native test.ssa output.s
     
    # sambe produces assembly meant to be compiled with NASM.
```

### REPL for development

First run `make sambe.byte` to produce `.cmo` files for all the
modules. Then, run `utop` in the `sambe` directory. This should start
a toplevel with all the modules loaded in.

[qbe]: https://c9x.me/compile/
