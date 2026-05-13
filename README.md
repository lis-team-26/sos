# Symbolic framework for the analysis of service orchestration

## Development setup

- Install [OPAM](https://opam.ocaml.org/) and [Z3](https://github.com/Z3Prover/z3)
- Clone the repository, create a new OPAM switch and setup the environment

```bash
git clone https://github.com/lis-team-26/lis-project
cd lis-project
opam switch create . --deps-only --with-dev-setup
eval $(opam env)
```

- Build the project

```bash
dune build
```

## Example usage

- Run the symbolic interpreter on a sample program

```bash
dune exec -- run test/examples/division_by_zero.example
```
