# Repository Guidelines

## Project Structure & Module Organization

This is an OCaml/Dune project for symbolic analysis of service orchestration. Source code lives under `lib/`, split into libraries such as `contract`, `orchestrator`, `expr`, `symbolic`, `policyChecker`, `reg2dfa`, `stateMonad`, and `utils`. The CLI entry point is `bin/main.ml`, exposed as the public executable `run`. Example inputs live in `test/contract_examples/` (`*.contract`) and `test/orchestrator_examples/` (`*.sos`). Policy checker tests live in `test/policy_checker_test/`. Generated build output and local switches stay in `_build/` and `_opam/`.

## Build, Test, and Development Commands

- `opam switch create . --deps-only --with-dev-setup`: create the local switch with development tools.
- `eval $(opam env)`: load the local switch into the shell.
- `dune build`: compile all libraries, Menhir parsers, ocamllex lexers, and the CLI.
- `dune exec -- run test/contract_examples/01_simple_echo.contract test/orchestrator_examples/01_simple_echo.sos`: run the simple echo sample.
- `dune build @example_test`: run only the example bash script over the contract/orchestrator samples.
- `dune build @policy_checker_test`: run only the policy checker tests.
- `dune runtest`: run both the example script and the policy checker tests.
- `dune fmt`: format OCaml files with the project `ocamlformat` settings.

Z3 is required at runtime by the symbolic stack; install it outside OPAM if your platform package manager provides it.

## Coding Style & Naming Conventions

Use OCaml formatted by `ocamlformat` version `0.29.0` with the default profile from `.ocamlformat`. Keep module filenames aligned with existing camelCase/lowercase patterns, such as `typeCheckExpr.ml`, `orchestratorInterpreter.ml`, and `dataUtils.ml`. Public interfaces belong in matching `.mli` files for reusable APIs. Parser and lexer files use `*Parser.mly` and `*Lexer.mll`.

## Testing Guidelines

Current tests are split between example-driven integration checks and policy checker unit tests. Add contract samples under `test/contract_examples/` and matching orchestrator programs under `test/orchestrator_examples/`, using descriptive lowercase names such as `division_by_zero.sos`; verify the example suite with `dune build @example_test`. Add policy checker tests under `test/policy_checker_test/` and verify them with `dune build @policy_checker_test`. Use `dune runtest` before handing off changes that should pass both suites.

## Commit & Pull Request Guidelines

Recent commits use short, imperative summaries such as `fixed incorrect line numbers` and `finished the group by`. Keep messages concise and focused on one change. Pull requests should describe the behavior changed, list commands run (`dune build`, `dune runtest`, sample executions), and link related issues. Include sample input/output when changing parser, type checker, interpreter, or heuristic behavior.

## Agent-Specific Instructions

Before editing, check `git status --short` and preserve unrelated local changes. Prefer Dune and OPAM workflows, and avoid touching `_build/`, `_opam/`, or generated `sos.opam` unless the change originates in `dune-project`.
