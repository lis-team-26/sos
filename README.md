# 🛟 SOS - Symbolic Orchestration with Soteria

SOS is an OCaml project for symbolic analysis of microservice orchestration. The tool takes a contract specification and an orchestration program as input, performs a preliminary static analysis with type checking, runs symbolic exploration with Soteria, detects manifest errors, and generates an HTML report.

## Table of Contents

- [Project Goals](#project-goals)
- [Features](#features)
- [Contracts](#contracts)
- [Orchestration](#orchestration)
- [Architecture and Project Structure](#architecture-and-project-structure)
- [Prerequisites](#prerequisites)
- [Installation](#installation)
- [Configuration](#configuration)
- [Running](#running)
- [Usage Examples](#usage-examples)
- [Testing](#testing)
- [Authors](#authors)
- [License](#license)

## Project Goals

The goal of the project is to provide a tool for describing, checking, and symbolically exploring orchestration programs with respect to a contract specification. The analysis is capable of identifying successful executions, erroneous executions, unexplored states, unknown results, and finally computes the manifest errors.

## Features

- A contract specification langauge (for modelling gray-box microservices in `.contract` files);
- An orchestration language (for modelling the orchestrator code in `.sos` files);
- Type checking for contracts and orchestrators;
- Different policy specification;
- Monadic interface for stateful symbolic execution;
- Fuel configuration for steps, branching, and loop unrolling;
- Symbolic execution engine based on Soteria;
- Manifest error detection based on Z3;
- Debug flags for pretty-printing ASTs, symbolic results, and manifest errors;
- HTML report generation to inspect the symbolic analysis in depth.

## Contracts

Contracts describe the environment in which an orchestration program is symbolically executed. At the top level, a contract contains:

- `globals`: typed global variables modelling the context available to the orchestrator;
- `functions`: black-box functions modelling operations whose behaviour is unknown to the orchestrator, declared by name and type signature;
- `QoS`: typed quality-of-service relevant fields generated at each microservice invocation;
- `services`: specification of gray-box services callable by the orchestrator;
- `policies`: specification of policies checked against the orchestrator's invocation history.

The supported primitive types are `int` and `bool`. Function types use curried arrow notation, for example `verify: int -> int -> bool`.

Each service specification contains:

- `name`: service identifier used by the orchestrator;
- `params`: typed input parameters;
- `returns`: a single typed return value;
- `precond`: conditions that must hold before the service can be invoked;
- `qos-postcond`: effects and constraints over QoS fields;
- `ok-postcond`: effects and constraints for successful invocations;
- `err-postcond`: optional effects and constraints modelling the non-deterministic service failure.

Postconditions are split into two parts:

- `effects`, which are explicit assignements;
- `constraints`, which add logical requirements that must hold for that execution.

Policies can express:

- aggregate constraint over QoS fields, using `sum`, `avg`, `min`, or `max` (e.g. `sum(latency) < 100`);
- monotonicity over a QoS field, using `ascending` or `descending` (e.g. `ascending(trust)`);
- safety properties on the invocation history (e.g. no more than 3 login attempts), specified by a regular expression on the services name.

Policies are either *ungrouped*, i.e. they are checked over the whole invocation history, or *grouped* over a specified parameter, meaning that it is checked against a filtered invocation history in which the paramater has the same value in each invocation, if present (e.g. no read of the same key after writing it).

## Orchestration

The orchestrator language resembles a minimal C-like language, with dedicated support for gray-box microservice invocation. Operationally, invoking a service triggers the following steps:

- evaluation of the actual parameters;
- assertion of service preconditions;
- generation of the QoS fields and return value;
- application of the service postconditions;
- creation of the "activation record" of the invocation, adding it to the invocation history;
- update of the policy checkers.

If the error postconditions are defined then the invocation can fail, and the orchestrator is capable of detecting it by inspecting the `receipt` object, which is what an service invocation returns and contains:

- the return value of the invocation;
- a boolean value saying whether the invocation succeded or not;
- QoS values of that specific invocation.

Additionally, the language exposes other useful statements like:

- `assume(e)`, which adds the boolean condition `e` in the current execution state;
- `assert(e)`, which verifies if the boolean condition `e` is always satisfied in the current execution state.

To model unknown values (e.g. dynamic input received by the orchestrator), the language allows to introduce typed non-deterministic values using `int?` and `bool?`.

## Architecture and Project Structure

The project is organized as a command-line application composed of several Dune libraries. The main command reads the two input files, builds the ASTs, performs static checks, starts the symbolic runtime, and then produces an HTML report.

The main parts are:

- contract module, for parser, AST, and type checker;
- orchestrator module, for parser, AST, type checker, and symbolic interpreter;
- symbolic, for symbolic data structure needed in static analysis and symbolic execution;
- state monad, to handle stateful symbolic execution;
- policy checker, for policy verification;
- manifest error checker;
- HTML report generator;
- support libraries for data, expressions, automata.

```text
.
├── bin/                         # CLI entry point
├── lib/
│   ├── contract/                # Contracts: parser, AST, type checker
│   ├── orchestrator/            # Orchestrators: parser, AST, checker, interpreter
│   ├── symbolic/                # Runtime and data for symbolic execution
│   ├── policyChecker/           # Policy checks
│   ├── reg2dfa/                 # Regex/NFA/DFA conversion
│   ├── stateMonad/              # State monad support
│   ├── htmlReport/              # HTML report generation
│   └── utils/                   # Shared utilities
├── test/
│   ├── contract_examples/       # Contract examples
│   ├── orchestrator_examples/   # Orchestrator examples
│   └── policy_checker_test/     # Policy checker tests
├── dune-project
└── README.md
```

## Prerequisites

- An OCaml version compatible with the dependencies declared in `dune-project`.
- OPAM installed.
- [Z3](https://github.com/Z3Prover/z3) installed and available on the system.

## Installation

Clone the repository and create a local OPAM switch with the project
dependencies:

```bash
git clone https://github.com/lis-team-26/sos
cd sos
opam switch create . --deps-only --with-dev-setup
eval $(opam env)
```

Build the project:

```bash
dune build
```

## Configuration

The project does not require external configuration files. The main execution options are passed through the command line:

- `-o <dir>`: HTML report output directory, default `out`;
- `-sf <n>`, `--steps-fuel <n>`: symbolic execution step limit;
- `-bf <n>`, `--branching-fuel <n>`: branching limit;
- `-uf <n>`, `--unroll-fuel <n>`: unrolling limit.

Fuel values must be greater than zero. If a fuel value is not provided, the corresponding limit is infinite.

## Running

The public command is `run`:

```bash
dune exec -- run [options] <contract_spec> <orchestrator_code>
```

Where:

- `<contract_spec>` is the path to a `.contract` file;
- `<orchestrator_code>` is the path to a `.sos` file.

Useful debug options:

- `-pc`, `--print-contract`: print the parsed contract;
- `-po`, `--print-orchestrator`: print the parsed orchestrator;
- `-pr`, `--print-results`: print symbolic execution results;
- `-pm`, `--print-manifest-errors`: print detected manifest errors.

## Usage Examples

Run the basic example:

```bash
dune exec -- run test/contract_examples/01_simple_echo.contract test/orchestrator_examples/01_simple_echo.sos
```

Run with a custom report directory and result printing:

```bash
dune exec -- run -o reports/simple_echo -pr -pm \
  test/contract_examples/01_simple_echo.contract \
  test/orchestrator_examples/01_simple_echo.sos
```

Examples are available in `test/contract_examples/` and `test/orchestrator_examples/`. Files with the same numeric prefix are intended to be run together.

## Testing

Run the full Dune test suite:

```bash
dune runtest
```

Run only the example script over the `.contract`/`.sos` examples:

```bash
dune build @example_test
```

Run only the policy checker tests:

```bash
dune build @policy_checker_test
```

To manually check a specific case, run the `run` command on a matching pair of `.contract` and `.sos` files.

## Authors

- Francesco Borri
- Lorenzo Ceccotti
- Tommaso Crocetti
- Federico Fornaciari
- Andrea Malatesti
- Eduardo Meli
- Angelo Passarelli
- Anna Ricci
- Daniele Sampietri
- Lorenzo Maria Siverino
- Natalija Tosic

## License

LGPL-3.0-or-later.
