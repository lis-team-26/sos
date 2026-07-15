#!/usr/bin/env bash
set -euo pipefail

if [[ $# -gt 0 ]]; then
  if [[ "$1" = /* ]]; then
    run_cmd=("$1")
  else
    run_cmd=("$(pwd)/$1")
  fi
else
  run_cmd=(dune exec -- run)
fi

if [[ -d "test/contract_examples" && -d "test/orchestrator_examples" ]]; then
  contract_dir="test/contract_examples"
  orchestrator_dir="test/orchestrator_examples"
elif [[ -d "contract_examples" && -d "orchestrator_examples" ]]; then
  cd ..
  contract_dir="test/contract_examples"
  orchestrator_dir="test/orchestrator_examples"
elif [[ -d "../../../test/contract_examples" && -d "../../../test/orchestrator_examples" ]]; then
  cd ../../..
  contract_dir="test/contract_examples"
  orchestrator_dir="test/orchestrator_examples"
else
  echo "Could not find contract_examples and orchestrator_examples directories" >&2
  exit 1
fi

output_dir="out"

mkdir -p "$output_dir"
shopt -s nullglob

successes=()
failures=()

for orchestrator in "$orchestrator_dir"/*.sos; do
  name="$(basename "$orchestrator" .sos)"

  if [[ ! "$name" =~ ^([0-9]+)_ ]]; then
    echo "Skipping $name: filename does not start with a numeric prefix" >&2
    continue
  fi

  prefix="${BASH_REMATCH[1]}"
  matching_contracts=("$contract_dir"/"$prefix"_*.contract)

  if [[ ${#matching_contracts[@]} -eq 0 ]]; then
    echo "Skipping $name: no contract found with prefix $prefix" >&2
    continue
  fi

  if [[ ${#matching_contracts[@]} -gt 1 ]]; then
    echo "Skipping $name: multiple contracts found with prefix $prefix" >&2
    printf '  %s\n' "${matching_contracts[@]}" >&2
    continue
  fi

  contract="${matching_contracts[0]}"

  echo "Running $name with $(basename "$contract")"
  if "${run_cmd[@]}" "$contract" "$orchestrator" -o "$output_dir/$name"; then
    successes+=("$name")
  else
    echo "Failed $name" >&2
    failures+=("$name")
  fi
done

echo
echo "Summary:"

echo "  Successful examples (${#successes[@]}):"
if [[ ${#successes[@]} -gt 0 ]]; then
  printf '    %s\n' "${successes[@]}"
else
  echo "    none"
fi

echo "  Failed examples (${#failures[@]}):"
if [[ ${#failures[@]} -gt 0 ]]; then
  printf '    %s\n' "${failures[@]}"
else
  echo "    none"
fi
