open Soteria.Stats
open Soteria.Symex.StatKeys
open Symbolic.Runtime
open Utils.Data

let contract_file = ref ""
let orchestrator_file = ref ""
let report_dir = ref "out"
let steps_fuel = ref Fuel.Infinite
let branching_fuel = ref Fuel.Infinite
let unroll_fuel = ref Fuel.Infinite
let print_contract = ref false
let print_orchestrator = ref false
let print_results = ref false
let anon_cnt = ref 0

let usage =
  Fmt.str "Usage: run [options] <contract_spec> <orchestrator_code> [-o out]"

let anon_fun anon_arg =
  (match !anon_cnt with
  | 0 -> contract_file := anon_arg
  | 1 -> orchestrator_file := anon_arg
  | _ -> ());
  incr anon_cnt

let set_fuel fuel_ref fuel_value = fuel_ref := Fuel.Finite fuel_value

let specs =
  [
    ( [ "contract_spec" ],
      Arg.String anon_fun,
      "Path to the contract specification file" );
    ( [ "orchestrator_code" ],
      Arg.String anon_fun,
      "Path to the orchestrator code file" );
    ( [ "-o" ],
      Arg.Set_string report_dir,
      "Path to the output report directory (default: out)" );
    ( [ "-sf"; "--steps-fuel" ],
      Arg.Int (set_fuel steps_fuel),
      "Steps fuel (default: infinite), must be greater than 0" );
    ( [ "-bf"; "--branching-fuel" ],
      Arg.Int (set_fuel branching_fuel),
      "Branching fuel (default: infinite), must be greater than 0" );
    ( [ "-uf"; "--unroll-fuel" ],
      Arg.Int (set_fuel unroll_fuel),
      "Unroll fuel (default: infinite), must be greater than 0" );
    ( [ "-pc"; "--print-contract" ],
      Arg.Set print_contract,
      "Print the parsed contract before analysis" );
    ( [ "-po"; "--print-orchestrator" ],
      Arg.Set print_orchestrator,
      "Print the parsed orchestrator before analysis" );
    ( [ "-pr"; "--print-results" ],
      Arg.Set print_results,
      "Print the symbolic execution results after analysis" );
  ]

let flatten_specs specs =
  List.fold_left
    (fun acc (flag_list, spec, str) ->
      List.fold_left (fun acc flag -> (flag, spec, str) :: acc) acc flag_list)
    [] specs

let simplify_specs specs =
  List.map
    (fun (flag_list, spec, str) ->
      (String.concat "," flag_list, spec, ": " ^ str))
    specs

let not_valid_fuel fuel_ref =
  match !fuel_ref with Fuel.Finite n when n <= 0 -> true | _ -> false

let validate_args () =
  if
    !anon_cnt != 2
    || String.equal !contract_file ""
    || String.equal !orchestrator_file ""
    || String.equal !report_dir ""
    || not_valid_fuel steps_fuel
    || not_valid_fuel branching_fuel
    || not_valid_fuel unroll_fuel
  then (
    Arg.usage (simplify_specs specs) usage;
    exit 1)

let () =
  Arg.parse (flatten_specs specs) anon_fun usage;
  validate_args ();
  let contract_file = !contract_file in
  let orchestrator_file = !orchestrator_file in
  let report_dir = !report_dir in
  let contract_ast = Contract.parse contract_file in
  let orchestrator_ast = Orchestrator.parse orchestrator_file in
  Contract.validate_contract contract_ast;
  let typed_contract_ast = Contract.type_check contract_ast |> get_or_fail in
  let typed_orchestrator_ast =
    Orchestrator.type_check contract_ast orchestrator_ast |> get_or_fail
  in
  if !print_contract then
    Fmt.pr "@[Contract specificaton:@,@[<v 2>  %a@]@]@."
      Contract.AST_pp.pp_contract contract_ast;
  if !print_orchestrator then
    Fmt.pr "@[Orchestrator code:@,@[<v 2>  %a@]@]@."
      Orchestrator.AST_pp.pp_program orchestrator_ast;
  Fmt.pr "@,Symbolic executing...@,%a" Fmt.flush ();
  let { res = results; stats } =
    let steps_fuel = !steps_fuel in
    let branching_fuel = !branching_fuel in
    let unroll_fuel = !unroll_fuel in
    Orchestrator.run ~steps_fuel ~branching_fuel ~unroll_fuel typed_contract_ast
      typed_orchestrator_ast
  in
  if !print_results then
    Fmt.pr "@[Results:@,@[<v 2>  %a@]@]@." Symbolic.Runtime_pp.pp_results
      results;
  let manifest_errors =
    ManifestError.find_manifest_errors typed_contract_ast.globals
      typed_contract_ast.globals_assumptions results
  in
  HtmlReport.write ~report_dir ~contract_file ~orchestrator_file ~results
    ~manifest_errors ~stats;
  Fmt.pr "Symbolic execution report written to folder '%s'@." report_dir
