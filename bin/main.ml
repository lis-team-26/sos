open Utils.Data

let contract_file = ref ""
let orchestrator_file = ref ""
let report_dir = ref "out"
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

let specs =
  [
    ( "contract_spec",
      Arg.String anon_fun,
      "Path to the contract specification file" );
    ( "orchestrator_code",
      Arg.String anon_fun,
      "Path to the orchestrator code file" );
    ( "-o",
      Arg.Set_string report_dir,
      "Path to the output report directory (default: out)" );
    ("-pc", Arg.Set print_contract, "Print the parsed contract before analysis");
    ( "-po",
      Arg.Set print_orchestrator,
      "Print the parsed orchestrator before analysis" );
    ( "-pr",
      Arg.Set print_results,
      "Print the symbolic execution results after analysis" );
    ( "--print-contract",
      Arg.Set print_contract,
      "Print the parsed contract before analysis" );
    ( "--print-orchestrator",
      Arg.Set print_orchestrator,
      "Print the parsed orchestrator before analysis" );
    ( "--print-results",
      Arg.Set print_results,
      "Print the symbolic execution results after analysis" );
  ]

let validate_args () =
  if
    !anon_cnt != 2
    || String.equal !contract_file ""
    || String.equal !orchestrator_file ""
    || String.equal !report_dir ""
  then (
    Arg.usage specs usage;
    exit 1)

let () =
  Arg.parse specs anon_fun usage;
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
  let results =
    Orchestrator.symb_run
      (typed_contract_ast, typed_orchestrator_ast)
      ~mode:Soteria.Symex.Approx.OX
  in
  if !print_results then
    Fmt.pr "@[Results:@,@[<v 2>  %a@]@]@." Symbolic.Runtime_pp.pp_results
      results;
  let manifest_errors =
    ManifestErrorHeuristics.find_manifest_errors typed_contract_ast.globals
      results
  in
  HtmlReport.write ~report_dir ~contract_file ~orchestrator_file ~results
    ~manifest_errors;
  Fmt.pr "Symbolic execution report written to folder '%s'@." report_dir
