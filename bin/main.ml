open Utils.Data

let contract_file = ref ""
let orchestrator_file = ref ""
let html_report_file = ref "out/report.html"
let print_contract = ref false
let print_orchestrator = ref false
let print_results = ref false

let usage =
  Fmt.str
    "Usage: run [options] <contract_spec> <orchestrator_code> [-o \
     out/report.html]"

let anon_cnt = ref 0

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
      Arg.Set_string html_report_file,
      "Path to the output HTML report file (default: out/report.html)" );
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

let () =
  Arg.parse specs anon_fun usage;
  let contract_file = !contract_file in
  let orchestrator_file = !orchestrator_file in
  let html_report_file = !html_report_file in
  if
    !anon_cnt = 2 && contract_file <> "" && orchestrator_file <> ""
    && html_report_file <> ""
  then (
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
    let manifestErrors = ManifErrHeuristics.find_manifest_errors [] results in
    HtmlReport.write ~html_report_file ~contract_file ~orchestrator_file
      ~results ~manifest_errors:manifestErrors;
    Fmt.pr "Symbolic execution report written to %s@." html_report_file)
  else Arg.usage specs usage;
  exit 1
