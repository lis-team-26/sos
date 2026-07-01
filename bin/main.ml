open Soteria.Stats
open Symbolic.Runtime
open Utils.Data_pp
open Utils.Loc
open Utils.Loc_pp
open Utils.Result

let contract_file = ref ""
let orchestrator_file = ref ""
let report_dir = ref "out"
let steps_fuel = ref Fuel.Infinite
let branching_fuel = ref Fuel.Infinite
let unroll_fuel = ref Fuel.Infinite
let print_contract_flag = ref false
let print_orchestrator_flag = ref false
let print_results_flag = ref false
let print_manifest_errors_flag = ref false
let anon_cnt = ref 0
let set_fuel fuel_ref fuel_value = fuel_ref := Fuel.Finite fuel_value

let fuel_is_valid fuel =
  match fuel with
  | Fuel.Infinite -> true
  | Fuel.Finite n when n > 0 -> true
  | _ -> false

let pp_parser_error = Fmt.string

let pp_type_check_error fmt error =
  match error.at with
  | Loc l -> Fmt.pf fmt "%s in %a" error.it pp_loc error.at
  | EOFLoc -> Fmt.pf fmt "%s at the end of the execution" error.it

let pp_static_error pp =
  Fmt.(styled `Red (const string "❌ [Static error] ") ++ pp)

let pp_runtime_error fmt msg =
  Fmt.(styled `Red (const string "❌ [Runtime error] ") ++ const string msg)
    fmt ()

let pp_sep fmt () = Fmt.pf fmt "--------------------------------%a" Fmt.cut ()

let pp_counters =
  let pp_counter fmt (counter, descr) = Fmt.pf fmt "%d %s" counter descr in
  let pp_manifest_errors fmt counter =
    let status, style =
      match counter with 0 -> ("👌 ", `Green) | n -> ("❗ ", `Red)
    in
    Fmt.(const string status ++ Fmt.styled style pp_counter)
      fmt
      (counter, "manifest errors")
  in
  let pp fmt ((ok, err, unexplored, unknown), manifest) =
    Fmt.pf fmt "🟢 %a@,🔴 %a@,🟠 %a@,🔵 %a@,%a"
      (Fmt.styled `Green pp_counter)
      (ok, "successful executions")
      (Fmt.styled `Red pp_counter)
      (err, "errorenous executions")
      (Fmt.styled `Yellow pp_counter)
      (unexplored, "unexplored executions")
      (Fmt.styled `Blue pp_counter)
      (unknown, "unknown results")
      pp_manifest_errors manifest
  in
  Fmt.(vbox (pp_field ~name:"Found" pp ++ cut))

let print_contract_ast ast =
  Fmt.vbox
    (pp_field ~with_cut:true ~name:"Contract specification"
       Contract.AST_pp.pp_contract)
    Fmt.stdout ast;
  pp_sep Fmt.stdout ()

let print_orchestrator_ast ast =
  Fmt.vbox
    (pp_field ~with_cut:true ~name:"Orchestrator code"
       Orchestrator.AST_pp.pp_program)
    Fmt.stdout ast;
  pp_sep Fmt.stdout ()

let print_results results =
  Fmt.vbox
    (pp_section ~with_cut:true ~name:"Symbolic execution results"
       Symbolic.Runtime_pp.pp_results)
    Fmt.stdout results;
  pp_sep Fmt.stdout ()

let print_manifest_errors manifest_errors =
  Fmt.vbox
    (pp_section ~with_cut:true ~name:"Manifest errors"
       Symbolic.Runtime_pp.pp_manifest_errors)
    Fmt.stdout manifest_errors;
  pp_sep Fmt.stdout ()

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
      Arg.Set print_contract_flag,
      "Print the parsed contract before analysis" );
    ( [ "-po"; "--print-orchestrator" ],
      Arg.Set print_orchestrator_flag,
      "Print the parsed orchestrator before analysis" );
    ( [ "-pr"; "--print-results" ],
      Arg.Set print_results_flag,
      "Print the symbolic execution results after analysis" );
    ( [ "-pm"; "--print-manifest-errors" ],
      Arg.Set print_manifest_errors_flag,
      "Print the manifest errors after analysis" );
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

let validate_args () =
  if
    !anon_cnt != 2
    || String.equal !contract_file ""
    || String.equal !orchestrator_file ""
    || String.equal !report_dir ""
    || (not (fuel_is_valid !steps_fuel))
    || (not (fuel_is_valid !branching_fuel))
    || not (fuel_is_valid !unroll_fuel)
  then (
    Arg.usage (simplify_specs specs) usage;
    exit 1)

let () =
  Fmt_tty.setup_std_outputs ();
  Arg.parse (flatten_specs specs) anon_fun usage;
  validate_args ();
  let contract_ast =
    Contract.parse !contract_file
    |> get_or_fail ~pp:(pp_static_error pp_parser_error)
  in
  let orchestrator_ast =
    Orchestrator.parse !orchestrator_file
    |> get_or_fail ~pp:(pp_static_error pp_parser_error)
  in
  let typed_contract_ast =
    Contract.type_check contract_ast
    |> get_or_fail ~pp:(pp_static_error pp_type_check_error)
  in
  let typed_orchestrator_ast =
    Orchestrator.type_check contract_ast orchestrator_ast
    |> get_or_fail ~pp:(pp_static_error pp_type_check_error)
  in
  if !print_contract_flag then print_contract_ast contract_ast;
  if !print_orchestrator_flag then print_orchestrator_ast orchestrator_ast;
  Fmt.vbox
    Fmt.(const string "👀 Running symbolic execution..." ++ cut ++ flush)
    Fmt.stdout ();
  let results, manifest_errors, stats =
    try
      let { res = results; stats } =
        let fuel =
          {
            steps = !steps_fuel;
            branching = !branching_fuel;
            unroll = !unroll_fuel;
          }
        in
        Orchestrator.run ~fuel typed_contract_ast typed_orchestrator_ast
      in
      let manifest_errors =
        ManifestError.find_manifest_errors
          ~globals:(List.map drop_loc contract_ast.globals)
          ~assumptions:contract_ast.globals_assumptions results
      in
      (results, manifest_errors, stats)
    with exn ->
      Fmt.epr "%a@." pp_runtime_error (Printexc.to_string exn);
      exit 1
  in
  if !print_results_flag then print_results results;
  if !print_manifest_errors_flag then print_manifest_errors manifest_errors;
  let counters =
    ( List.fold_left
        (fun (ok, err, unexplored, unknown) (state, _) ->
          match Compo_res.to_result_opt state with
          | Some (Ok _) -> (ok + 1, err, unexplored, unknown)
          | Some (Error (Err _)) -> (ok, err + 1, unexplored, unknown)
          | Some (Error (Unexplored _)) -> (ok, err, unexplored + 1, unknown)
          | None -> (ok, err, unexplored, unknown + 1))
        (0, 0, 0, 0) results,
      List.length manifest_errors )
  in
  HtmlReport.write ~contract_file:!contract_file
    ~orchestrator_file:!orchestrator_file ~results ~manifest_errors ~stats
    !report_dir;
  pp_counters Fmt.stdout counters;
  Fmt.pr "✅ Symbolic execution report written to folder '%s'. Done@."
    !report_dir
