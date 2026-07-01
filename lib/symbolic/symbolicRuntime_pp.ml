open Format
open SymbolicRuntime
open SymbolicData
open SymbolicData_pp
open Utils.Data
open Utils.Data_pp
open Utils.Loc
open Utils.Loc_pp
open Utils.Types_pp
open Expr.TypedAST_pp
open Contract.TypedAST_pp

type status = Success | Error | Unexplored | Unknown

let pp_status ?(with_cut = false) =
  let pp fmt status =
    let status_str, style =
      match status with
      | Success -> ("🟢 SUCCESS", `Green)
      | Error -> ("🔴 ERROR", `Red)
      | Unexplored -> ("🟠 UNEXPLORED", `Yellow)
      | Unknown -> ("🔵 UNKNOWN", `Blue)
    in
    Fmt.pf fmt "Status: %a" Fmt.(styled style @@ const string status_str) ()
  in
  if with_cut then Fmt.(pp ++ cut) else pp

let pp_error_cause fmt = function
  | DivByZeroError -> Fmt.pf fmt "Division by zero"
  | PrecondError { name } ->
      Fmt.pf fmt "'%s' service precondition not satisfied" name
  | PolicyError (idx, _) -> Fmt.pf fmt "Violated policy #%d" idx
  | AssertionError _ -> Fmt.pf fmt "Assertion failed"

let pp_located_error_cause fmt { it = cause; at = loc } =
  match loc with
  | Loc _ -> Fmt.pf fmt "%a at %a" pp_error_cause cause pp_loc loc
  | EOFLoc -> Fmt.pf fmt "%a at end of file" pp_error_cause cause

let pp_path_condition fmt path_condition =
  pp_list ~sep:Fmt.(const string " ∧" ++ cut) pp_svalue fmt path_condition

let pp_invocation fmt (idx, invocation) =
  let pp fmt (idx, invocation) =
    pp_field ~with_cut:true ~name:"Args"
      (pp_env Fmt.string pp_value)
      fmt invocation.actual_args;
    Fmt.pf fmt "Returned: %a@,Successful: %a@," pp_value invocation.ret_val
      pp_svalue invocation.successful;
    pp_field ~name:"QoS" (pp_env Fmt.string pp_value) fmt invocation.actual_qos
  in
  pp_field
    ~name:(Fmt.str "(#%d) %s" idx invocation.service.name)
    pp fmt (idx, invocation)

let pp_history fmt history =
  pp_list pp_invocation fmt @@ List.mapi (fun idx inv -> (idx + 1, inv)) history

let pp_scope fmt scope =
  let public_scope = List.length scope in
  let env_name_of idx =
    if idx = public_scope then Fmt.str "Public Environment"
    else Fmt.str "Environment #%d" idx
  in
  let pp fmt (idx, env) =
    pp_field ~name:(env_name_of idx) (pp_env Fmt.string pp_value) fmt env
  in
  pp_list pp fmt @@ List.mapi (fun idx env -> (idx + 1, env)) scope

let pp_function_env fmt (f, env) =
  let pp_entry fmt (key_args, value) =
    Fmt.pf fmt "[ %a ] -> %a" (pp_list_inline pp_svalue) key_args pp_value value
  in
  let pp fmt env = pp_list pp_entry fmt env in
  pp_field ~name:f pp fmt
  @@ (env |> SymbolicListMap.syntactic_bindings |> List.of_seq)

let pp_function_envs fmt function_envs =
  pp_list pp_function_env fmt @@ StringMap.bindings function_envs

let pp_fuel fmt fuel =
  Fmt.pf fmt "Steps: %a@,Branching: %a@,Unroll: %a" Fuel.pp fuel.steps Fuel.pp
    fuel.branching Fuel.pp fuel.unroll

let pp_ok_state ?(unexplored = false) fmt (state, path_condition) =
  pp_status ~with_cut:true fmt (if unexplored then Unexplored else Success);
  pp_field ~with_cut:true ~name:"Path condition" pp_path_condition fmt
    path_condition;
  pp_field ~with_cut:true ~name:"Invocation stack" pp_history fmt state.history;
  pp_field ~with_cut:true ~name:"Scope stack" pp_scope fmt state.scope;
  pp_field ~with_cut:true ~name:"Functions environment" pp_function_envs fmt
    state.function_envs;
  pp_field ~name:"Remaining fuel" pp_fuel fmt state.fuel

let pp_err_state fmt (state, path_condition) =
  pp_status ~with_cut:true fmt Error;
  Fmt.pf fmt "Cause: %a@," pp_located_error_cause state.cause;
  pp_field ~with_cut:true ~name:"Path condition" pp_path_condition fmt
    path_condition;
  pp_field ~with_cut:true ~name:"Invocation stack" pp_history fmt
    state.err_history;
  pp_field ~with_cut:true ~name:"Scope stack" pp_scope fmt state.err_scope;
  pp_field ~name:"Functions environment" pp_function_envs fmt
    state.function_envs

let pp_result fmt (idx, state, path_condition) =
  let pp fmt (state, path_condition) =
    match Compo_res.to_result_opt state with
    | Some (Ok state) -> pp_ok_state fmt (state, path_condition)
    | Some (Error (Err state)) -> pp_err_state fmt (state, path_condition)
    | Some (Error (Unexplored state)) ->
        pp_ok_state ~unexplored:true fmt (state, path_condition)
    | None ->
        pp_status ~with_cut:true fmt Unknown;
        pp_section ~name:"Path condition" pp_path_condition fmt path_condition
  in
  pp_section ~name:(Fmt.str "Result #%d" idx) pp fmt (state, path_condition)

let pp_results fmt results =
  results
  |> List.mapi (fun i (s, pc) -> (i + 1, s, pc))
  |> Fmt.vbox (pp_list pp_result) fmt

let pp_manifest_errors = Fmt.vbox @@ pp_list pp_located_error_cause
