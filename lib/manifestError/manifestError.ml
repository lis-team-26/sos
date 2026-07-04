module E = Expr.AST
module Smt = Simple_smt
open Symbolic.Data
module Svalue = Soteria.Tiny_values.Svalue
open Symbolic.Runtime
open Utils.Data
open Utils.Loc
open Utils.Types

(** Translate a source-language binary operator to the matching SMT operator. *)
let translate_expr_bin_op op e1 e2 =
  match op with
  | E.Eq -> Smt.eq e1 e2
  | E.Le -> Smt.num_leq e1 e2
  | E.Lt -> Smt.num_lt e1 e2
  | E.Neq -> Smt.bool_not @@ Smt.eq e1 e2
  | E.Ge -> Smt.num_geq e1 e2
  | E.Gt -> Smt.num_gt e1 e2
  | E.Add -> Smt.num_add e1 e2
  | E.Sub -> Smt.num_sub e1 e2
  | E.Mul -> Smt.num_mul e1 e2
  | E.Div -> Smt.num_div e1 e2
  | E.And -> Smt.bool_and e1 e2
  | E.Or -> Smt.bool_or e1 e2

(** Translate a Soteria symbolic binary operator to the matching SMT operator.
*)
let translate_soteria_bin_op = function
  | Svalue.Binop.And -> Smt.bool_and
  | Svalue.Binop.Or -> Smt.bool_or
  | Svalue.Binop.Eq -> Smt.eq
  | Svalue.Binop.Leq -> Smt.num_leq
  | Svalue.Binop.Lt -> Smt.num_lt
  | Svalue.Binop.Plus -> Smt.num_add
  | Svalue.Binop.Minus -> Smt.num_sub
  | Svalue.Binop.Times -> Smt.num_mul
  | Svalue.Binop.Div -> Smt.num_div
  | Svalue.Binop.Rem -> Smt.num_rem
  | Svalue.Binop.Mod -> Smt.num_mod

(** Translate an assumption over global variables to an SMT expression. *)
let rec translate_expr env e =
  match e.it with
  | E.EInt i -> Smt.int_k i
  | E.EVar v -> StringMap.find v env
  | E.EBool true -> Smt.bool_k true
  | E.EBool false -> Smt.bool_k false
  | E.EUnOp (Not, e) -> Smt.bool_not (translate_expr env e)
  | E.EBinOp (e1, op, e2) ->
      translate_expr_bin_op op (translate_expr env e1) (translate_expr env e2)
  | _ -> failwith "This is not an assumption on global variables"

(** Translate a Soteria symbolic value to an SMT expression. *)
let rec translate_soteria_expr env e =
  match Svalue.kind e with
  | Svalue.Var v -> IntMap.find (Soteria.Symex.Var.to_int v) env
  | Svalue.Bool true -> Smt.bool_k true
  | Svalue.Bool false -> Smt.bool_k false
  | Svalue.Int i -> Smt.int_zk i
  | Svalue.Unop (Not, e) -> Smt.bool_not (translate_soteria_expr env e)
  | Svalue.Binop (op, e1, e2) ->
      (translate_soteria_bin_op op)
        (translate_soteria_expr env e1)
        (translate_soteria_expr env e2)
  | Svalue.Nop (Distinct, es) ->
      es |> List.map (translate_soteria_expr env) |> Smt.distinct
  | Svalue.Ite (e1, e2, e3) ->
      Smt.ite
        (translate_soteria_expr env e1)
        (translate_soteria_expr env e2)
        (translate_soteria_expr env e3)

(** Converts an expression type to the corresponding SMT sort. *)
let smt_sort_of_expr_type = function
  | TInt -> Smt.t_int
  | TBool -> Smt.t_bool
  | TReceipt _ -> failwith "Receipts can't be an smt constant"

(** Converts a Soteria type to the corresponding expression type. *)
let expr_type_of_soteria_type t =
  if t |> Typed.untype_type |> Svalue.is_bool_ty then TBool else TInt

(** Add every non-global typed symbolic variable in [e] to [vars].

    If [var_id] is greater than [forall_vars_count], it is considered an
    existential variable because it was introduced after the global variables
    generation, and thus it is local.*)
let add_existential_vars ~forall_vars_count vars e =
  let vars = ref vars in
  let add_existential_var (var_name, var_type) =
    let var_id = Soteria.Symex.Var.to_int var_name in
    if var_id > forall_vars_count then
      vars :=
        !vars
        |> IntMap.update var_id (function
          | None -> Some var_type
          | Some old_type when Typed.equal_ty var_type old_type -> Some old_type
          | Some _ ->
              Fmt.failwith
                "Type mismatch for symbolic variable %d in the same path \
                 condition"
                var_id)
  in
  Typed.iter_vars e add_existential_var;
  !vars

(** Group symbolic-execution failures by their concrete error cause. *)
let group_by_error_cause results =
  results
  |> List.filter_map (fun (state, path_condition) ->
      match Soteria.Symex.Compo_res.to_result_opt state with
      | None | Some (Ok _) | Some (Error (Unexplored _)) -> None
      | Some (Error (Err { cause })) -> Some (cause, path_condition))
  |> List.fold_left
       (fun map (cause, error) ->
         ErrorCauseMap.update cause
           (function None -> Some [ error ] | Some l -> Some (error :: l))
           map)
       ErrorCauseMap.empty
  |> ErrorCauseMap.bindings

(** Create a fresh SMT constant descriptor using [next_id] as a local counter.
*)
let make_smt_constant next_id t =
  let id = !next_id in
  incr next_id;
  let name = Fmt.str "manifest_%d" id in
  (Smt.symbol name, smt_sort_of_expr_type t)

(** Converts a pair [(var_symbol, var_type)] to an SMT quantified-variable
    binder. *)
let binder (var_symbol, var_type) = Smt.list [ var_symbol; var_type ]

(** Build an existential quantifier. If [bound_vars] is empty, returns [body].
*)
let exists_const bound_vars body =
  match bound_vars with
  | [] -> body
  | _ -> Smt.app_ "exists" [ bound_vars |> List.map binder |> Smt.list; body ]

(** Build a universal quantifier. If [bound_vars] is empty, returns [body]. *)
let forall_const bound_vars body =
  match bound_vars with
  | [] -> body
  | _ -> Smt.app_ "forall" [ bound_vars |> List.map binder |> Smt.list; body ]

(** Collect and allocate SMT variables for the existential variables that occur
    in one symbolic path condition. *)
let local_vars_of ~forall_vars_count ~make_smt_constant path_condition =
  path_condition
  |> List.fold_left (add_existential_vars ~forall_vars_count) IntMap.empty
  |> IntMap.map (expr_type_of_soteria_type >> make_smt_constant)

(** Build the SMT formula that characterizes manifest errors. It takes as input
    a list of path conditions, which should all lead to the same error cause.
    The formula has shape:

    ∀ globals. [globals_assumptions] =>
    - ∃ path-local variables. [path_condition[1]] ∨
    - ∃ path-local variables. [path_condition[2]] ∨
    - ...
    - ∃ path-local variables. [path_condition[n]]

    Each error path may introduce different symbolic variables, so each path
    gets its own existential quantifier before all paths are disjoined.*)
let manifest_formula ~global_vars ~globals_assumptions path_condition_list =
  let make_smt_constant = make_smt_constant (ref 0) in
  let forall_vars, forall_vars_name_env =
    global_vars
    |> List.mapi (fun var_id (var_name, var_type) ->
        let var_symbol, var_type = make_smt_constant var_type in
        (var_id + 1, var_name, var_symbol, var_type))
    |> List.fold_left
         (fun (forall_vars, forall_vars_name_env)
              (var_id, var_name, var_symbol, var_type) ->
           ( IntMap.add var_id (var_symbol, var_type) forall_vars,
             StringMap.add var_name var_symbol forall_vars_name_env ))
         (IntMap.empty, StringMap.empty)
  in
  let forall_symbols_env = IntMap.map fst forall_vars in
  let forall_bound_vars = forall_vars |> IntMap.to_list |> List.map snd in
  let forall_vars_count = IntMap.cardinal forall_vars in
  (* Merge global and existential SMT symbols before translating a path. *)
  let merge_symbols_envs exists_symbols_env =
    IntMap.union
      (fun var_id _ _ ->
        Fmt.failwith
          "Global and path-local symbolic values have the same id: %d" var_id)
      exists_symbols_env forall_symbols_env
  in
  (* Translate the user-provided global assumptions as a conjunction. *)
  let translated_assumptions =
    globals_assumptions
    |> List.map (translate_expr forall_vars_name_env)
    |> Smt.bool_ands
  in
  path_condition_list
  |> List.map (fun path_condition ->
      let exists_vars =
        local_vars_of ~forall_vars_count ~make_smt_constant path_condition
      in
      let symbols_env = exists_vars |> IntMap.map fst |> merge_symbols_envs in
      let exists_body =
        path_condition
        |> List.map Typed.Expr.of_value
        |> List.map (translate_soteria_expr symbols_env)
        |> Smt.bool_ands
      in
      let bound_vars = exists_vars |> IntMap.to_list |> List.map snd in
      exists_const bound_vars exists_body)
  |> Smt.bool_ors
  |> Smt.bool_implies translated_assumptions
  |> forall_const forall_bound_vars

(** Asserts that [formula] always holds using Z3, by verifying that its negation
    is unsatisfiable. *)
let z3_assert formula =
  let solver = Smt.new_solver Smt.z3 in
  formula |> Smt.bool_not |> Smt.assume |> Smt.ack_command solver;
  match Smt.check solver with
  | Smt.Unsat -> true
  | Smt.Sat | Smt.Unknown -> false

(** Find all manifest error causes in symbolic-execution results. *)
let find_manifest_errors ~global_vars ~globals_assumptions results =
  results |> group_by_error_cause
  |> List.filter_map (fun (cause, path_condition_list) ->
      let formula =
        manifest_formula ~global_vars ~globals_assumptions path_condition_list
      in
      if z3_assert formula then Some cause else None)
