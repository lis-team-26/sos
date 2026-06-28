open Symbolic.Runtime
open Symbolic.Data
open Utils.Data
open Utils.Types
open Soteria.Tiny_values
open Z3.Arithmetic
open Z3

type error_result = { err_stack : stack; path_condition : symb_bool list }

(* After creating a symbolic value with nondet, get the id of the value*)
let get_var_as_int sv =
  match Typed.kind sv with
  | Var v -> Soteria.Symex.Var.to_int v
  | _ -> failwith "this wasn't a nondet, it was another expression"

let get_vars expr =
  let vars = ref IntSet.empty in
  Typed.iter_vars expr (fun (x, _) ->
      vars := IntSet.add (Soteria.Symex.Var.to_int x) !vars);
  !vars

let get_typed_vars expr =
  let vars = ref IntMap.empty in
  Typed.iter_vars expr (fun (x, t) ->
      vars := IntMap.add (Soteria.Symex.Var.to_int x) t !vars);
  !vars

let group_by_error_cause results =
  let error_results =
    List.filter_map
      (fun (state, path_condition) ->
        match Soteria.Symex.Compo_res.to_result_opt state with
        | None | Some (Ok _) | Some (Error (Unexplored _)) -> None
        | Some (Error (Err { err_stack; cause })) ->
            Some (cause, { err_stack; path_condition }))
      results
  in
  List.fold_left
    (fun error_cause_map (cause, error) ->
      ErrorCauseMap.update cause
        (function None -> Some [ error ] | Some l -> Some (error :: l))
        error_cause_map)
    ErrorCauseMap.empty error_results

let rec translate_expr ctx id_map exp =
  match Svalue.kind exp with
  | Svalue.Var v -> IntMap.find (Soteria.Symex.Var.to_int v) id_map
  | Svalue.Bool true -> Boolean.mk_true ctx
  | Svalue.Bool false -> Boolean.mk_false ctx
  | Svalue.Int i -> Integer.mk_numeral_i ctx (i |> Z.to_int)
  | Svalue.Unop (op, expr) -> (
      match op with Not -> Boolean.mk_not ctx (translate_expr ctx id_map expr))
  | Svalue.Binop (op, expr1, expr2) -> (
      let e1, e2 =
        (translate_expr ctx id_map expr1, translate_expr ctx id_map expr2)
      in
      match op with
      | And -> Boolean.mk_and ctx [ e1; e2 ]
      | Or -> Boolean.mk_or ctx [ e1; e2 ]
      | Eq -> Boolean.mk_eq ctx e1 e2
      | Leq -> Arithmetic.mk_le ctx e1 e2
      | Lt -> Arithmetic.mk_lt ctx e1 e2
      | Plus -> Arithmetic.mk_add ctx [ e1; e2 ]
      | Minus -> Arithmetic.mk_sub ctx [ e1; e2 ]
      | Times -> Arithmetic.mk_mul ctx [ e1; e2 ]
      | Div -> Arithmetic.mk_div ctx e1 e2
      | Rem -> Integer.mk_rem ctx e1 e2
      | Mod -> Integer.mk_mod ctx e1 e2)
  | Svalue.Nop (op, exprList) -> (
      match op with
      | Distinct ->
          Boolean.mk_distinct ctx
            (List.map (translate_expr ctx id_map) exprList))
  | Svalue.Ite (expr1, expr2, expr3) ->
      Boolean.mk_ite ctx
        (translate_expr ctx id_map expr1)
        (translate_expr ctx id_map expr2)
        (translate_expr ctx id_map expr3)

let counter = ref 0

let make_z3_constant ctx typ =
  let symbol = Symbol.mk_int ctx !counter in
  let () = counter := !counter + 1 in
  (match typ with
  | TInt -> Integer.mk_const
  | TBool -> Boolean.mk_const
  | _ -> failwith "receipt can't be a z3 constant")
    ctx symbol

let z3_find globals assumptions error_list =
  let ctx = mk_context [] in
  let forall_vars =
    globals |> List.map snd
    |> List.mapi (fun i t -> (i + 1, make_z3_constant ctx t))
    |> IntMap.of_list
  in
  let forall_vars_count = IntMap.cardinal forall_vars in
  let path_conditions = List.map (fun x -> x.path_condition) error_list in
  let exists_vars path_condition =
    List.fold_left
      (fun set bexpr ->
        get_typed_vars bexpr
        |> IntMap.filter (fun id _ -> id > forall_vars_count)
        |> IntMap.union
             (fun id t1 t2 ->
               if t1 == t2 then Some t1
               else failwith "type mismatch in the same path condition")
             set)
      IntMap.empty path_condition
    |> IntMap.map (fun t ->
        make_z3_constant ctx
          (if t |> Typed.untype_type |> Svalue.is_bool_ty then TBool else TInt))
  in
  let union_vars ev =
    IntMap.union
      (fun _ _ _ ->
        failwith "initial and not initial symb values have the same id")
      ev forall_vars
  in
  let translate_conjunction ctx vars expr_list =
    expr_list
    |> List.map Typed.Expr.of_value
    |> List.map (translate_expr ctx vars)
    |> Boolean.mk_and ctx
  in
  let assumptions =
    Boolean.mk_true ctx (*translate_conjunction ctx forall_vars assumptions*)
  in
  let forall_body =
    path_conditions
    |> List.map (fun pc ->
        let ev = exists_vars pc in
        (ev, translate_conjunction ctx (union_vars ev) pc))
    |> List.map (fun (vars, conjunction) ->
        let bound = vars |> IntMap.to_list |> List.map snd in
        if bound = [] then conjunction
        else
          Quantifier.mk_exists_const ctx bound conjunction (Some 1) [] [] None
            None
          |> Quantifier.expr_of_quantifier)
    |> Boolean.mk_or ctx
    |> Boolean.mk_implies ctx assumptions
  in
  let forall_bound = forall_vars |> IntMap.to_list |> List.map snd in
  let final_formula =
    if forall_bound = [] then forall_body
    else
      Quantifier.mk_forall_const ctx forall_bound forall_body (Some 1) [] []
        None None
      |> Quantifier.expr_of_quantifier
    (*in
  let () = Printf.printf "manifest condition: %s\n" (Quantifier.to_string final_formula)*)
  in
  let solver = Solver.mk_solver ctx None in
  Solver.add solver [ final_formula ];
  match Solver.check solver [] with SATISFIABLE -> true | _ -> false

let find_manifest_errors globals assumptions results =
  group_by_error_cause results
  |> ErrorCauseMap.bindings
  |> List.filter_map (fun (cause, error_list) ->
      if z3_find globals assumptions error_list then Some cause else None)
