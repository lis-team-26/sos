open Expr.AST
open Contract.AST
open OrchestratorAST
module Symex = Soteria.Symex.Make (Soteria.Tiny_values.Tiny_solver.Z3_solver)
module Typed = Soteria.Tiny_values.Typed
module Compo_res = Soteria.Symex.Compo_res
module StringMap = Soteria.Soteria_std.Map.Make (Soteria.Soteria_std.String)
open Symex.Syntax
open Typed.Infix
open Typed.Syntax

module Key = struct
  type t = Typed.T.any Typed.t list

  let compare = List.compare Typed.compare

  let sem_eq l1 l2 =
    if List.length l1 <> List.length l2 then Typed.v_false
    else
      List.combine l1 l2
      |> List.fold_left
           (fun acc (v1, v2) -> acc &&@ Typed.sem_eq v1 v2)
           Typed.v_true

  let simplify = Symex.map_list ~f:Symex.simplify

  let rec pp fmt l =
    let open Format in
    match l with
    | [] -> fprintf fmt "[]"
    | [ v ] -> fprintf fmt "[%a]" Typed.ppa v
    | v :: vs ->
        fprintf fmt "[%a; " Typed.ppa v;
        pp fmt vs;
        fprintf fmt "]"

  let show l =
    l
    |> List.map (fun v ->
        v |> Typed.Expr.of_value |> Soteria.Tiny_values.Svalue.show)
    |> String.concat "; "

  let distinct _ = Typed.v_false
end

module SymbolicMap = Soteria.Data.S_map.Make (Symex) (Key)

type symb_int = Typed.T.sint Typed.t
type symb_bool = Typed.T.sbool Typed.t
type value = SymbInt of symb_int | SymbBool of symb_bool
type env = value StringMap.t
type invocation = { service : service; actual_args : env; qos : env }
type stack = invocation list

type ok_state = {
  private_env : env;
  public_env : env;
  service_map : service StringMap.t;
  function_map : (fun_type * value SymbolicMap.t) StringMap.t;
  qos_fields : var_type StringMap.t;
  stack : stack;
}

type err_state = { msg : string; stack : stack }

let error msg stack = Symex.Result.error { msg; stack }
let init_error msg = Symex.Result.error (fun stack -> { msg; stack })

let attach_stack stack state =
  let+- err = state in
  err stack

let cast_to_int v =
  match Typed.cast_checked v Typed.t_int with
  | Some i -> Symex.Result.ok i
  | None -> init_error "Expected an integer"

let cast_to_bool v =
  match Typed.cast_checked v Typed.t_bool with
  | Some b -> Symex.Result.ok b
  | None -> init_error "Expected a boolean"

let update_env (s : ok_state) t x v =
  let** v =
    (match t with
      | TInt ->
          let++ v = cast_to_int v in
          SymbInt v
      | TBool ->
          let++ v = cast_to_bool v in
          SymbBool v)
    |> attach_stack s.stack
  in
  Symex.Result.ok { s with private_env = StringMap.add x v s.private_env }

let symb_eval_arithm_op v1 op v2 =
  let** v1 = cast_to_int v1 in
  let** v2 = cast_to_int v2 in
  match op with
  | Add -> Symex.Result.ok (v1 +@ v2)
  | Sub -> Symex.Result.ok (v1 -@ v2)
  | Mul -> Symex.Result.ok (v1 *@ v2)
  | Div ->
      if%sat v2 ==@ Typed.int 0 then init_error "Division by zero"
      else
        let v2 = Typed.cast v2 in
        Symex.Result.ok (v1 /@ v2)
  | _ -> init_error "Type error in arithmetic operation"

let symb_eval_bool_bin_op v1 op v2 =
  let** v1 = cast_to_bool v1 in
  let** v2 = cast_to_bool v2 in
  match op with
  | And -> Symex.Result.ok (v1 &&@ v2)
  | Or -> Symex.Result.ok (v1 ||@ v2)
  | _ -> init_error "Type error in boolean operation"

let symb_eval_bool_un_op op v =
  let++ v = cast_to_bool v in
  match op with Not -> Typed.not v

let symb_eval_cmp_op v1 op v2 =
  let** v1 = cast_to_int v1 in
  let** v2 = cast_to_int v2 in
  match op with
  | Lt -> Symex.Result.ok (v1 <@ v2)
  | Le -> Symex.Result.ok (v1 <=@ v2)
  | Gt -> Symex.Result.ok (v1 >@ v2)
  | Ge -> Symex.Result.ok (v1 >=@ v2)
  | Eq -> Symex.Result.ok (v1 ==@ v2)
  | Neq -> Symex.Result.ok (Typed.not (v1 ==@ v2))
  | _ -> init_error "Type error in comparison operation"

let symb_eval_expr s e =
  let rec helper = function
    | EInt n -> Symex.Result.ok (Typed.int n)
    | EBool b -> Symex.Result.ok (Typed.of_bool b)
    | EVar x -> (
        match StringMap.find_opt x s.private_env with
        | Some (SymbInt i) -> Symex.Result.ok (Typed.cast i)
        | Some (SymbBool b) -> Symex.Result.ok (Typed.cast b)
        | None -> init_error (Fmt.str "Variable %s not found" x))
    | ENonDet ->
        let* v = Symex.nondet Typed.t_int in
        Symex.Result.ok v
    | EApp (f, args) ->
        init_error "Function application not supported in symbolic execution"
    | EUnOp (op, e) ->
        let** v = helper e in
        let++ v = symb_eval_bool_un_op op v in
        Typed.cast v
    | EBinOp (e1, op, e2) -> (
        let** v1 = helper e1 in
        let** v2 = helper e2 in
        match op with
        | Add | Sub | Mul | Div -> symb_eval_arithm_op v1 op v2
        | And | Or -> symb_eval_bool_bin_op v1 op v2
        | Lt | Le | Gt | Ge | Eq | Neq ->
            let++ v = symb_eval_cmp_op v1 op v2 in
            Typed.cast v)
  in
  attach_stack s.stack (helper e)

let symb_eval_aexpr s e =
  let** v = symb_eval_expr s e in
  attach_stack s.stack (cast_to_int v)

let symb_eval_bexpr s e =
  let** v = symb_eval_expr s e in
  attach_stack s.stack (cast_to_bool v)

let rec symb_eval_stmt state = function
  | Skip -> Symex.Result.ok state
  | Declare (t, x, e) ->
      let** v = symb_eval_expr state e in
      update_env state t x v
  | Assign (x, e) ->
      let** v = symb_eval_expr state e in
      let** t =
        match StringMap.find_opt x state.private_env with
        | Some (SymbInt _) -> Symex.Result.ok TInt
        | Some (SymbBool _) -> Symex.Result.ok TBool
        | None -> error (Fmt.str "Variable %s not declared" x) state.stack
      in
      update_env state t x v
  | Seq (s1, s2) ->
      let** state = symb_eval_stmt state s1 in
      symb_eval_stmt state s2
  | If (e, then_s, else_s) ->
      let** b = symb_eval_bexpr state e in
      if%sat b then symb_eval_stmt state then_s else symb_eval_stmt state else_s
  | While (e, s) ->
      let** b = symb_eval_bexpr state e in
      if%sat b then
        let** state = symb_eval_stmt state s in
        symb_eval_stmt state (While (e, s))
      else Symex.Result.ok state
  | Assume e ->
      let** b = symb_eval_bexpr state e in
      let* () = Symex.assume [ b ] in
      Symex.Result.ok state
  | Assert e ->
      let** b = symb_eval_bexpr state e in
      let** () =
        Symex.assert_or_error b
          {
            msg = Fmt.str "Assertion %a failed" Typed.ppa b;
            stack = state.stack;
          }
      in
      Symex.Result.ok state
  | Invoke (f, args) ->
      let** args = Symex.Result.map_list args ~f:(symb_eval_expr state) in
      let args = List.combine serv.params args in
      let** args =
        Symex.Result.fold_list args
          ~f:(fun acc (i, arg) ->
            let++ arg = arg in
            StringMap.add (Fmt.str "arg%d" i) (SymbInt arg) acc)
          state.hist
      in
      let* cost = Symex.nondet Typed.t_int in
      let* latency = Symex.nondet Typed.t_int in
      let call = { serv_name = serv; args; qos = { cost; latency } } in
      Symex.Result.ok { state with stack = call :: state.stack }
  | AssignInvoke (x, serv, args) ->
      let** args =
        Symex.Result.map_list args ~f:(fun arg ->
            wrap_error (symb_eval_aexpr s.env arg) state.stack)
      in
      let* cost = Symex.nondet Typed.t_int in
      let* latency = Symex.nondet Typed.t_int in
      let* ret_val = Symex.nondet Typed.t_int in
      let call = { serv_name = serv; args; qos = { cost; latency } } in
      Symex.Result.ok
        { env = StringMap.add x ret_val state.env; stack = call :: state.stack }

let build_symb_process stmt contract _ =
  let global_env =
    Symex.fold_list contract.globals ~init:StringMap.empty ~f:(fun acc (x, t) ->
        let* v =
          match t with
          | TInt ->
              let* v = Symex.nondet Typed.t_int in
              Symex.return (SymbInt v)
          | TBool ->
              let* v = Symex.nondet Typed.t_bool in
              Symex.return (SymbBool v)
        in
        Symex.return (StringMap.add x v acc))
  in
  let initial_state = { env = global_env; stack = [] } in
  let final_state =
    let++ final_ok_state = symb_eval_stmt initial_state stmt in
    { final_ok_state with stack = List.rev final_ok_state.stack }
  in
  let final_state =
    let+- final_err_state = final_state in
    { final_err_state with stack = List.rev final_err_state.stack }
  in
  final_state
