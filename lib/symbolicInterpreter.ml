open Ast
module Symex = Soteria.Symex.Make (Soteria.Tiny_values.Tiny_solver.Z3_solver)
module Typed = Soteria.Tiny_values.Typed
module Compo_res = Soteria.Symex.Compo_res
module SymbMap = Soteria.Soteria_std.Map.Make (Soteria.Soteria_std.String)
open Symex.Syntax
open Typed.Infix
open Typed.Syntax

type symb_int = Typed.T.sint Typed.t
type env = symb_int SymbMap.t
type qos = { cost : symb_int; latency : symb_int }
type call = { serv_name : string; args : symb_int list; qos : qos }
type hist = call list
type ok_state = { env : env; hist : hist }
type err_state = { msg : string; hist : hist }

let wrap_error result hist =
  let+- err_msg = result in
  { msg = err_msg; hist }

let rec symb_eval_aexpr env = function
  | Int n -> Symex.Result.ok (Typed.int n)
  | Var x -> (
      match SymbMap.find_opt x env with
      | Some v -> Symex.Result.ok v
      | None -> Symex.Result.error (Fmt.str "Variable %s not found" x))
  | NonDet ->
      let* v = Symex.nondet Typed.t_int in
      Symex.Result.ok v
  | AOp (expr1, op, expr2) -> (
      let** v1 = symb_eval_aexpr env expr1 in
      let** v2 = symb_eval_aexpr env expr2 in
      match op with
      | Add -> Symex.Result.ok (v1 +@ v2)
      | Sub -> Symex.Result.ok (v1 -@ v2)
      | Mul -> Symex.Result.ok (v1 *@ v2)
      | Div ->
          if%sat Typed.not (v2 ==@ 0s) then
            let v2 = Typed.cast v2 in
            Symex.Result.ok (v1 /@ v2)
          else Symex.Result.error "Division by zero")

and symb_eval_bexpr env = function
  | Bool b -> Symex.Result.ok (Typed.of_bool b)
  | Not bexpr ->
      let++ v = symb_eval_bexpr env bexpr in
      Typed.not v
  | BOp (bexpr1, op, bexpr2) -> (
      let** v1 = symb_eval_bexpr env bexpr1 in
      let++ v2 = symb_eval_bexpr env bexpr2 in
      match op with And -> v1 &&@ v2 | Or -> v1 ||@ v2)
  | COp (aexpr1, op, aexpr2) -> (
      let** v1 = symb_eval_aexpr env aexpr1 in
      let++ v2 = symb_eval_aexpr env aexpr2 in
      match op with
      | Eq -> v1 ==@ v2
      | Neq -> Typed.not (v1 ==@ v2)
      | Lt -> v1 <@ v2
      | Le -> v1 <=@ v2
      | Gt -> v1 >@ v2
      | Ge -> v1 >=@ v2)

let rec symb_eval_stmt state = function
  | Skip -> Symex.Result.ok state
  | Assign (x, aexpr) ->
      let++ v = wrap_error (symb_eval_aexpr state.env aexpr) state.hist in
      { state with env = SymbMap.add x v state.env }
  | Seq (stmt1, stmt2) ->
      let** state = symb_eval_stmt state stmt1 in
      symb_eval_stmt state stmt2
  | If (cond_bexpr, then_stmt, else_stmt) ->
      let** cond =
        wrap_error (symb_eval_bexpr state.env cond_bexpr) state.hist
      in
      if%sat cond then symb_eval_stmt state then_stmt
      else symb_eval_stmt state else_stmt
  | While (cond_bexpr, body_stmt) ->
      let** cond =
        wrap_error (symb_eval_bexpr state.env cond_bexpr) state.hist
      in
      if%sat cond then
        let** state = symb_eval_stmt state body_stmt in
        symb_eval_stmt state (While (cond_bexpr, body_stmt))
      else Symex.Result.ok state
  | Assume bexpr ->
      let** cond = wrap_error (symb_eval_bexpr state.env bexpr) state.hist in
      let* () = Symex.assume [ cond ] in
      Symex.Result.ok state
  | Assert bexpr ->
      let** cond = wrap_error (symb_eval_bexpr state.env bexpr) state.hist in
      let** () =
        Symex.assert_or_error cond
          {
            msg = Fmt.str "Assertion %a failed" Typed.ppa cond;
            hist = state.hist;
          }
      in
      Symex.Result.ok state
  | Invoke (serv, args) ->
      let** args =
        Symex.Result.map_list args ~f:(fun arg ->
            wrap_error (symb_eval_aexpr state.env arg) state.hist)
      in
      let* cost = Symex.nondet Typed.t_int in
      let* latency = Symex.nondet Typed.t_int in
      let call = { serv_name = serv; args; qos = { cost; latency } } in
      Symex.Result.ok { state with hist = call :: state.hist }
  | AssignInvoke (x, serv, args) ->
      let** args =
        Symex.Result.map_list args ~f:(fun arg ->
            wrap_error (symb_eval_aexpr state.env arg) state.hist)
      in
      let* cost = Symex.nondet Typed.t_int in
      let* latency = Symex.nondet Typed.t_int in
      let* ret_val = Symex.nondet Typed.t_int in
      let call = { serv_name = serv; args; qos = { cost; latency } } in
      Symex.Result.ok
        { env = SymbMap.add x ret_val state.env; hist = call :: state.hist }

let build_symb_process stmt _ _ =
  let initial_state = { env = SymbMap.empty; hist = [] } in
  let final_state =
    let++ final_ok_state = symb_eval_stmt initial_state stmt in
    { final_ok_state with hist = List.rev final_ok_state.hist }
  in
  let final_state =
    let+- final_err_state = final_state in
    { final_err_state with hist = List.rev final_err_state.hist }
  in
  final_state
