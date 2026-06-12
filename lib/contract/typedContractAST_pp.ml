open Format
open TypedContractAST
open ContractAST_pp
open Expr.AST_pp
open Expr.TypedAST_pp
open Utils.Data_pp
open Utils.Types_pp

let rec pp_regex fmt (s2letter, regex) =
  fprintf fmt "[";
  List.iter (fun (s, l) -> fprintf fmt " %s -> %c" s l) s2letter;
  fprintf fmt " ] %s" regex

let pp_policy_type fmt = function
  | QosFieldOp (agg_op, v, cmp_op_val, i) ->
      fprintf fmt "%a(%s) %a %d" pp_aggr_op agg_op v pp_cmp_op cmp_op_val i
  | Regex (l, r) -> pp_regex fmt (l, r)
  | Sort id -> fprintf fmt "sorted(%s)" id

let pp_policy fmt (p, group_by) =
  pp_policy_type fmt p;
  match group_by with None -> () | Some x -> fprintf fmt " group by %s" x

let pp_effct_lhs fmt = function
  | LVar x -> fprintf fmt "%s" x
  | LApp (f, args) -> fprintf fmt "%s(%a)" f pp_typed_expr_list args

let pp_effct fmt (lhs, e) =
  fprintf fmt "%a := %a" pp_effct_lhs lhs pp_typed_expr e

let pp_postcond fmt (es, cs) =
  fprintf fmt "effects:@,@[<v 2>  %a@]@,constraints:@,@[<v 2>  %a@]"
    (pp_list pp_effct) es (pp_list pp_bexpr) cs

let pp_service fmt s =
  fprintf fmt "service %s {@,@[<v 2>  " s.name;
  (* fprintf fmt "params:@,@[<v 2>  %a@]@," (pp_list pp_typed_var) s.params; *)
  fprintf fmt "returns:@,@[<v 2>  %a@]@," pp_typed_var s.returns;
  fprintf fmt "precond:@,@[<v 2>  %a@]@," (pp_list pp_bexpr) s.precond;
  fprintf fmt "qos_postcond:@,@[<v 2>  %a@]@," pp_postcond s.qos_postcond;
  fprintf fmt "ok_postcond:@,@[<v 2>  %a@]@," pp_postcond s.ok_postcond;
  (match s.err_postcond with
  | None -> ()
  | Some err_postcond ->
      fprintf fmt "err_postcond:@,@[<v 2>  %a@]" pp_postcond err_postcond);
  fprintf fmt "@]@,}"

let pp_contract fmt c =
  pp_open_vbox fmt 0;
  fprintf fmt "globals {@,@[<v 2>  %a@]@,}@," (pp_list pp_typed_var) c.globals;
  fprintf fmt "functions {@,@[<v 2>  %a@]@,}@,"
    (pp_list_inline Fmt.string)
    c.functions;
  fprintf fmt "policies {@,@[<v 2>  %a@]@,}@," (pp_list pp_policy) c.policies;
  fprintf fmt "qos {@,@[<v 2>  %a@]@,}@," (pp_list pp_typed_var) c.qos;
  fprintf fmt "services {@,@[<v 2>  %a@]@,}@," (pp_list pp_service) c.services;
  pp_close_box fmt ()
