open Format
open ContractAST
open Expr.PP

let rec pp_fun_type fmt = function
  | ContractAST.TFun (args, ret) ->
      fprintf fmt "(";
      pp_type_list fmt args;
      fprintf fmt ") -> %a" pp_var_type ret

and pp_type_list fmt = function
  | [] -> ()
  | [ t ] -> pp_var_type fmt t
  | t :: ts ->
      pp_var_type fmt t;
      fprintf fmt ", ";
      pp_type_list fmt ts

let pp_aggr_op fmt = function
  | ContractAST.Sum -> fprintf fmt "sum"
  | ContractAST.Avg -> fprintf fmt "avg"
  | ContractAST.Min -> fprintf fmt "min"
  | ContractAST.Max -> fprintf fmt "max"

let pp_global fmt (id, ty) = fprintf fmt "%s : %a" id pp_var_type ty
let pp_qos_def fmt (id, ty) = fprintf fmt "%s : %a" id pp_var_type ty
let pp_param fmt (id, ty) = fprintf fmt "%s : %a" id pp_var_type ty
let pp_ret fmt (id, ty) = fprintf fmt "%s : %a" id pp_var_type ty
let pp_condition fmt = pp_expr fmt
let pp_trust fmt tr = fprintf fmt "%d" tr

let rec pp_regex fmt = function
  | ContractAST.RService s -> fprintf fmt "%s" s
  | ContractAST.RConcat (r1, r2) ->
      fprintf fmt "(";
      pp_regex fmt r1;
      fprintf fmt " . ";
      pp_regex fmt r2;
      fprintf fmt ")"
  | ContractAST.RChoice (r1, r2) ->
      fprintf fmt "(";
      pp_regex fmt r1;
      fprintf fmt " + ";
      pp_regex fmt r2;
      fprintf fmt ")"
  | ContractAST.RStar r ->
      fprintf fmt "(";
      pp_regex fmt r;
      fprintf fmt ")*"

let pp_policy_type fmt = function
  | ContractAST.QosFieldOp (aggr_op, v, cmp_op, i) ->
      pp_aggr_op fmt aggr_op;
      fprintf fmt "(%s)" v;
      pp_bin_op fmt cmp_op;
      fprintf fmt "%d" i
  | ContractAST.Regex r -> pp_regex fmt r
  | ContractAST.Sort id -> fprintf fmt "sorted(%s)" id

let pp_policy fmt (policy, groupBy) =
  pp_policy_type fmt policy;
  if Option.is_some groupBy then fprintf fmt " group by %s" (Option.get groupBy)

let pp_lhs fmt = function
  | ContractAST.LVar id -> fprintf fmt "%s" id
  | ContractAST.LApp (f, args) ->
      fprintf fmt "%s(" f;
      pp_expr_list fmt args;
      fprintf fmt ")"

let pp_effct fmt (id, e) = fprintf fmt "%a := %a" pp_lhs id pp_expr e

let pp_constrnt fmt (op, id, e) =
  fprintf fmt "%a %a = %a" pp_bin_op op pp_lhs id pp_expr e

let pp_postcond fmt (effs, constrs) =
  fprintf fmt "effects:\n";
  List.iter (fun e -> fprintf fmt "  %a\n" pp_effct e) effs;
  fprintf fmt "constraints:\n";
  List.iter (fun c -> fprintf fmt "  %a\n" pp_expr c) constrs

let pp_service fmt s =
  fprintf fmt "service %s {\n" s.name;
  fprintf fmt "  params:\n";
  List.iter (fun p -> fprintf fmt "    %a\n" pp_param p) s.params;
  fprintf fmt "  returns:\n";
  List.iter (fun r -> fprintf fmt "    %a\n" pp_ret r) s.returns;
  fprintf fmt "  precond:\n";
  List.iter (fun c -> fprintf fmt "    %a\n" pp_condition c) s.precond;
  fprintf fmt "  qos:\n";
  pp_postcond fmt s.qos_postcond;
  fprintf fmt "  ok_post:\n";
  pp_postcond fmt s.ok_postcond;
  fprintf fmt "  err_post:\n";
  pp_postcond fmt s.err_postcond;
  fprintf fmt "}\n"

let pp_contract fmt p =
  fprintf fmt "globals:\n";
  List.iter (fun g -> fprintf fmt "  %a\n" pp_global g) p.globals;

  fprintf fmt "\nfunctions:\n";
  List.iter
    (fun (f, ty) -> fprintf fmt "  %s : %a\n" f pp_fun_type ty)
    p.functions;

  fprintf fmt "\nqos:\n";
  List.iter (fun q -> fprintf fmt "  %a\n" pp_qos_def q) p.qos;

  fprintf fmt "\npolicies:\n";
  List.iter (fun p -> fprintf fmt "  %a\n" pp_policy p) p.policies;

  fprintf fmt "\nservices:\n";
  List.iter (fun s -> fprintf fmt "%a\n" pp_service s) p.services
