open Format
open ContractAST

let rec pp_type fmt = function
  | ContractAST.TInt -> fprintf fmt "int"
  | ContractAST.TBool -> fprintf fmt "bool"
  | ContractAST.TArrow (args, ret) ->
      fprintf fmt "(";
      pp_type_list fmt args;
      fprintf fmt ") -> ";
      pp_type fmt ret

and pp_type_list fmt = function
  | [] -> ()
  | [ t ] -> pp_type fmt t
  | t :: ts ->
      pp_type fmt t;
      fprintf fmt ", ";
      pp_type_list fmt ts

let pp_bin_op fmt = function
  | ContractAST.Add -> fprintf fmt "+"
  | ContractAST.Sub -> fprintf fmt "-"
  | ContractAST.Mul -> fprintf fmt "*"
  | ContractAST.Div -> fprintf fmt "/"
  | ContractAST.Lt -> fprintf fmt "<"
  | ContractAST.Le -> fprintf fmt "<="
  | ContractAST.Gt -> fprintf fmt ">"
  | ContractAST.Ge -> fprintf fmt ">="
  | ContractAST.Eq -> fprintf fmt "=="
  | ContractAST.Neq -> fprintf fmt "!="
  | ContractAST.Or -> fprintf fmt "||"
  | ContractAST.And -> fprintf fmt "&&"

let pp_un_op fmt = function ContractAST.Not -> fprintf fmt "!"

let pp_aggr_op fmt = function
  | ContractAST.Sum -> fprintf fmt "sum"
  | ContractAST.Avg -> fprintf fmt "avg"
  | ContractAST.Min -> fprintf fmt "min"
  | ContractAST.Max -> fprintf fmt "max"

let rec pp_expr fmt = function
  | ContractAST.EInt i -> fprintf fmt "%d" i
  | ContractAST.EBool b -> fprintf fmt "%b" b
  | ContractAST.EVar v -> fprintf fmt "%s" v
  | ContractAST.ESla -> fprintf fmt "<sla>"
  | ContractAST.EField (e, id) -> fprintf fmt "%a.%s" pp_expr e id
  | ContractAST.EApp (f, args) ->
      fprintf fmt "%s(" f;
      pp_expr_list fmt args;
      fprintf fmt ")"
  | ContractAST.EBinOp (op, a, b) ->
      fprintf fmt "(";
      pp_expr fmt a;
      fprintf fmt " %a " pp_bin_op op;
      pp_expr fmt b;
      fprintf fmt ")"
  | ContractAST.EUnOp (op, e) -> fprintf fmt "(%a%a)" pp_un_op op pp_expr e

and pp_expr_list fmt = function
  | [] -> ()
  | [ e ] -> pp_expr fmt e
  | e :: es ->
      pp_expr fmt e;
      fprintf fmt ", ";
      pp_expr_list fmt es

let pp_global fmt (id, ty) = fprintf fmt "%s : %a" id pp_type ty
let pp_qos_def fmt (id, ty) = fprintf fmt "%s : %a" id pp_type ty
let pp_param fmt (id, ty) = fprintf fmt "%s : %a" id pp_type ty
let pp_ret fmt (id, ty) = fprintf fmt "%s : %a" id pp_type ty
let pp_condition fmt = pp_expr fmt
let pp_trust fmt tr = fprintf fmt "%d" tr

let pp_fun_type =
  let rec go fmt = function
    | ContractAST.TBase t -> pp_type fmt t
    | ContractAST.TArrow (t, rest) -> fprintf fmt "%a -> %a" pp_type t go rest
  in
  go

let pp_func_sig fmt f = fprintf fmt "function %s : %a" f.fname pp_fun_type f.ty

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
  | ContractAST.QosFieldOp (cmp_op, agg_op, id, i) ->
      pp_aggr_op fmt agg_op;
      fprintf fmt "(%s)" id;
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

let pp_behavior fmt (effs, constrs) =
  fprintf fmt "effects:\n";
  List.iter (fun e -> fprintf fmt "  %a\n" pp_effct e) effs;
  fprintf fmt "constraints:\n";
  List.iter (fun c -> fprintf fmt "  %a\n" pp_constrnt c) constrs

let pp_service fmt s =
  fprintf fmt "service %s {\n" s.name;
  fprintf fmt "  params:\n";
  List.iter (fun p -> fprintf fmt "    %a\n" pp_param p) s.params;
  fprintf fmt "  returns:\n";
  List.iter (fun r -> fprintf fmt "    %a\n" pp_ret r) s.returns;
  fprintf fmt "  trusted:\n";
  fprintf fmt "    %a\n" pp_trust s.trust;
  fprintf fmt "  precond:\n";
  List.iter (fun c -> fprintf fmt "    %a\n" pp_condition c) s.precond;
  fprintf fmt "  qos:\n";
  pp_behavior fmt s.qos;
  fprintf fmt "  ok_post:\n";
  pp_behavior fmt s.ok_post;
  fprintf fmt "  err_post:\n";
  pp_behavior fmt s.err_post;
  fprintf fmt "}\n"

let pp_contract fmt p =
  fprintf fmt "globals:\n";
  List.iter (fun g -> fprintf fmt "  %a\n" pp_global g) p.globals;

  fprintf fmt "\nfunctions:\n";
  List.iter (fun f -> fprintf fmt "  %a\n" pp_func_sig f) p.functions;

  fprintf fmt "\nqos:\n";
  List.iter (fun q -> fprintf fmt "  %a\n" pp_qos_def q) p.qos;

  fprintf fmt "\npolicies:\n";
  List.iter (fun p -> fprintf fmt "  %a\n" pp_policy p) p.policies;

  fprintf fmt "\nservices:\n";
  List.iter (fun s -> fprintf fmt "%a\n" pp_service s) p.services
