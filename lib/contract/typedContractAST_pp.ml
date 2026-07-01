open TypedContractAST
open Expr.TypedAST_pp
open Utils.Data_pp
open Utils.Loc
open Utils.Types_pp

let pp_aggr_op fmt =
  let open ContractAST in
  function
  | Sum -> Fmt.pf fmt "sum"
  | Avg -> Fmt.pf fmt "avg"
  | Min -> Fmt.pf fmt "min"
  | Max -> Fmt.pf fmt "max"

let rec pp_regex fmt (s2l, regex) =
  Fmt.pf fmt "[ %a ] \"%s\""
    (pp_list_inline @@ pp_entry Fmt.string Fmt.char)
    s2l regex

let pp_policy_type fmt = function
  | QosFieldOp (aggr_op, field, cmp_op, threshold) ->
      Fmt.pf fmt "%a(%s) %a %d" pp_aggr_op aggr_op field pp_cmp_op cmp_op
        threshold
  | Regex (s2l, regex) -> pp_regex fmt (s2l, regex)
  | Sort field -> Fmt.pf fmt "sorted(%s)" field

let pp_policy fmt (policy, group_by) =
  pp_policy_type fmt policy;
  Fmt.option (fun fmt x -> Fmt.pf fmt " group by %s" x) fmt group_by

let pp_lhs fmt lhs =
  match lhs with
  | LVar x -> Fmt.pf fmt "%s" x
  | LApp (f, args) -> Fmt.pf fmt "%s(%a)" f (pp_list pp_typed_expr) args

let pp_effect fmt (lhs, rhs) =
  Fmt.pf fmt "%a := %a" pp_lhs lhs pp_typed_expr rhs

let pp_postcond fmt (es, cs) =
  pp_field ~with_cut:true ~name:"effects" (pp_list pp_effect) fmt es;
  pp_field ~name:"constraints" (pp_list pp_bexpr) fmt cs

let pp_service fmt s =
  let pp fmt s =
    pp_field ~with_cut:true ~name:"params" (pp_list Fmt.string) fmt s.params;
    pp_field ~with_cut:true ~name:"returns" pp_typed_var fmt s.returns;
    pp_field ~with_cut:true ~name:"precond" (pp_list pp_bexpr) fmt s.precond;
    pp_field ~with_cut:true ~name:"qos_postcond" pp_postcond fmt s.qos_postcond;
    pp_field ~name:"ok_postcond" pp_postcond fmt s.ok_postcond;
    Fmt.(option (cut ++ pp_field ~name:"err_postcond" pp_postcond))
      fmt s.err_postcond
  in
  pp_section ~name:(Fmt.str "service %s" s.name) pp fmt s

let pp_contract c =
  let pp fmt c =
    pp_section ~with_cut:true ~name:"globals" (pp_list pp_typed_var) fmt
      c.globals;
    pp_section ~with_cut:true ~name:"globals_assumptions" (pp_list pp_bexpr) fmt
      c.globals_assumptions;
    pp_section ~with_cut:true ~name:"functions" (pp_list Fmt.string) fmt
      c.functions;
    pp_section ~with_cut:true ~name:"policies" (pp_list pp_policy) fmt
      c.policies;
    pp_section ~with_cut:true ~name:"qos" (pp_list pp_typed_var) fmt c.qos;
    pp_section ~name:"services" (pp_list pp_service) fmt c.services
  in
  Fmt.vbox pp c
