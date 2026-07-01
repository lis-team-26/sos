open ContractAST
open Expr.TypedAST
open Utils.Data

(** Typed policy type. See [ContractAST.policy_type] for more details. *)
type policy_type =
  | QosFieldOp of aggr_op * ident * cmp_op * int
  | Regex of serv2letter * string
  | Sort of ident

type policy_spec = policy_type * ident option
(** Typed policy specification. See [ContractAST.policy_spec] for more details.
*)

(** Typed effect left-hand side. See [ContractAST.effct_lhs] for more details.
*)
type effct_lhs = LVar of ident | LApp of ident * typed_expr list

type effct = effct_lhs * typed_expr
(** Typed effect. See [ContractAST.effct] for more details. *)

type postcond = effct list * bexpr list
(** Typed postcondition. See [ContractAST.postcond] for more details. *)

type service = {
  name : ident;
  params : ident list;
  returns : typed_var;
  precond : bexpr list;
  qos_postcond : postcond;
  ok_postcond : postcond;
  err_postcond : postcond option;
}
(** Typed service. See [ContractAST.service] for more details. *)

type contract = {
  globals : typed_var list;
  globals_assumptions : bexpr list;
  functions : ident list;
  policies : policy_spec list;
  qos : typed_var list;
  services : service list;
}
(** Typed contract specification. See [ContractAST.contract] for more details.
*)
