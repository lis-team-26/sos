open Expr.AST
open Utils.Data
open Utils.Loc
open Utils.Types

type typed_var = ident * var_type
(** An identificator paired with its type *)

type typed_fun = ident * fun_type
(** A function name paired with its type *)

(** Type for aggregation operations *)
type aggr_op = Sum | Avg | Min | Max

type serv2letter = (string * char) list
(** A mapping from service names to letters *)

(** Policy types *)
type policy_type =
  | QosFieldOp of aggr_op * string * bin_op * int
      (** A policy constraining QoS fields *)
  | Regex of serv2letter * string
      (** A regex policy, consisting of a map from services to letters and a
          regex over those letters. *)
  | Sort of ident
      (** A policy constraining values of the attached identificator to be
          sorted along invocation history *)

type policy_spec = (policy_type * string option) located
(** A policy specification, consisting of its type and an optional grouping
    identificator *)

(** Left-hand side of effects *)
type effct_lhs = LVar of ident | LApp of ident * expr list

type effct = effct_lhs located * expr
(** A postcond's effect, consisting of an left-hand side and a right-hand side
    expression *)

type postcond = effct list * expr list
(** A service's postcondition, consisting of a list of effects and a list of
    constraints *)

type service = {
  name : ident;
  params : typed_var located list;
  returns : typed_var;
  precond : expr list;
  qos_postcond : postcond;
  ok_postcond : postcond;
  err_postcond : postcond option;
}
(** A service, consisting of its name, parameters, return value, precondition,
    postconditions and optional error postcondition *)

type contract = {
  globals : typed_var located list;
  globals_assumptions : expr list;
  functions : typed_fun located list;
  policies : policy_spec list;
  qos : typed_var located list;
  services : service located list;
}
(** A contract, consisting of global variables, functions, policies, QoS fields
    common to all services, and a list of services *)
