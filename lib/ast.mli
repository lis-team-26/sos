type arithm_bin_op = Add | Sub | Mul | Div
type bool_bin_op = And | Or
type comp_bin_op = Eq | Neq | Lt | Le | Gt | Ge

type aexpr =
  | Int of int
  | Var of string
  | NonDet
  | AOp of aexpr * arithm_bin_op * aexpr

type bexpr =
  | Bool of bool
  | Not of bexpr
  | BOp of bexpr * bool_bin_op * bexpr
  | COp of aexpr * comp_bin_op * aexpr

type stmt =
  | Skip
  | Assign of string * aexpr
  | Seq of stmt * stmt
  | If of bexpr * stmt * stmt
  | While of bexpr * stmt
  | Assume of bexpr
  | Assert of bexpr
  | Invoke of string * aexpr list
  | AssignInvoke of string * string * aexpr list
