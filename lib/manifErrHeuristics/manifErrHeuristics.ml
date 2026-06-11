open Symbolic.Runtime
open Symbolic.Data
open Utils.Data

type violation_id =
  | ServicePrecond of string
  | Policy of int
  | Assert of int

type err_state = { msg : string; err_stack : stack; id : violation_id }
