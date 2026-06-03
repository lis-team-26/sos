open Soteria
open Expr.AST
open Contract.AST
open Utils.Data
module Typed = Soteria.Tiny_values.Typed
include Typed.Infix
include Typed.Syntax

type symb_int = Typed.T.sint Typed.t
type symb_bool = Typed.T.sbool Typed.t
type symbolic_value = SymbInt of symb_int | SymbBool of symb_bool
