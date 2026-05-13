open Ast
module Symex : Soteria.Symex.S
module SymbMap : Soteria.Soteria_std.Map.S with type key = string
module Typed = Soteria.Tiny_values.Typed

type symb_int = Typed.T.sint Typed.t
type env = symb_int SymbMap.t
type qos = { cost : symb_int; latency : symb_int }
type call = { serv_name : string; args : symb_int list; qos : qos }
type hist = call list
type ok_state = { env : env; hist : hist }
type err_state = { msg : string; hist : hist }

val build_symb_process : stmt -> (ok_state, err_state, 'a) Symex.Result.t
