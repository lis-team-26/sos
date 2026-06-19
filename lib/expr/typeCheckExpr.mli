open ExprAST
open TypedExprAST
open Utils.Data
open Utils.Types

val type_check_arithm :
  var_type env list -> fun_type env -> expr -> (aexpr, string) result

val type_check_bool :
  var_type env list -> fun_type env -> expr -> (bexpr, string) result

val type_check_expr :
  var_type ->
  var_type scope ->
  fun_type env ->
  expr ->
  (typed_expr, string) result

val type_check_args :
  ident ->
  var_type list ->
  expr list ->
  var_type env list ->
  fun_type env ->
  (typed_expr list, string) result
