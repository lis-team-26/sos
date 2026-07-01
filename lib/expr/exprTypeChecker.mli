open ExprAST
open TypedExprAST
open Utils.Data
open Utils.Loc
open Utils.Scope
open Utils.Types

val type_check_bool :
  scope:var_type scope ->
  fun_env:fun_type env ->
  expr ->
  (bexpr, string located) result
(** Takes an untyped expression [expr] and checks if it is a well-typed boolean
    expression in the given [scope] and [fun_env]; if not, it returns an error
    message with the source code location of the problematic sub-expression. *)

val type_check_expr :
  scope:var_type scope ->
  fun_env:fun_type env ->
  var_type ->
  expr ->
  (typed_expr, string located) result
(** Takes an untyped expression [expr] and checks if it is a well-typed
    expression of the given type [var_type] in the given [scope] and [fun_env];
    if not, it returns an error message with the source code location of the
    problematic sub-expression. *)

val type_check_app :
  scope:var_type scope ->
  fun_env:fun_type env ->
  loc:loc ->
  ident ->
  expr list ->
  (typed_expr list, string located) result
(** Takes a function application and checks if it is well-typed in the given
    [scope] and [fun_env], by checking that the arguments are both well-typed
    and match the function's signature; if not, it returns an error message with
    the source code location [loc]. *)

val has_fun_app : expr -> loc option
(** Returns the location of the first function application in the expression, if
    any. *)
