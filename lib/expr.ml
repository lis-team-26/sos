open ExprAST
open Utils.Data
module AST = ExprAST
module AST_pp = ExprAST_pp
module TypedAST = TypedExprAST
module TypedAST_pp = TypedExprAST_pp
module TypeCheck = TypeCheckExpr

let rec free_vars = function
  | EInt _ | EBool _ | EIntNonDet -> StringSet.empty
  | EBoolNonDet -> StringSet.empty
  | EVar v -> StringSet.singleton v
  | EUnOp (_, e) -> free_vars e
  | EApp (_, args) ->
      List.fold_left
        (fun acc arg -> StringSet.union acc (free_vars arg))
        StringSet.empty args
  | EBinOp (e1, _, e2) -> StringSet.union (free_vars e1) (free_vars e2)
