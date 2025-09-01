-- FunctionDesugar.hs
module IR.FunctionDesugar where

import Lang.Abs

desugarStmt :: Stmt -> Either String Stmt
desugarStmt (SLet name args body) =
  let
    lamChain = foldr (\arg acc -> ELam [arg] acc) body args
  in
    Right (SLet name [] lamChain)

desugarStmt other =
  Right other

desugarFunction :: Program -> Either String Program
desugarFunction (Program stmts expr) = do
  stmts' <- traverse desugarStmt stmts
  return (Program stmts' expr)