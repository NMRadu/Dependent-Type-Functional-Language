module IR.LambdaDesugar where
import Lang.Abs
import qualified Control.Lens as L
import Control.Lens.Plated (transformM)
import IR.PlatedExp ()

nestLambdas :: [Ident] -> Exp -> Exp
nestLambdas args body =
  foldr (\arg acc -> ELam [arg] acc) body args

convertExpr :: Exp -> Either String Exp
convertExpr = transformM step
  where
    step (ELam [] _) =
      Left "Error: lambda must have at least one argument"
    step (ELam args body) =
      Right (nestLambdas args body)
    step e = Right e


convertStmt :: Stmt -> Either String Stmt
convertStmt (SLet name args e) = do
  e' <- convertExpr e
  return (SLet name args e')

convertStmt (SBind name e) =
  SBind name <$> convertExpr e

convertStmt (SBindDom name e dom) =
  SBindDom name <$> convertExpr e <*> pure dom

desugarLambda :: Program -> Either String Program
desugarLambda (Program stmts expr) = do
  stmts' <- traverse convertStmt stmts
  expr'  <- convertExpr expr
  return (Program stmts' expr')