module IR.NatDesugar where

import Lang.Abs
import Control.Lens.Plated (transformM)
import qualified Control.Lens as L
import IR.PlatedExp ()

desugar :: Integer -> Either String Exp
desugar n
  | n < 0     = Left "Cannot have negative numbers"
  | otherwise = Right $ go n
  where
    go 0 = EVar (Ident "zero")
    go m = EApp (EVar (Ident "suc")) (go (m - 1))

convertExpr :: Exp -> Either String Exp
convertExpr = transformM step
  where
    step (ENat n)
      | n < 0     = Left "Cannot have negative numbers"
      | otherwise = Right (go n)
    step e = Right e

    go 0 = EVar (Ident "zero")
    go m = EApp (EVar (Ident "suc")) (go (m - 1))

convertConst :: Const -> Either String Const
convertConst (Const name typ) = Const name <$> convertExpr typ

convertArgBind :: ArgBind -> Either String ArgBind
convertArgBind (ArgBind ident typ) = ArgBind ident <$> convertExpr typ

convertStmt :: Stmt -> Either String Stmt
convertStmt (SLet name args e) = do
  e' <- convertExpr e
  return (SLet name args e')

convertStmt (SBind name e) =
  SBind name <$> convertExpr e

convertStmt (SInductive name args typ consts) = do
  args'   <- mapM convertArgBind args
  typ'    <- convertExpr typ
  consts' <- mapM convertConst consts
  return (SInductive name args' typ' consts')

convertStmt (SInductiveSimple name typ consts) = do
  typ'    <- convertExpr typ
  consts' <- mapM convertConst consts
  return (SInductiveSimple name typ' consts')



desugarNat :: Program -> Either String Program
desugarNat (Program stmts expr) = do
  stmts' <- traverse convertStmt stmts
  expr'  <- convertExpr expr
  return (Program stmts' expr')