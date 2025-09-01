module IR.SimplePiConvertor where

import Lang.Abs
import qualified Control.Lens as L
import Control.Lens.Plated (transformM)
import IR.PlatedExp ()


convertExpr :: Exp -> Either String Exp
convertExpr = transformM step
  where
    step :: Exp -> Either String Exp
    step (EPiSimple dom cod) = do
      dom' <- convertExpr dom
      cod' <- convertExpr cod
      let dummyBinder = Ident "_"
      return $ EPi dummyBinder dom' cod'
    step e = Right e
  

convertConsts :: [Const] -> Either String [Const]
convertConsts = traverse convertOne
  where
    convertOne :: Const -> Either String Const
    convertOne (Const name bindExpr) = do
      bindExpr' <- convertExpr bindExpr
      return (Const name bindExpr')

convertStmt :: Stmt -> Either String Stmt
convertStmt (SLet name args e) = SLet name args <$> convertExpr e
convertStmt (SBind name e) = SBind name <$> convertExpr e
convertStmt (SBindDom name e dom) =
  SBindDom name <$> convertExpr e <*> pure dom

convertSimplePi :: Program -> Either String Program
convertSimplePi (Program stmts expr) = do
  stmts' <- traverse convertStmt stmts
  expr'  <- convertExpr expr
  return $ Program stmts' expr'
