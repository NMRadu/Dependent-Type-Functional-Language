{-# LANGUAGE OverloadedStrings #-}
module TypeCheck.Stmt where

import TypeCheck.Context  
import TypeCheck.Expr   
import Lang.Abs
import IR.BruijnConvertor

typecheckStmt :: Context -> Stmt -> Either String Context
typecheckStmt ctx (SBind (Ident x) tyExp) = do
  _ <- inferUniverse ctx [] tyExp
  let ctx' = extendCtx (tyExp, Nothing, x, Nothing) ctx
  return ctx'

typecheckStmt ctx (SBindDom (Ident x) tyExp dom) = do
  _ <- inferUniverse ctx [] tyExp
  let ctx' = extendCtx (tyExp, Nothing, x, Just dom) ctx
  return ctx'

typecheckStmt ctx (SLetIndex (Ident x) i e) 
  | i == 0 = do
    case lookupTy ctx i of
      Nothing ->
        Left $ "Internal error: no declared type at index " ++ show i ++ " for " ++ x
      Just declaredTy -> do
        _ <- checkType ctx [] e declaredTy
        normE <- normalize ctx e
        let ctx' = updateValue i (Just normE) ctx
        return ctx'
  | otherwise = Left "Error: A value definition must follow its type declaration without any intervening statements"
