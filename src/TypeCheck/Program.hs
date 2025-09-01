{-# LANGUAGE OverloadedStrings #-}
module TypeCheck.Program where

import TypeCheck.Context   
import TypeCheck.Stmt      
import TypeCheck.Expr    
import Lang.Abs


evaluateProgram :: Program -> Either String (String, String)
evaluateProgram (Program decls mainExp) = do
  ctx <- goStmts emptyCtx decls
  resultType <- inferType ctx [] mainExp
  resultTypeNorm <- normalize ctx mainExp
  result <- normalize ctx mainExp
  return $ prettyPrintProgramOutput ctx result resultType
  where
    goStmts :: Context -> [Stmt] -> Either String Context
    goStmts ctx []       = Right ctx
    goStmts ctx (s : ss) = do
      ctx' <- typecheckStmt ctx s
      goStmts ctx' ss

prettyPrintProgramOutput :: Context -> Exp -> Exp -> (String, String)
prettyPrintProgramOutput ctx result resultTy =
  let ctxStr   = prettyPrintContextAll ctx
      resultStr    = prettyPrintExp ctx result
      resultTyStr  = prettyPrintExp ctx resultTy
  in (ctxStr, resultStr ++ " : " ++ resultTyStr)