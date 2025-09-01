module IR.InductiveDesugar where

import Lang.Abs


appendArgs::[ArgBind] -> Exp -> Exp
appendArgs (ArgBind name exp : xs) final = EPi name exp (appendArgs xs final)
appendArgs [] final = final

desugarStmt :: Stmt -> [Stmt]
desugarStmt (SInductive name args tyExp consts) = typeBind : consBinds
  where
      typeBind  = SBind name (appendArgs args tyExp)
      consBinds = [SBindDom cName (appendArgs args cTy) (EVar name) | Const cName cTy <- consts]
desugarStmt (SInductiveSimple name tyExp consts) = typeBind : consBinds
  where
      typeBind  = SBind name tyExp
      consBinds = [SBindDom cName cTy (EVar name)| Const cName cTy <- consts]

desugarStmt other = [other]

-- | Desugar an entire program
desugarInductive :: Program -> Either String Program
desugarInductive (Program decls mainExp) =
  return (Program (concatMap desugarStmt decls) mainExp)
