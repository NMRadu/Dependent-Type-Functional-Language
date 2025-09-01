module IR.BruijnConvertor where

import Lang.Abs

import qualified Data.Map as Map

import Lang.Abs

type Ctx = Map.Map Ident Integer

collectPatternVars :: Exp -> [Ident]
collectPatternVars (EVar x)     = [x]
collectPatternVars (EApp f arg) = collectPatternVars f ++ collectPatternVars arg
collectPatternVars _            = []


shiftAndBind :: Ident -> Ctx -> Ctx
shiftAndBind x oldCtx =
  let shifted = Map.map (+1) oldCtx
  in  Map.insert x 0 shifted

convertExprMap :: Ctx -> Exp -> Either String Exp

convertExprMap _   (EType n) = Right (EType n)
convertExprMap _   (ENat n) = Left "Should not be possible"
convertExprMap ctx (EVar x) =
  case Map.lookup x ctx of
    Just i  -> Right (EIndex i)
    Nothing -> Left ("[convertBruijn] Unbound variable: " ++ show x)

convertExprMap _   (EIndex i) = Right (EIndex i)

convertExprMap ctx (EApp e1 e2) = do
  e1' <- convertExprMap ctx e1
  e2' <- convertExprMap ctx e2
  return (EApp e1' e2')

convertExprMap ctx (ELam [] body) =
  convertExprMap ctx body

convertExprMap ctx (ELam [x] body) =
  let ctx' = shiftAndBind x ctx
  in do
    body' <- convertExprMap ctx' body
    return (ELam [x] body')

convertExprMap ctx (EPi x dom cod) = do
  dom'  <- convertExprMap ctx dom
  let ctx' = shiftAndBind x ctx
  cod'  <- convertExprMap ctx' cod
  return (EPi x dom' cod')

convertExprMap ctx (EMatch scrut branches) = do
  scrut'   <- convertExprMap ctx scrut
  branches' <- mapM (convertBranch ctx) branches
  return (EMatch scrut' branches')
 where
  convertBranch :: Ctx -> Branch -> Either String Branch
  convertBranch ctx0 (Branch pat body) = do
    let binders = collectPatternVars pat
        ctx'    = foldl (flip shiftAndBind) ctx0 binders
    body' <- convertExprMap ctx' body
    return (Branch pat body')


convertStmtMap :: Ctx -> Stmt -> Either String (Ctx, Stmt)
convertStmtMap ctx (SLet fname [] e) = do
  findex <- case Map.lookup fname ctx of
    Just i  -> Right i
    Nothing -> Left ("Unbound function: " ++ show fname)
  e' <- convertExprMap ctx e
  return (ctx, SLetIndex fname findex e')

convertStmtMap ctx (SBind name e) = do
  e'   <- convertExprMap ctx e
  let ctx' = shiftAndBind name ctx
  return (ctx', SBind name e')

convertStmtMap ctx (SBindDom name e dom) = do
  e'   <- convertExprMap ctx e
  dom' <- convertExprMap ctx dom
  let ctx' = shiftAndBind name ctx
  return (ctx', SBindDom name e' dom')



convertBruijn :: Program -> Either String Program
convertBruijn (Program stmts expr) = do
  let initialCtx = Map.empty
  (finalCtx, stmts') <- foldl step (Right (initialCtx, [])) stmts
  expr' <- convertExprMap finalCtx expr
  return (Program (reverse stmts') expr')
  where
    step (Left err) _ = Left err
    step (Right (ctxSoFar, accStmts)) thisStmt = do
      (ctxNext, thisStmt') <- convertStmtMap ctxSoFar thisStmt
      return (ctxNext, thisStmt' : accStmts)



shift :: Integer   -> Integer  -> Exp -> Exp
shift cutoff k = go
  where
    go (EType i)       = EType i
    go (EVar x)        = EVar x
    go (EIndex i)
      | i >= cutoff    = EIndex (i + k)
      | otherwise      = EIndex i
    go (EApp f a)      = EApp (go f) (go a)
    go (ELam [] b)     = ELam [] (go b)
    go (ELam [x] b)    =
      ELam [x] (shift (cutoff + 1) k b)
    go (EPi x d c)     =
      let d' = go d
          c' = shift (cutoff + 1) k c
      in EPi x d' c'
    go (EMatch scrut branches) =
      let scrut'= go scrut
          shiftBranch (Branch pat rhs) =
            let nBinds = countBinds pat
            in Branch pat (shift (cutoff + nBinds) k rhs)
      in EMatch scrut' (map shiftBranch branches)
    


subst :: Integer -> Exp -> Exp -> Exp
subst n u = go
  where
    go (EType i)    = EType i
    go (EIndex i)
      | i == n      = u
      | i >  n      = EIndex (i - 1)
      | otherwise   = EIndex i
    go (EApp f a)   = EApp (go f) (go a)
    go (ELam [] b)  = ELam [] (go b)
    go (ELam [x] b) =
      let u' = shift 0 1 u
      in ELam [x] (subst (n + 1) u' b)
    go (EPi x d c)  =
      let d'  = go d
          u'  = shift 0 1 u
          c'  = subst (n + 1) u' c
      in EPi x d' c'
    go (EMatch scrut branches) =
      let scrut' = go scrut
          substBranch (Branch pat rhs) =
            let nBinds = countBinds pat
                u' = shift 0 nBinds u
           in Branch pat
                      (subst (n + nBinds) u' rhs)
      in EMatch scrut' (map substBranch branches)



countBinds :: Exp -> Integer
countBinds (EVar _)       = 1
countBinds (EApp f x)     = countBinds f + countBinds x
countBinds _              = 0

shiftDownAll :: Integer -> Exp -> Either String Exp
shiftDownAll d expr = Right(shift 0 (-d) expr)