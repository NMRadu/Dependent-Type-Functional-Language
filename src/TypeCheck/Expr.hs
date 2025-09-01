{-# LANGUAGE OverloadedStrings #-}
module TypeCheck.Expr where

import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import           Data.List      (foldl')
import TypeCheck.Context
import Lang.Abs
import IR.BruijnConvertor (subst, shift, shiftAndBind, shiftDownAll, collectPatternVars, countBinds)
import Data.Maybe (fromJust)
import qualified Data.Bifunctor
import Control.Arrow (ArrowChoice(right))
import Debug.Trace (trace)
import GHC.Base (TyCon(TyCon))

inferType :: Context -> SubsContext -> Exp -> Either String Exp
-- EType
inferType ctx recurrenceList (EType level) = Right (EType (level + 1))
-- EIndex
inferType ctx recurrenceList (EIndex index) = case lookupTy ctx index of
    Just varType -> normalize ctx varType
    Nothing -> Left "Variable is not bound"
--EApp
inferType ctx recurrenceList (EApp e1 e2) = do
  e1Norm <- normalize ctx e1
  case e1Norm of
    ELam _ body -> do
      e2Norm <- normalize ctx e2
      body' <- normalize ctx (subst 0 e2Norm body)
      inferType ctx recurrenceList body'
    _ -> do
      (dom, codom) <- inferPi ctx recurrenceList e1Norm
      _ <- checkType ctx recurrenceList e2 dom
      normalize ctx (subst 0 e2 codom)
--ELam
inferType ctx recurrenceList (ELam arg body) = Right (ELam arg body)
-- EMatch
inferType ctx recurrenceList (EMatch scrut branches) = do
    Right (EMatch scrut branches)
--Epi
inferType ctx recurrenceList (EPi (Ident x) e1 e2) = do
  k1 <- inferUniverse ctx recurrenceList e1
  k2 <- inferUniverse (extendCtx (e1, Nothing, x, Nothing) ctx) recurrenceList e2
  return (EType (max k1 k2))


normalize::Context -> Exp -> Either String Exp
normalize ctx (EIndex i) = case lookupValue ctx i of
    Just maybeExp -> case maybeExp of
        Just e -> normalize ctx e
        Nothing -> return (EIndex i)
    _ ->  Left $ "Unbound index: " ++ show i ++ show ctx
normalize ctx (EApp e1 e2) =
  do
    e2Norm <- normalize ctx e2
    e1Norm <- normalize ctx e1
    case e1Norm of
        ELam _ body -> normalize ctx (subst 0 e2Norm body)
        e1' -> Right (EApp e1' e2Norm)
normalize ctx (EType k) = Right (EType k)
normalize ctx (EPi arg@(Ident name) dom codom) = do
    domNorm <- normalize ctx dom
    codomNorm <- normalize (extendCtx (domNorm, Nothing, name, Nothing) ctx) codom
    return (EPi arg domNorm codomNorm)
normalize ctx (ELam args body) = return (ELam args body)
--EMatch
normalize ctx (EMatch scrut branches) = do
    scrutNorm <- normalize ctx scrut
    case go ctx scrutNorm branches of
      Left error -> return (EMatch scrut branches)
      Right rez -> return rez
  where
    go::Context -> Exp -> [Branch] ->Either String Exp
    go ctx scrut (branch: branches) = do
      case normalizeBranch ctx scrut branch of
        Left error -> go ctx scrut branches
        Right result -> return result
    go ctx scrut [] = Left ("Non-exhaustive pattern-matching on"++ prettyPrintContextAll ctx ++ prettyPrintExp ctx scrut)


inferPi::Context -> SubsContext -> Exp -> Either String (Exp, Exp)
inferPi ctx recurrenceList e = do
    t <- inferType ctx recurrenceList e
    case t of
        EPi _ e1 e2 -> return (e1,e2)
        eBad -> Left ("Expected Pi but got: " ++ prettyPrintExp ctx eBad)


inferUniverse::Context -> SubsContext -> Exp -> Either String Integer
inferUniverse ctx recurrenceList e = do
    u <- inferType ctx recurrenceList e
    result <- normalize ctx u
    case result of
        EMatch scrut branches -> go ctx scrut branches
        (EType k) -> return k
        eBad -> Left ("Expected a type but got: " ++  prettyPrintExp ctx result)
  where
    go::Context->Exp-> [Branch]-> Either String Integer
    go ctx scrut [] = Right 0
    go ctx scrut (branch:xs) = do
      (rhsTy, patBody, ctx') <- inferBranch ctx recurrenceList scrut branch
      rhsNorm <- normalize ctx' rhsTy
      case rhsNorm of
        (EType k) -> do
          kFinal <- go ctx scrut xs
          return (max k kFinal)
        eBad -> Left ("Expected a type but got: " ++ prettyPrintExp ctx rhsNorm)

compareBranch::Context -> Exp -> (Branch, Branch) -> Either String ()
compareBranch ctx scrut (b1@(Branch pat1 rhs1), Branch pat2 rhs2) = if pat1 == pat2
  then do
    (rhs1Type, _, ctx') <- inferBranch ctx [] scrut b1
    rhs2Value <- normalize ctx' rhs2
    if rhs1Type == rhs2Value
      then
        return ()
      else
        Left $ unlines [ "Type missmatch in rhs:"
          , "  expected = " ++ prettyPrintExp ctx' rhs2Value
          , "  but got  = " ++ prettyPrintExp ctx' rhs1Type
          ]
  else
    Left $ unlines [ "Patterns do not match:"
          , prettyPrintExp ctx pat1
          , prettyPrintExp ctx pat2
          ]




-- Check that `e :: expectedTy`, returning the (possibly α-renamed) e
checkType :: Context -> SubsContext -> Exp -> Exp -> Either String ()
checkType ctx recurrenceList e expectedTy = do
  expectedTy' <- normalize ctx expectedTy
  case (e, expectedTy') of
    (EMatch scrut1 branch1, EMatch scrut2 branch2 ) ->
      if normalize ctx scrut1 == normalize ctx scrut2 then do
        let comb = zip branch1 branch2
        if length branch1 == length branch2
          then do
            scrut <- normalize ctx scrut1
            mapM_ (compareBranch ctx scrut) comb
          else
            Left "EMatches have different lengths"
      else
        Left $ unlines
          [ "Type missmatch in scrut:"
          , "  expected = " ++ prettyPrintExp ctx scrut1
          , "  but got  = " ++ prettyPrintExp ctx scrut2
          ]
    (stuckEMatch@(EMatch scrut branches), _ ) -> do
      let recurrenceMap = fromAssocList recurrenceList
      let match = replaceIndices ctx stuckEMatch
      case M.lookup match recurrenceMap of
        Just ty -> return ()
        _ -> do
          scrutNorm <- normalize ctx scrut
          let recurrenceList' = (match, expectedTy'): recurrenceList
          mapM_ (checkTypeBranch ctx recurrenceList' scrut expectedTy') branches
    (ELam args@[Ident name] body, EPi _ dom codom) -> do
      let ctx' = extendCtx (dom, Nothing, name, Nothing) ctx
      body' <- checkType ctx' recurrenceList body codom
      return ()
    _ -> do
      inferred <- inferType ctx recurrenceList e
      case inferred of
        stuckEMatch@(EMatch _ _) -> do
          let recurrenceMap = fromAssocList recurrenceList
          let match = replaceIndices ctx stuckEMatch
          case M.lookup match recurrenceMap of
            Just ty -> return()
            _ -> do
              let recurrenceList' = (match, expectedTy'): recurrenceList
              checkType ctx recurrenceList' stuckEMatch expectedTy'
        stuckELam@(ELam _ _) -> do
          checkType ctx recurrenceList stuckELam expectedTy'
        _ -> do
          inferred'  <- normalize ctx inferred
          if inferred' == expectedTy'
            then return ()
            else do
              Left $ unlines
                [ "Type missmatch:"
                , "  expected = " ++ prettyPrintExp ctx expectedTy'
                , "  but got  = " ++ prettyPrintExp ctx inferred'
                ]


checkTypeBranch :: Context -> SubsContext -> Exp -> Exp -> Branch -> Either String ()
checkTypeBranch ctx recurrenceList scrut expectedTy branch = do
  (rhsTy, patBody, ctx') <- inferBranch ctx recurrenceList scrut branch
  let shiftAmount = fromIntegral (length ctx' - length ctx)
  expectedTy' <- shiftDownAll (- shiftAmount) expectedTy
  scrut' <- shiftDownAll (- shiftAmount) scrut
  let sCtxMap = fromAssocList [(scrut', patBody)]
  expectedTy'' <- normalize ctx' (applySubstitution sCtxMap expectedTy')
  case rhsTy of
    e@(EMatch _ _) -> do
      recurrenceList' <- mapM (\(x,y) -> do
          y' <- shiftDownAll (- shiftAmount) y
          return (x, y') ) recurrenceList
      let match = replaceIndices ctx' e
      let recurrenceMap = fromAssocList recurrenceList'
      case M.lookup match recurrenceMap of
        Just ty -> return()
        _ -> do
          let recurrenceList'' = (match, expectedTy''): recurrenceList'
          checkType ctx' recurrenceList'' e expectedTy''
    _ -> if  rhsTy == expectedTy''
      then return ()
      else Left ("Error when checking " ++ prettyPrintExp ctx' rhsTy++ " with " ++ prettyPrintExp ctx expectedTy )




---------------------------- Pattern Match Infer Logic ---------------------------

type SubsContext =  [(Exp, Exp)]

recSearch::Context -> Integer -> Ident -> Exp -> Either String Integer
recSearch ctx i name@(Ident strName) searchDom
    | index < length ctx = case lookupDom ctx i of
        Just (Just someDom) -> if someDom == searchDom && Just strName == lookupName ctx i then Right i else recSearch ctx (i + 1) name searchDom
        _ -> recSearch ctx (i + 1) name searchDom
    | otherwise = Left ("The pattern: " ++ show name ++ " does not match to anything")
    where
        index::Int
        index = fromIntegral i

indexOfInDomain::Context -> Exp -> Exp -> Either String Integer
indexOfInDomain ctx (EVar name ) searchDom@(EIndex _) = recSearch ctx 0 name searchDom
indexOfInDomain ctx (EVar name ) _ = Left "Cannot search on something that is not an index"

takeFirstArg::Exp -> Either String Exp
takeFirstArg (EApp e1 e2) = takeFirstArg e1
takeFirstArg out@(EVar _) = Right out
takeFirstArg out@(EType _) = Right out
takeFirstArg out@(EIndex _) = Right out
takeFirstArg x = Left ("You can only pattern check on types and not on (1) :" ++ show x)

reshapePattern::Exp -> Either String Exp
reshapePattern (EApp e1@(EApp _ _) e2) = do
    aux <- reshapePattern e1
    return (EApp aux e2)
reshapePattern (EApp _ e2) = return e2
reshapePattern x = Left ("You can only pattern check on types and not on (2) :" ++ show x)



reapplyAll :: (Exp, Exp) -> SubsContext -> SubsContext
reapplyAll rule ctx =
  let smap = fromAssocList [rule]
  in  rule:[ ( applySubstitution smap l
        , applySubstitution smap r
        )
      | (l,r) <- ctx
      ]

updateSubsts::SubsContext -> Exp -> Exp -> Either String SubsContext
updateSubsts sCtx (EApp e11 e12) (EApp e21 e22) = do
        sCtx' <- updateSubsts sCtx e11 e21
        updateSubsts sCtx' e12 e22
updateSubsts sCtx e1@(EIndex i1) e2@(EIndex i2)
  | i1 == i2 = return sCtx
  | i1 < i2 = return (reapplyAll (e1, e2) sCtx)
  | otherwise = return (reapplyAll (e2, e1) sCtx)
updateSubsts sCtx e1@(EIndex _) e2 = return (reapplyAll (e1, e2) sCtx)
updateSubsts sCtx e1 e2@(EIndex _) = return (reapplyAll (e1, e2) sCtx)
updateSubsts sCtx _ _ = return sCtx

collectTeleTypes :: Exp -> ([Exp], Exp)
collectTeleTypes = go []
  where
    go acc (EPi _ dom cod) = go (acc ++ [dom]) cod
    go acc e               = (acc, e)

applyShifts
  :: Integer-> Integer-> [Exp]-> [Exp]
applyShifts a b = zipWith (\i x -> shift (a + i) b x) [0..]

applySubstList:: Integer-> [Exp ]-> [Exp]-> [Exp]
applySubstList a b = zipWith (\i x -> subst (a + i) (buildEApp (applyShifts 0 i b)) x) [0..]

recursivePassType::Context -> SubsContext -> Exp -> [Exp] -> [Exp] -> Either String (Context, SubsContext, [Exp], [Exp])
recursivePassType ctx sCtx e@(EApp x y) [element, last_element] collected = do
  (ctx'', sCtx'', prefix', result) <- checkPatternType ctx sCtx e element
  let collected'' = buildEApp result : map (shift 0 prefix') collected
  let tList' = applyShifts 1 prefix' [last_element]
  let tList'' = applySubstList 0 result tList'
  return (ctx'', sCtx'',  tList'', collected'')

recursivePassType ctx sCtx (EApp x y) list collected = do
    (ctx', sCtx', rest, collected') <- recursivePassType ctx sCtx x list collected
    case y of
        (EVar _) -> do
            (ctx'', sCtx'', rest', collected'') <- recursivePassType ctx' sCtx' y  rest collected'
            return (ctx'', sCtx'', rest', collected'')
        (EApp _ _) -> do
            case rest of
              [] -> Left "Error in pattern type formatting: Too little arguments given in pattern"
              (hList:tList) -> do
                (ctx'', sCtx'', prefix', result) <- checkPatternType ctx' sCtx' y hList
                let collected'' = buildEApp result : map (shift 0 prefix') collected'
                let tList' = applyShifts 1 prefix' tList
                let tList'' = applySubstList 0 result tList'

                return (ctx'', sCtx'',  tList'', collected'')

recursivePassType ctx sCtx (EVar name) (head:rest) collected = do
  (ctx', sCtx', prefix, result) <- checkPatternType ctx sCtx (EVar name) head
  let collected' = buildEApp result : map (shift 0 prefix) collected
  return (ctx', sCtx', rest, collected')

recursivePassType ctx sCtx _ _ _ = Left "Error in pattern type formatting"

buildEApp :: [Exp] -> Exp
buildEApp [] = error "buildEApp: cannot build from empty list"
buildEApp xs = foldl EApp (last xs) (reverse (init xs))

checkPatternType::Context -> SubsContext -> Exp -> Exp -> Either String (Context, SubsContext, Integer, [Exp])
checkPatternType ctx subCtx pat comp = do
    patTy <- takeFirstArg pat
    compTy <- takeFirstArg comp
    case indexOfInDomain ctx patTy compTy of
        Left _ -> case pat of
            (EVar (Ident name)) -> do
              return (extendCtx (comp, Nothing, name, Nothing) ctx, map ( Data.Bifunctor.bimap (shift 0 1) (shift 0 1) ) subCtx , 1, [EIndex 0])
            x -> Left ("The pattern format is invalid: " ++ prettyPrintExp ctx x )
        Right resultTypeIndex -> do
            let aTy = fromJust (lookupTy ctx resultTypeIndex)
            let aVal = Just (EIndex resultTypeIndex)
            let aName = fromJust (lookupName ctx resultTypeIndex)
            let aDom = fromJust (lookupDom ctx resultTypeIndex)
            let ctx' = extendCtx (aTy, aVal , aName, aDom) ctx

            let resultTypeIndex' = resultTypeIndex + 1
            let sCtx = (EIndex 0, EIndex resultTypeIndex') : map ( Data.Bifunctor.bimap (shift 0 1) (shift 0 1) ) subCtx
            case pat of
                (EVar _) -> return (ctx', sCtx, 1, [EIndex 0])
                _ -> do
                    piType <- inferType ctx' [] (EIndex resultTypeIndex')
                    let (types, result) = collectTeleTypes piType
                    pat' <- reshapePattern pat
                    (ctx'', sCtx', rest, collected) <- recursivePassType ctx' sCtx pat' (types ++ [result]) [EIndex 0]

                    case rest of
                      [compResType] -> do
                        let addedElements = fromIntegral (length ctx'' - length ctx)
                        sCtx'' <- updateSubsts sCtx' compResType (shift 0 addedElements comp)
                        return (ctx'', sCtx'', addedElements, collected)
                      _ -> Left $ "Miss match in the number of parameters in a pattern" ++ show (types ++ [result])

applySubstitution :: SubstMap -> Exp -> Exp
applySubstitution smap expr =
    let expr' = go expr
    in if expr' == expr then expr else applySubstitution smap expr'
  where
    shiftSubstMap :: Integer -> SubstMap -> SubstMap
    shiftSubstMap d = M.map (shift 0 d) . M.mapKeys (shift 0 d)

    go :: Exp -> Exp
    go e
      | Just rhs <- M.lookup e smap = rhs
    go (EApp f x) = EApp (go f) (go x)
    go (ELam args body) =
      let smap' = shiftSubstMap 1 smap
      in ELam args (applySubstitution smap' body)
    go (EPi arg dom codom) =
      let dom' = go dom
          smap' = shiftSubstMap 1 smap
          codom' = applySubstitution smap' codom
      in EPi arg dom' codom'
    go (EType k) = EType k
    go (EIndex i) = EIndex i
    go (EMatch scrut branches) =
      let scrut' = go scrut
      in EMatch scrut' (map goBranch branches)

    goBranch :: Branch -> Branch
    goBranch (Branch pat rhs) =
      let nBinds = countBinds pat
          smap'  = shiftSubstMap nBinds smap
      in Branch pat (applySubstitution smap' rhs)


applySubstitutionContext::Context -> SubsContext -> Context
applySubstitutionContext ctx ((e1,e2):sCtx) = case (e1,e2) of
  (EIndex i, EIndex j) -> applySubstitutionContext (updateValue i (Just (EIndex (j-i))) ctx) sCtx
  _ -> applySubstitutionContext ctx sCtx
applySubstitutionContext ctx [] = ctx


inferBranch::Context -> SubsContext-> Exp -> Branch -> Either String (Exp, Exp, Context)
inferBranch ctx recurrenceList scrut (Branch pat rhs) = do
  comp <- inferType ctx recurrenceList scrut
  (ctx', sCtx, prefix, collected) <- checkPatternType ctx [] pat comp

  let ctx'' = applySubstitutionContext ctx' sCtx

  let patternBody = applySubstitution (fromAssocList sCtx) (buildEApp collected)
  rhsNorm <- normalize ctx'' rhs
  rhsTy <- inferType ctx'' recurrenceList rhsNorm
  let rhsTy' = applySubstitution (fromAssocList sCtx) rhsTy
  rhsTy'' <- normalize ctx'' rhsTy'

  return (rhsTy'', patternBody, ctx'')
--------------------- Normalize Pattern Matching ---------------------------------

normalizeBranch:: Context -> Exp -> Branch -> Either String Exp
normalizeBranch ctx evalExp (Branch pat rhs) = do
  (ctx', sCtx, _) <- buildContextPattern ctx [] pat evalExp
  rhsNorm <- normalize ctx' rhs
  let rhsNorm' = applySubstitution (fromAssocList sCtx) rhsNorm
  rez <- shiftDownAll (fromIntegral (length ctx' - length ctx)) rhsNorm'
  shiftDownAll (fromIntegral (length ctx' - length ctx)) rhsNorm'

checkMatchingName::Context -> Exp -> Exp -> Either String Integer
checkMatchingName ctx (EVar (Ident name)) (EIndex i) = if fromJust (lookupName ctx i) == name
  then return i else Left "Invalid Pattern"
checkMatchingName ctx _ comp = Left $ "Can only pattern check on data types and not on: " ++ prettyPrintExp ctx comp

createValueTypeBinder::Context -> String -> Exp -> Either String Context
createValueTypeBinder ctx name elem = do
  resultType <- inferType ctx [] elem
  let ctx' = extendCtx (resultType, Nothing, name, Nothing) ctx
  let ctx''= updateValue 0 (Just (shift 0 1 elem )) ctx'
  return ctx''

recursivePassNorm::Context -> SubsContext -> Exp -> Exp -> Either String (Context, SubsContext, Integer)
recursivePassNorm ctx sCtx (EApp x y) (EApp a b)  = do
    (ctx', sCtx', shiftNumber) <- recursivePassNorm ctx sCtx x a
    let b' = shift 0 shiftNumber b
    case y of
        (EVar _) -> do
            (ctx'', sCtx'', shiftNumber') <- recursivePassNorm ctx' sCtx' y b'
            return (ctx'', sCtx'', shiftNumber' + shiftNumber)
        (EApp _ _) -> do
            (ctx'', sCtx'', shiftNumber') <- buildContextPattern ctx' sCtx' y b'
            return (ctx'', sCtx'', shiftNumber + shiftNumber')

recursivePassNorm ctx sCtx (EVar name) e = do
  (ctx', sCtx', shiftNumber) <- buildContextPattern ctx sCtx (EVar name) e
  return (ctx', sCtx', shiftNumber)

recursivePassNorm ctx sCtx _ _ = Left "Error in pattern type formatting"

checkDomainConflict :: Context -> Exp -> Maybe Exp -> Bool
checkDomainConflict _ _ Nothing = False
checkDomainConflict ctx (EVar (Ident patName)) dom =
  any ok [0..toInteger (length ctx) - 1]
  where
    ok i = lookupDom ctx i == Just dom
        && lookupName ctx i == Just patName
checkDomainConflict _ _ _ = False


buildContextPattern::Context -> SubsContext -> Exp -> Exp -> Either String (Context, SubsContext, Integer)
buildContextPattern ctx subCtx pat comp = do
    patTy <- takeFirstArg pat
    compTy <- takeFirstArg comp
    case checkMatchingName ctx patTy compTy of
        Left error -> case pat of
            (EVar (Ident name)) -> do
              case compTy of
                (EType u) -> do ctx' <- createValueTypeBinder ctx name comp
                                return (ctx', map ( Data.Bifunctor.bimap (shift 0 1) (shift 0 1) ) subCtx , 1)
                (EIndex i) -> do
                  compType <- inferType ctx [] comp
                  compFirstElement <- takeFirstArg compType
                  case indexOfInDomain ctx patTy compFirstElement of
                                Right _ -> Left ("Cannot use an element from the same domain to pattern match " ++ prettyPrintExp ctx patTy)
                                Left error -> do
                                  ctx' <- createValueTypeBinder ctx name comp
                                  return (ctx', map ( Data.Bifunctor.bimap (shift 0 1) (shift 0 1) ) subCtx , 1)


            x -> Left error
        Right resultTypeIndex -> do
            let aTy = fromJust (lookupTy ctx resultTypeIndex)
            let aVal = fromJust (lookupValue ctx resultTypeIndex)
            let aName = fromJust (lookupName ctx resultTypeIndex)
            let aDom = fromJust (lookupDom ctx resultTypeIndex)
            let ctx' = extendCtx (aTy, aVal , aName, aDom) ctx
            let resultTypeIndex' = resultTypeIndex + 1

            let comp' = shift 0 1 comp
            let sCtx = (EIndex 0, EIndex resultTypeIndex') : map ( Data.Bifunctor.bimap (shift 0 1) (shift 0 1) ) subCtx

            case pat of
                (EVar _) -> return (ctx', sCtx, 1)
                _ -> do
                    comp'' <- reshapePattern comp'
                    pat' <- reshapePattern pat
                    (ctx'', sCtx', shiftNumber) <- recursivePassNorm ctx' sCtx pat' comp''
                    return (ctx'', sCtx', shiftNumber + 1)

----------------------------- Pretty Print ---------------------------------------

prettyPrintExp :: Context -> Exp -> String
prettyPrintExp ctx (EIndex x) = case lookupName ctx x of
  Just name -> name
  Nothing   -> "@" ++ show x  -- fallback to index display

prettyPrintExp ctx (EType x) = "U " ++ show x

prettyPrintExp ctx (EApp e1 e2) =
  "(" ++ prettyPrintExp ctx e1 ++ " " ++ prettyPrintExp ctx e2 ++ ")"

prettyPrintExp ctx (ELam args body) =
  case args of
    [Ident name] ->
      let ctx' = extendCtx (EIndex 0, Nothing, name, Nothing) ctx
       in "\\" ++ name ++ " -> " ++ prettyPrintExp ctx' body
    _ -> error "Unsupported: multiple args in lambda (only one supported here)"

prettyPrintExp ctx (EPi (Ident name) dom codom) =
  let domStr   = prettyPrintExp ctx dom
      ctx'     = extendCtx (dom, Nothing, name, Nothing) ctx
      codomStr = prettyPrintExp ctx' codom
   in "(" ++ name ++ " : " ++ domStr ++ ") -> " ++ codomStr

prettyPrintExp ctx (EMatch scrut branches) =
  "match " ++ prettyPrintExp ctx scrut ++ " with\n"
  ++ concatMap (prettyPrintBranch ctx) branches

prettyPrintExp ctx (EVar (Ident name)) = name


prettyPrintBranch :: Context -> Branch -> String
prettyPrintBranch ctx (Branch pat rhs) =
    let
      names = collectPatternVars pat
      ctx'  = foldl' (\c (Ident name) ->
                        extendCtx (EType 0, Nothing, name, Nothing) c
                     )
                     ctx
                     names

  in  "  | " ++ prettyPrintExp ctx  pat
   ++ " -> "  ++ prettyPrintExp ctx' rhs
   ++ "\n"


prettyPrintContextAll :: Context -> String
prettyPrintContextAll ctx = let
  ctxLines = [ prettyPrintContext ctx i | i <- reverse [0 .. fromIntegral (length ctx - 1)] ]
  in
    unlines ctxLines

prettyPrintContext :: Context -> Integer -> String
prettyPrintContext ctx index
  | i < length ctx =
  let
    name   = maybe "_" id (lookupName ctx index)
    mty    = lookupTy    ctx index
    mval   = lookupValue ctx index
    mDom   = lookupDom ctx index
    tyStr  = maybe "<unknown type>" (prettyPrintExp ctx) mty
    valStr = case mval of
      Just (Just v) -> " = " ++ prettyPrintExp ctx v
      _             -> ""
    domStr = case mDom of
      Just (Just v) -> "   under domain: " ++ prettyPrintExp ctx v
      _             -> ""
  in
    name ++ " : " ++ tyStr ++ valStr ++ domStr

  | otherwise = "<index out of bounds>"

  where
    i :: Int
    i = fromIntegral index
prettyPrintSubsContextAll ::  Context -> SubsContext -> String
prettyPrintSubsContextAll ctx sCtx = let
  ctxLines = [ prettyPrintSubsContext ctx sCtx i | i <- reverse [0 .. fromIntegral (length sCtx - 1)] ]
  in
    unlines ctxLines

prettyPrintSubsContext :: Context -> SubsContext -> Integer -> String
prettyPrintSubsContext ctx sCtx index
  | i < length sCtx =
  let
    firstElement = prettyPrintExp ctx (fst (sCtx !! i)) ++ "(" ++ show (fst (sCtx !! i)) ++ ")"
    secondElement = prettyPrintExp ctx (snd (sCtx !! i)) ++ "(" ++ show (snd (sCtx !! i)) ++ ")"
  in
    "Replace " ++ firstElement ++ " with " ++ secondElement

  | otherwise = "<index out of bounds>"
  where
    i :: Int
    i = fromIntegral index
