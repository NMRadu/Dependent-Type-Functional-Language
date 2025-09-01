{-# LANGUAGE OverloadedStrings #-}
module TypeCheck.Context where
import Lang.Abs
import IR.BruijnConvertor
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import Data.Foldable

type Context = [(Exp, Maybe Exp, String, Maybe Exp)]

take_1 :: (a, b, c, d) -> a
take_1 (x, _, _, _) = x

take_2 :: (a, b, c, d) -> b
take_2 (_, y, _, _) = y

take_3 :: (a, b, c, d) -> c
take_3 (_, _, c, _) = c

take_4 :: (a, b, c, d) -> d
take_4 (_, _, _, d) = d

-- | The empty context (no top‐level binds yet).
emptyCtx :: Context
emptyCtx = []

-- | Push a new binder‐type onto the context.
--   After `extendCtx t ctx`, the new binder of type `t` is at index 0.
extendCtx :: (Exp, Maybe Exp, String, Maybe Exp) -> Context -> Context
extendCtx = (:)

lookupTy :: Context -> Integer -> Maybe Exp
lookupTy ctx k
  | i >= 0 && i < length ctx = Just (shift 0 shift_amount (take_1 (ctx !! i)))
  | otherwise      = Nothing
  where 
    i::Int
    i = fromIntegral k
    shift_amount::Integer
    shift_amount = k + 1

lookupDom :: Context -> Integer -> Maybe (Maybe Exp)
lookupDom ctx k
  | i >= 0 && i < length ctx = Just (case take_4 (ctx !! i) of
      Just exp -> Just (shift 0 shift_amount exp)
      _ -> Nothing)
  | otherwise      = Nothing
  where 
    i::Int
    i = fromIntegral k
    shift_amount::Integer
    shift_amount = k + 1


lookupName :: Context -> Integer -> Maybe String
lookupName ctx k
  | i >= 0 && i < length ctx = Just (take_3 (ctx !! i))
  | otherwise      = Nothing
  where 
    i::Int
    i = fromIntegral k

lookupValue :: Context -> Integer -> Maybe (Maybe Exp)
lookupValue ctx k
  | i >= 0 && i < length ctx = Just (case take_2 (ctx !! i) of
      Just exp -> Just (shift 0 shift_amount exp)
      _ -> Nothing)
  | otherwise      = Nothing
  where 
    i::Int
    i = fromIntegral k
    shift_amount::Integer
    shift_amount = k

    

updateValue :: Integer -> Maybe Exp -> Context -> Context  
updateValue idx rhs ctx =
  [ if i == idx
      then (ty, rhs, name, dom)
      else (ty, val, name, dom)
  | ((ty, val, name, dom), i) <- zip ctx [0..]
  ]



type SubstMap = Map Exp Exp

fromAssocList :: [(Exp, Exp)] -> SubstMap
fromAssocList list = M.fromList (reverse list)

replaceIndices :: Context -> Exp -> Exp
replaceIndices = go
  where
    go :: Context -> Exp -> Exp
    go ctx0 (EIndex i) =
      case lookupName ctx0 i of
        Just name -> EVar (Ident name)
        Nothing   -> EIndex i

    go _    (EVar x)     = EVar x
    go _    (EType k)    = EType k

    go ctx0 (EApp f x)   = EApp (go ctx0 f) (go ctx0 x)

    go ctx0 (ELam [Ident name] body) =
      let ctx' = extendCtx (EIndex 0, Nothing, name, Nothing) ctx0
       in ELam [Ident name] (go ctx' body)
    go _ (ELam _       _) =
      error "replaceIndices: unsupported multi-arg lambda"

    go ctx0 (EPi (Ident name) dom codom) =
      let dom'  = go ctx0 dom
          ctx'  = extendCtx (dom', Nothing, name, Nothing) ctx0
          co'   = go ctx' codom
       in EPi (Ident name) dom' co'

    go ctx0 (EMatch scrut branches) =
      EMatch (go ctx0 scrut) (map (goBranch ctx0) branches)

    goBranch :: Context -> Branch -> Branch
    goBranch ctx0 (Branch pat rhs) =
      let names = collectPatternVars pat
          ctx'  = foldl' (\c (Ident n) -> extendCtx (EType 0, Nothing, n, Nothing) c)
                         ctx0
                         names
       in Branch (go ctx0 pat) (go ctx' rhs)
       