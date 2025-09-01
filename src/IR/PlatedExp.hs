module IR.PlatedExp where

import Lang.Abs

import Control.Lens
import Control.Lens.Plated

instance Plated Exp where
  plate f (EApp e1 e2) =
    EApp <$> f e1 <*> f e2

  plate f (ELam xs body) =
    ELam xs <$> f body

  plate f (EPiSimple dom codom) =
    EPiSimple <$> f dom <*> f codom

  plate f (EPi name dom codom) =
    EPi name <$> f dom <*> f codom

  plate f (EMatch scrutinee branches) =
    EMatch <$> f scrutinee <*> traverse goBranch branches
    where
      goBranch (Branch pat rhs) =
        Branch <$> f pat <*> f rhs

  plate _ e = pure e