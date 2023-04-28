{-# LANGUAGE DataKinds, KindSignatures, GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

import Control.Monad.Writer
import Control.Applicative
import Data.Monoid

data TermType = Named | Unnamed

type Var = String
type MuVar = String

data Expr (n :: TermType) where
  Var :: Var -> Expr Unnamed
  Lam :: Var -> Expr Unnamed -> Expr Unnamed
  App :: Expr Unnamed -> Expr Unnamed -> Expr Unnamed
  Freeze :: MuVar -> Expr Unnamed -> Expr Named
  Mu :: MuVar -> Expr Named -> Expr Unnamed
deriving instance Show (Expr n)

substU :: Var -> Expr Unnamed -> Expr n -> Expr n
substU x e = go
  where
    go :: Expr n -> Expr n
    go (Var y) = if y == x then e else Var y
    go (Lam y e) = Lam y $ if y == x then e else go e
    go (App f e) = App (go f) (go e)
    go (Freeze alpha e) = Freeze alpha (go e)
    go (Mu alpha u) = Mu alpha (go u)

renameN :: MuVar -> MuVar -> Expr n -> Expr n
renameN beta alpha = go
  where
    go :: Expr n -> Expr n
    go (Var x) = Var x
    go (Lam x e) = Lam x (go e)
    go (App f e) = App (go f) (go e)
    go (Freeze gamma e) = Freeze (if gamma == beta then alpha else gamma) (go e)
    go (Mu gamma u) = Mu gamma $ if gamma == beta then u else go u

appN :: MuVar -> Expr Unnamed -> Expr n -> Expr n
appN beta v = go
  where
    go :: Expr n -> Expr n
    go (Var x) = Var x
    go (Lam x e) = Lam x (go e)
    go (App f e) = App (go f) (go e)
    go (Freeze alpha w) = Freeze alpha $ if alpha == beta then App (go w) v else go w
    go (Mu alpha u) = Mu alpha $ if alpha /= beta then go u else u

reduceTo :: a -> Writer Any a
reduceTo x = tell (Any True) >> return x

reduce0 :: Expr n -> Writer Any (Expr n)
reduce0 (App (Lam x u) v) = reduceTo $ substU x v u
reduce0 (App (Mu beta u) v) = reduceTo $ Mu beta $ appN beta v u
reduce0 (Freeze alpha (Mu beta u)) = reduceTo $ renameN beta alpha u
reduce0 e = return e

reduce1 :: Expr n -> Writer Any (Expr n)
reduce1 (Var x) = return $ Var x
reduce1 (Lam x e) = reduce0 =<< (Lam x <$> reduce1 e)
reduce1 (App f e) = reduce0 =<< (App <$> reduce1 f <*> reduce1 e)
reduce1 (Freeze alpha e) = reduce0 =<< (Freeze alpha <$> reduce1 e)
reduce1 (Mu alpha u) = reduce0 =<< (Mu alpha <$> reduce1 u)

reduce :: Expr n -> Expr n
reduce e = case runWriter (reduce1 e) of
    (e', Any changed) -> if changed then reduce e' else e
