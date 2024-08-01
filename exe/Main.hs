{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.Random
import Data.Kind (Type)

type family BaseP t :: Type -> Type
type family BaseM t :: Type -> Type

data MixF pAlg mAlg m x
  = Pure (pAlg x)
  | Monadic (mAlg (m x))
  deriving (Functor)

class (Traversable (BaseP t), Traversable (BaseM t)) => Recurse t where
  project :: Applicative m => t -> MixF (BaseP t) (BaseM t) m t

cata
  :: forall t m a . (Monad m, Recurse t)
  => (BaseP t a -> a) -- ^ Pure algebra
  -> (BaseM t (m a) -> m a) -- ^ Monadic algebra
  -> t
  -> m a
cata f g expr = case project expr of
  Pure p -> runPure p
  Monadic m -> runMona m
  where
    runPure :: BaseP t t -> m a
    runPure = fmap f . sequence . fmap (cata f g)
    
    runMona :: BaseM t (m t) -> m a
    runMona = g <=< traverse (fmap (cata f g))

---

data Expr
  = Lit Int
  | Add Expr Expr
  | Random Expr Expr

data ExprP x
  = LitF Int
  | AddF x x
  deriving (Functor, Foldable, Traversable)

data ExprM x = RandomF x x
  deriving (Functor, Foldable, Traversable)

type instance BaseP Expr = ExprP
type instance BaseM Expr = ExprM

-- | Unravel the recursion one step
instance Recurse Expr where
  project :: Applicative m => Expr -> MixF (BaseP Expr) (BaseM Expr) m Expr
  project (Lit i) = Pure (LitF i)
  project (Add x y) = Pure (AddF x y)
  project (Random low high) = Monadic (RandomF (pure low) (pure high))

evalP :: BaseP Expr Int -> Int
evalP (LitF i) = i
evalP (AddF x y) = x + y

evalM :: MonadRandom m => BaseM Expr (m Int) -> m Int
evalM (RandomF mlow mhigh) = do
  low <- mlow
  high <- mhigh
  getRandomR (low, high)

example :: Expr
example = Add (Lit 1) (Add (Random (Lit 0) (Lit 10)) (Lit 3))

main :: IO ()
main = do
  gen <- getStdGen
  print $ evalRand (cata evalP evalM example) gen
