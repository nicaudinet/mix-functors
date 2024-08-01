{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad.Random
import Data.Functor.Mixfix

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

instance Num Expr where
  fromInteger = Lit . fromInteger
  (+) = Add
  (*) = undefined
  abs = undefined
  signum = undefined
  negate = undefined

evalP :: BaseP Expr Int -> Int
evalP (LitF i) = i
evalP (AddF x y) = x + y

evalM :: MonadRandom m => BaseM Expr (m Int) -> m Int
evalM (RandomF mlow mhigh) = do
  low <- mlow
  high <- mhigh
  getRandomR (low, high)

rand :: Expr -> Expr -> Expr
rand = Random

example :: Expr
example = 1 + (rand 0 10) + 3

main :: IO ()
main = do
  gen <- getStdGen
  print $ evalRand (cata evalP evalM example) gen
