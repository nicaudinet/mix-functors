{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Monad.Random (MonadRandom, Random, getRandomR, evalRandIO) 
import Data.Functor.Mix (Mix(..), mixM)
import Data.Functor.Foldable (cata)
import Data.Fix (Fix(..))

-----------
-- Types --
-----------

data CalcF a x
  = LitF a
  | AddF x x
  | MulF x x
  deriving (Functor, Foldable, Traversable)

data RandF x
  = RandF x x
  deriving Functor

type CalcRand a = Fix (Mix (CalcF a) RandF)

------------------
-- Construction --
------------------

instance Num a => Num (CalcRand a) where
  fromInteger = Fix . F . LitF . fromInteger
  x + y = Fix (F (AddF x y))
  x * y = Fix (F (MulF x y))
  abs = undefined
  signum = undefined
  negate = undefined

rand :: CalcRand a -> CalcRand a -> CalcRand a
rand x y = Fix (G (RandF x y))

example :: Num a => CalcRand a
example = 1 + (rand 0 10) + 3 * (rand 0 (4 * 5))

----------------
-- Evaluation --
----------------

evalCalcAlg :: Num a => CalcF a a -> a
evalCalcAlg (LitF a) = a
evalCalcAlg (AddF x y) = x + y
evalCalcAlg (MulF x y) = x * y

evalRandAlg :: (MonadRandom m, Random a) => RandF (m a) -> m a
evalRandAlg (RandF mx my) = do
  x <- mx
  y <- my
  getRandomR (x, y)

main :: IO ()
main = do
  result <- evalRandIO $ cata (mixM evalCalcAlg evalRandAlg) example :: IO Int
  print $ result
