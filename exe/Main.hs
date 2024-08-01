{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Proxy
import Control.Monad.Random

-- data Fix f = Fix (f (Fix f))

data Expr
  = Lit Int
  | Add Expr Expr
  | Random Expr Expr

data PureF x = LitF Int | AddF x x

instance Functor PureF where
  fmap :: (a -> b) -> PureF a -> PureF b
  fmap _ (LitF x) = LitF x
  fmap f (AddF x y) = AddF (f x) (f y)

instance Foldable PureF where
  foldMap :: Monoid m => (a -> m) -> PureF a -> m
  foldMap _ (LitF _) = mempty
  foldMap f (AddF x y) = f x <> f y

instance Traversable PureF where
  traverse :: Applicative f => (a -> f b) -> PureF a -> f (PureF b)
  traverse _ (LitF x) = pure (LitF x)
  traverse f (AddF x y) = AddF <$> f x <*> f y

data MonaF x = RandomF x x

instance Functor MonaF where
  fmap :: (a -> b) -> MonaF a -> MonaF b
  fmap f (RandomF low high) = RandomF (f low) (f high)

instance Foldable MonaF where
  foldMap :: Monoid m => (a -> m) -> MonaF a -> m
  foldMap f (RandomF low high) = f low <> f high

instance Traversable MonaF where
  traverse :: Applicative f => (a -> f b) -> MonaF a -> f (MonaF b)
  traverse f (RandomF low high) = RandomF <$> f low <*> f high

data ExprF m x
  = Pure (PureF x)
  | Mona (MonaF (m x))

instance Functor m => Functor (ExprF m) where
  fmap f (Pure p) = Pure (fmap f p)
  fmap f (Mona m) = Mona (fmap (fmap f) m)

-- | Unravel the recursion one step
project :: Applicative m => Proxy m -> Expr -> ExprF m Expr
project _ (Lit i) = Pure (LitF i)
project _ (Add x y) = Pure (AddF x y)
project _ (Random low high) = Mona (RandomF (pure low) (pure high))

computePure :: PureF Int -> Int
computePure (LitF i) = i
computePure (AddF x y) = x + y

computeMona :: MonadRandom m => MonaF (m Int) -> m Int
computeMona (RandomF mlow mhigh) = do
  low <- mlow
  high <- mhigh
  getRandomR (low, high)

cata :: forall m. MonadRandom m => Expr -> m Int
cata expr = case project (Proxy :: Proxy m) expr of
  Pure p -> runPure p
  Mona m -> runMona m
  where
    runPure :: Monad m => PureF Expr -> m Int
    runPure = fmap computePure . sequence . fmap cata

    runMona :: Monad m => MonaF (m Expr) -> m Int
    runMona = computeMona <=< traverse (fmap cata)

example :: Expr
example = Add (Lit 1) (Add (Random (Lit 0) (Lit 10)) (Lit 3))

main :: IO ()
main = do
  gen <- getStdGen
  print $ evalRand (cata example) gen
