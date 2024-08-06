{-# LANGUAGE DeriveFunctor #-}

module Data.Functor.Mix
  ( Mix(..)
  , mix
  , mixM
  ) where

data Mix f g x
  = F (f x)
  | G (g x)
  deriving Functor

mix :: (f a -> a) -> (g a -> a) -> Mix f g a -> a
mix f _ (F x) = f x
mix _ g (G x) = g x

mixM
  :: (Monad m, Traversable f)
  => (f a -> a)
  -> (g (m a) -> m a)
  -> Mix f g (m a)
  -> m a
mixM f _ (F x) = fmap f (sequence x)
mixM _ g (G x) = g x
