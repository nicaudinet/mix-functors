{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.Functor.Mixfix where

import Control.Monad ((<=<))
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
