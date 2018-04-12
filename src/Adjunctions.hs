{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE TypeOperators          #-}

module Adjunctions where

import           Control.Monad

class (Functor f, Functor g) => Adjunction f g | f -> g, g -> f where

  {-# MINIMAL (unit, counit) | (leftAdjunct, rightAdjunct) #-}

  -- leftAdjunct . rightAdjunct === id
  -- rightAdjunct . leftAdjunct === id

  leftAdjunct  :: (f a -> b) -> a -> g b
  leftAdjunct f = fmap f . unit

  rightAdjunct :: (a -> g b) -> f a -> b
  rightAdjunct f = counit . fmap f

  -- leftAdjunct counit === id
  -- rightAdjunct unit === id

  -- g.f is a monad

  unit :: a -> g (f a)
  unit = leftAdjunct id

  join :: g (f (g (f a))) -> g (f a)
  join = fmap counit

  -- f.g is a comonad

  counit :: f (g a) -> a
  counit = rightAdjunct id

  duplicate :: f (g a) -> f (g (f (g a)))
  duplicate = fmap unit

  -- uninteresting (I think)

  join' :: f (g (f (g a))) -> f (g a)
  join' = (fmap . fmap) counit

  duplicate' :: g (f a) -> g (f (g (f a)))
  duplicate' = (fmap . fmap) unit

instance Adjunction ((,) e) ((->) e) where
  -- leftAdjunct :: ((e, a) -> b) -> a -> e -> b
  -- rightAdjunct :: (a -> e -> b) -> (e, a) -> b
  leftAdjunct  f a e   = f (e,a)
  rightAdjunct f (e,a) = f a e

infixr 7 :.:
newtype (:.:) (f :: k2 -> *) (g :: k1 -> k2) (p :: k1) = Comp1 { unComp1 :: f (g p) }
    deriving (Functor)

instance Adjunction f g => Applicative (g :.: f) where
  pure  = return
  (<*>) = ap

instance Adjunction f g => Monad (g :.: f) where
  return  = Comp1 . unit
  x >>= f = Comp1 (fmap (rightAdjunct (unComp1 . f)) (unComp1 x))

type MyState s = ((->) s :.: (,) s)
