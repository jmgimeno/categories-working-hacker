{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module CatAdjunctions where

import           Control.Category
import           Control.Monad     ((<=<))
import           Prelude           hiding ((.))

class (Category x, Category y) => CatFunctor x f y where
  catMap :: a `x` b -> f a `y` f b

{- Puzzle 1:

    catMap id = id
    catMap (f . g) = catMap f . catMap g
-}

type EndoFunctor k f = CatFunctor k f k

type HaskFunctor f = EndoFunctor (->) f

instance Functor f => CatFunctor (->) f (->) where
  catMap = fmap
  
-- Needs UndecidableInstances and -fno-warn-orphans
instance EndoFunctor (->) f => Functor f where
  fmap = catMap

class ( CatFunctor x f y
      , CatFunctor y g x
      ) => CatAdjunction x y f g where
  catLeftAdjunct  :: (f a `y`   b) -> (a   `x` g b)
  catRightAdjunct :: (a   `x` g b) -> (f a `y`   b)

-- Puzzle 2:

newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

instance Monad m => Category (Kleisli m) where
  id = Kleisli return
  (Kleisli f) . (Kleisli g) = Kleisli $ f <=< g

-- Puzzle 3:

newtype LeftK a = LeftK { unLeftK :: a }

instance Monad m => CatFunctor (->) LeftK (Kleisli m) where
  catMap = Kleisli . (pure .) . haskCatMap
    where
      haskCatMap f (LeftK x) = LeftK (f x)

newtype RightK m b = RightK { unRightK :: m b }

instance Monad m => CatFunctor (Kleisli m) (RightK m) (->) where
  catMap (Kleisli f) (RightK a) = RightK $ a >>= f

instance Monad m => CatAdjunction (->) (Kleisli m) LeftK (RightK m) where
  catLeftAdjunct  f = RightK . runKleisli f . LeftK
  catRightAdjunct f = Kleisli $ unRightK . f . unLeftK

-- Defining Identity to not depend on mtl

newtype Identity a = Identity { runIdentity :: a }

instance Monad m => CatFunctor (->) Identity (Kleisli m) where
  catMap = Kleisli . (pure .) . fmap

instance Monad m => CatFunctor (Kleisli m) m (->) where
  catMap (Kleisli f) ma = ma >>= f

instance Monad m => CatAdjunction (->) (Kleisli m) Identity m where
  catLeftAdjunct f = runKleisli f . Identity
  catRightAdjunct f = Kleisli $ f . runIdentity
