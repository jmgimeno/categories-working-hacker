{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module CompilingToCategories where

import Prelude hiding (id, (.), curry, uncurry, const)

-- Classes 

infixr 9 .
class Category k where
  id :: a `k` a
  (.) :: (b `k` c) -> (a `k` b) -> (a `k` c)
  
infixr 3 |*|
class Category k => Cartesian k where
  (|*|) :: (a `k` c) -> (a `k` d) -> (a `k` (c, d))
  exl :: (a, b) `k` a
  exr :: (a, b) `k` b
  
-- How to generalize () for any category?
class Category k => Terminal k u | k -> u where
  it :: a `k` u
  
class Cartesian k => Closed k where
  applyC :: ((a -> b), a) `k` b
  curry :: ((a, b) `k` c) -> (a `k` (b -> c))
  uncurry :: (a `k` (b -> c)) -> ((a, b)  `k` c)
  
class ConstCat k where
  unitArrow :: b -> (a `k` b)
  
class Cartesian k => BoolCat k where
  notC :: Bool `k` Bool
  andC, orC :: (Bool, Bool) `k` Bool
  ifteC :: (Bool, (a, a)) `k` a
  
class NumCat k a where
  negateC :: a `k` a
  addC, mulC :: (a, a) `k` a

class OrdCat k a where
  gteC :: (a, a) `k` Bool
  
-- Instances

instance Category (->) where
  id = \x -> x
  g . f = \x -> g (f x)
  
instance Cartesian (->) where
  f |*| g = \x -> (f x, g x)
  exl = \(a, b) -> a
  exr = \(a, b) -> b
  
instance Terminal (->) () where
  it = \a -> ()
  
instance Closed (->) where
  applyC (f, a) = f a
  curry f = \a b -> f (a, b)
  uncurry f = \(a, b) -> f a b

instance ConstCat (->) where
  unitArrow b = \_ -> b
  
instance BoolCat (->) where
  notC = not
  andC = uncurry (&&)
  orC  = uncurry (||)
  ifteC = \(b, (t, e)) -> if b then t else e
  
instance Num a => NumCat (->) a where
  negateC = negate
  addC = uncurry (+)
  mulC = uncurry (*)

instance Ord a => OrdCat (->) a where 
  gteC = uncurry (>=)
