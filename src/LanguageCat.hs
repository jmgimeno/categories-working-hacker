{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
    
module LanguageCat where
    
import Prelude hiding (id, (.), curry, uncurry, const)
    
import CompilingToCategories
import Language
      
-- In the code below 
-- find a concrete value for `l`
-- and elide all the mocks
    
swap :: Cartesian k => (a, b) `k` (b, a)
swap = exr |*| exl
    
instance (Closed k
         ,ConstCat k, NumCat k Int, OrdCat k Int
         , BoolCat k
         ) => Language k where
  here       = exl
  before f   = f . exr
  lambda f   = curry (f . swap)
  apply  f g = applyC . (f |*| g)
    
  loop   f   = applyC . ( f |*| loop f)
    
  int        = unitArrow
  add a b    = addC . (a |*| b)
  mult a b   = mulC . (a |*| b)
  down a     = add a (int (-1))
  up a       = add a (int 1)
  gte a b    = gteC . (a |*| b)
    
  bool       = unitArrow
  and a b    = andC . (a |*| b)
  or a b     = orC . (a |*| b)
  neg a      = notC . a
    
  ifte b t e = ifteC . (b |*| t |*| e)

{-   
instance (Closed k 
         ,ConstCat k Int, NumCat k Int, OrdCat k Int
         ,ConstCat k Bool, BoolCat k) 
         => Interpreted k where
  run = undefined
-}    
--interpret :: Term a -> a
--interpret t = (run :: h `l` a -> h -> a) t ()