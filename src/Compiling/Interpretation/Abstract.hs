{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Compiling.Interpretation.Abstract where

import           Prelude              hiding (const, curry, (.))

import           Compiling.Categories
import           Compiling.Language

swap :: Cartesian k => (a, b) `k` (b, a)
swap = exr |*| exl

instance ( Closed k
         , ConstCat k
         , NumCat k Int
         , OrdCat k Int
         , BoolCat k
         ) => Language k where
  here       = exl
  before f   = f . exr
  lambda f   = curry (f . swap)
  apply  f g = applyC . (f |*| g)

  loop   f   = applyC . ( f |*| loop f)

  int        = const
  add a b    = addC . (a |*| b)
  mult a b   = mulC . (a |*| b)
  down a     = add a (int (-1))
  up a       = add a (int 1)
  gte a b    = gteC . (a |*| b)

  bool       = const
  and a b    = andC . (a |*| b)
  or a b     = orC . (a |*| b)
  neg a      = notC . a

  ifte b t e = ifteC . (b |*| t |*| e)
