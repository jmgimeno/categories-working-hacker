{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- After getting rid of mocks, shouldn't need these two below
-- {-# LANGUAGE FlexibleInstances   #-}
-- {-# LANGUAGE MonoLocalBinds      #-}

module Language where

import Prelude hiding (and, or)

class Language k where
  here   :: (a, h) `k` a
  before :: h `k` a -> (any, h) `k` a
  lambda :: (a, h) `k` b -> h `k` (a -> b)
  apply  :: h `k` (a -> b) -> (h `k` a -> h `k` b)

  loop   :: h `k` (a -> a) -> h `k` a

  int    :: Int -> h `k` Int
  add    :: h `k` Int -> h `k` Int -> h `k` Int
  down   :: h `k` Int -> h `k` Int
  up     :: h `k` Int -> h `k` Int
  mult   :: h `k` Int -> h `k` Int -> h `k` Int
  gte    :: h `k` Int -> h `k` Int -> h `k` Bool

  bool   :: Bool -> h `k` Bool
  and    :: h `k` Bool -> h `k` Bool -> h `k` Bool
  or     :: h `k` Bool -> h `k` Bool -> h `k` Bool
  neg    :: h `k` Bool -> h `k` Bool

  ifte   :: h `k` Bool -> h `k` a -> h `k` a -> h `k` a

class Language k => Interpreted k where
  run :: h `k` a -> h -> a

type Term a = forall k h . Language k => h `k` a
