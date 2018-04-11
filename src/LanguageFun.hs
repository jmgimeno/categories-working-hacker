{-# LANGUAGE Rank2Types #-}

module LanguageFun where

import Language

instance Language (->) where
  here   (a, _)   = a
  before f (_, h) = f h
  lambda f h a    = f (a, h)
  apply  f g h    = f h (g h) 

  loop   f h      = f h a where a = loop f h 

  int    n _      = n
  add    f g h    = f h + g h
  mult   f g h    = f h * g h
  down   f h      = f h - 1
  up     f h      = f h + 1
  gte    f g h    = f h >= g h

  bool   b _      = b
  and    f g h    = f h && g h
  or     f g h    = f h || g h
  neg    f h      = not (f h)

  ifte   b t e h  = if b h then t h else e h

instance Interpreted (->) where
  run f h = f h

interpret :: Term a -> a
interpret t = (run :: (h -> a) -> h -> a) t ()
