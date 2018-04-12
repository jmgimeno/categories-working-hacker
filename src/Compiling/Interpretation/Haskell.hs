{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE Rank2Types     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Compiling.Interpretation.Haskell where

import           Compiling.Categories              ()
import           Compiling.Interpretation.Abstract ()
import           Compiling.Language

instance Interpreted (->) where
  run = ($)

interpret :: Term a -> a
interpret t = (run :: (h -> a) -> h -> a) t ()
