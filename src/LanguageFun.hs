{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MonoLocalBinds #-}
    
module LanguageFun where
      
import CompilingToCategories
import Language
import LanguageCat
    
instance Interpreted (->) where
  run f h = f h
    
interpret :: Term a -> a
interpret t = (run :: (h -> a) -> h -> a) t ()