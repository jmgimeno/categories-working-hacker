{-# LANGUAGE Rank2Types #-}

module Classical where

-- `efq` stands for *ex falso quodlibet*
newtype Falsum = Falsum { efq :: forall a . a } 

-- John Baez's definition of negation
newtype Not a = Not { unNot :: a -> Falsum }

contraposition :: (a -> b) -> Not b -> Not a
contraposition = (flip . curry . (Not .) . uncurry . ((.) .)) unNot

contraposition' :: (a -> b) -> (b -> Falsum) -> a -> Falsum
contraposition' = (flip . curry . uncurry) (.)

newtype Classical a = Thm { getProof :: Not (Not a) }

instance Functor Classical where
  fmap = ( flip 
         . curry 
         . (Thm .) 
         . uncurry 
         . (flip (contraposition . contraposition) .)
         ) getProof
         
toDoubleNeg :: Classical a -> (a -> Falsum) -> Falsum
toDoubleNeg = flip (flip (unNot . getProof) . Not)

fromDoubleNeg :: ((a -> Falsum) -> Falsum) -> Classical a
fromDoubleNeg = Thm . Not . contraposition' unNot

instance Applicative Classical where
  pure  = Thm . Not . flip unNot
  (<*>) = ( curry 
          . (fromDoubleNeg .) 
          . uncurry 
          . (. toDoubleNeg) 
          . flip 
          . (. toDoubleNeg) 
          . (contraposition' .) 
          . flip 
          . (contraposition' .) 
          . flip
          ) contraposition'
          
ax1 :: Classical (a -> b -> a)
ax1 = pure pure

ax2 :: Classical ((a -> b -> c) -> (a -> b) -> a -> c)
ax2 = pure (<*>)

unApply :: (Classical a -> Classical b) -> Classical (a -> b)
unApply = ( fromDoubleNeg
          . ((<*>) (<*>) ((<*>) pure))
          . ( curry
            . flip
            ) ( ( flip
                . uncurry
                . flip
                . (flip ($) .)
                ) (. (efq .))
              . (. pure)
              )
          ) . (toDoubleNeg .) . (. fromDoubleNeg)
          
ax3 :: Classical ((Not (Not a)) -> a)
ax3 = (. (toDoubleNeg . Thm))
        <$> unApply ( fromDoubleNeg
                    . flip ((flip ($)) . toDoubleNeg . pure)
                    . toDoubleNeg)
                    
instance Monad Classical where
  return = pure
  (>>=) = undefined
  
          