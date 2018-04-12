{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE Rank2Types     #-}

module Main where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Compiling.Interpretation.Haskell (interpret)
import           Compiling.Language


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

ex1 :: Int
ex1 = interpret (apply (lambda here) (int 6))

ex2 :: Bool
ex2 = interpret (apply (lambda here) (bool True))

ex3 :: Int
ex3 = interpret (ifte (bool True) (int 3) (int 4))

ex4 :: Int -> Int -> Int
ex4 = interpret (lambda (lambda (add (before here) here)))

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "(\\x -> x) 6 === 6" $
      interpret (apply (lambda here) (int 6)) @?= 6
  , testCase "(\\x -> x) True === True" $
      interpret (apply (lambda here) (bool True)) @?= True
  , testCase "if True then 3 else 4 === 3" $
      interpret (ifte (bool True) (int 3) (int 4)) @?= (3 :: Int)
  , testCase "(\\x -> \\ y -> (+) x y) 1 2 === 3" $
      interpret (lambda (lambda (add (before here) here))) 1 2 @?= (3 :: Int)
  ]

