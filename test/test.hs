{-# LANGUAGE OverloadedStrings, TypeApplications, TypeOperators #-}

module Main where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Hedgehog.HigherOrder

good :: (Eq a, Show a) => Gen ((a -> a) :-> a) -> Property
good g = property $ do
  f <- applyFun <$> forAll g
  f id === f id

bad :: (Eq a, Show a) => Gen ((a -> a) :-> a) -> Gen (a :-> a) -> Property
bad gf gg = property $ do
  f <- applyFun <$> forAll gf
  g <- applyFun <$> forAll gg
  annotate "Failure is to be expected."
  f id === f g

cobad :: (Eq a, Show a) => Gen ((a -> a) :-> a) -> Gen (a :-> a) -> Property
cobad gf gg = withTests 1 $ property $ do
  b <- check (bad gf gg)
  assert (not b)

genInt :: Gen Int
genInt = Gen.integral (Range.linear 0 10)

genList :: Gen [Int]
genList = Gen.list (Range.linear 0 10) genInt

type C a = CoGen (a -> a)
type G a = Gen ((a -> a) :-> a)

cogenFunInt :: C Int
cogenFunInt = cogenFun genInt id cogenInt

genFunInt :: G Int
genFunInt = genFun cogenFunInt genInt

cogenFunBool :: C Bool
cogenFunBool = cogenFun Gen.bool id coarbitrary

genFunBool :: G Bool
genFunBool = genFun cogenFunBool Gen.bool

cogenFunList :: C [Int]
cogenFunList = cogenFun genList id (cogenList cogenInt)

genFunList :: G [Int]
genFunList = genFun cogenFunList genList

genEndoInt :: Gen (Int :-> Int)
genEndoInt = genFun cogenInt genInt

genEndoBool :: Gen (Bool :-> Bool)
genEndoBool = genFun coarbitrary Gen.bool

genEndoList :: Gen ([Int] :-> [Int])
genEndoList = genFun coarbitrary genList


main :: IO Bool
main =
  checkParallel $ Group "test"
    [ ("good-Int" , good genFunInt)
    , ("good-Bool", good genFunBool)
    , ("good-List", good genFunList)
    , ("bad-Int" , cobad genFunInt genEndoInt)
    , ("bad-Bool", cobad genFunBool genEndoBool)
    , ("bad-List", cobad genFunList genEndoList)
    ]
