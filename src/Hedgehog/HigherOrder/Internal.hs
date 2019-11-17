{-# LANGUAGE
    AllowAmbiguousTypes,
    ConstraintKinds,
    DeriveFunctor,
    FlexibleContexts,
    RankNTypes,
    ScopedTypeVariables,
    TypeApplications,
    TypeOperators #-}

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Hedgehog.HigherOrder.Internal where

import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Functor.Identity (Identity(..))
import GHC.Generics

import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Gen (GenT(..), Gen)
import Hedgehog.Internal.Range (Size)
import qualified Hedgehog.Internal.Seed as Seed
import Hedgehog.Internal.Tree (TreeT(..), NodeT(..))

import qualified Test.Fun as Fun
import Test.Fun (FunName, TypeName, (:+)(..))

type Maybe_ = MaybeT Identity

-- * QuickCheck's Gen

newtype LazyGen a = LazyGen
  { unLazyGen :: Size -> Seed.Seed -> a }
  deriving Functor

instance Applicative LazyGen where
  pure a = LazyGen (\_ _ -> a)
  gf <*> gx = do
    f <- gf
    f <$> gx

instance Monad LazyGen where
  return = pure
  LazyGen u >>= k = LazyGen (\sz sd ->
    case Seed.split sd of
      (sk, sm) -> unLazyGen (k (u sz sm)) sz sk)

lazyGen :: Gen a -> LazyGen (TreeT Maybe_ a)
lazyGen (GenT f) = LazyGen f

runLazyGen :: LazyGen a -> Gen a
runLazyGen (LazyGen f) = GenT (\sz sd -> pure (f sz sd))

choose :: Int -> LazyGen Int
choose n = LazyGen (\_ sd -> (fromInteger . fst . Seed.nextInteger 0 (toInteger n)) sd)

-- * Testable functions

data a :-> b = MkFun (a Fun.:-> TreeT Maybe_ b) b

infixr 1 :->

applyFun :: (a :-> r) -> a -> r
applyFun (MkFun h r) a = case (runIdentity . runMaybeT . runTreeT . Fun.applyFun h) a of
  Just t -> nodeValue t
  Nothing -> r

applyFun2 :: (a :-> b :-> r) -> a -> b -> r
applyFun2 h a b = h `applyFun` a `applyFun` b

applyFun3 :: (a :-> b :-> c :-> r) -> a -> b -> c -> r
applyFun3 h a b c = h `applyFun` a `applyFun` b `applyFun` c

showsPrecFun :: Fun.ShowsPrec r -> Fun.ShowsPrec (a :-> r)
showsPrecFun showsPrec_ n (MkFun f r) =
  Fun.showsPrecFun (\m -> showsPrec_ m . unTree r) n f

unTree :: r -> TreeT Maybe_ r -> r
unTree _ (TreeT (MaybeT (Identity (Just t)))) = nodeValue t
unTree r _ = r

instance Show b => Show (a :-> b) where
  showsPrec = showsPrecFun showsPrec

shrinkTree :: TreeT Maybe_ r -> [TreeT Maybe_ r]
shrinkTree t0 = case (runIdentity . runMaybeT . runTreeT) t0 of
  Just t -> nodeChildren t
  Nothing -> []

-- * Cogenerators

newtype CoGen a = CoGen
  { unCoGen :: forall r. LazyGen r -> LazyGen (a Fun.:-> r) }

genFun :: CoGen a -> Gen b -> Gen (a :-> b)
genFun ca gb = MkFun <$> gf <*> gb where
  gf = Gen.shrink (Fun.shrinkFun shrinkTree) (runLazyGen (unCoGen ca (lazyGen gb)))

cogenEmbed :: FunName -> (a -> b) -> CoGen b -> CoGen a
cogenEmbed fn f (CoGen c) = CoGen (Fun.cogenEmbed fn f c)

cogenIntegral :: Integral a => TypeName -> CoGen a
cogenIntegral tn = CoGen (Fun.cogenIntegral tn)

cogenIntegral' :: Integral a => TypeName -> (a -> Integer) -> CoGen a
cogenIntegral' tn f = CoGen (Fun.cogenIntegral' tn f)

-- | This is equivalent to 'coarbitrary'.
cogenInteger :: CoGen Integer
cogenInteger = cogenIntegral "Integer"

-- | This is equivalent to 'coarbitrary'.
cogenInt :: CoGen Int
cogenInt = cogenIntegral "Int"

concreteTree :: Show a => Fun.Concrete (NodeT Maybe_ a)
concreteTree = Fun.Concrete shrink' showsPrec' where
  shrink' t = [ u | TreeT (MaybeT (Identity (Just u))) <- nodeChildren t ]
  showsPrec' n = showsPrec n . nodeValue

cogenFun :: Show a0 =>
  Gen a0 ->
  (a0 -> a) ->
  CoGen b ->
  CoGen (a -> b)
cogenFun g f (CoGen cb) = CoGen cf where
  cf = Fun.cogenFun concreteTree ga (f . nodeValue) cb
  ga = do
    i <- choose 4
    if i == 0 then
      pure Nothing
    else
      (runIdentity . runMaybeT . runTreeT) <$> lazyGen g

cogenGeneric :: forall a. (Generic a, Fun.GCoGen a) => GSumCoGen a -> CoGen a
cogenGeneric gs = CoGen
  (Fun.cogenGeneric (gs @r) :: forall r. LazyGen r -> LazyGen (a Fun.:-> r))

(<?) :: CoGen a -> (z -> LazyGen r) -> (z -> LazyGen (a Fun.:-> r))
(<?) (CoGen c) = fmap c

infixr 3 <?

cogenList :: CoGen a -> CoGen [a]
cogenList c = self where
  self = cogenGeneric (id :+ (c <? self <? id) :+ ())

type GSumCoGen a = forall r. Fun.GSumCo LazyGen a r

type CoArbitrary_ = Fun.CoArbitrary LazyGen

coarbitrary :: CoArbitrary_ a => CoGen a
coarbitrary = CoGen Fun.coarbitrary
