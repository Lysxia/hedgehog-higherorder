module Hedgehog.HigherOrder
  ( (:->)
  , applyFun
  , applyFun2
  , applyFun3
  , showsPrecFun

  , CoGen()
  , genFun
  , cogenEmbed
  , cogenIntegral
  , cogenIntegral'
  , cogenInteger
  , cogenInt
  , cogenList
  , cogenFun

  , cogenGeneric
  , (:+)(..)
  , (<?)
  , LazyGen()
  , coarbitrary
  , CoArbitrary_

  ) where

import Test.Fun ((:+)(..))
import Hedgehog.HigherOrder.Internal
