{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Test.Hspec (hspec, describe, it)
import Test.ShouldNotTypecheck (shouldNotTypecheck)

import ConstrainedArrays
import Data.Proxy

arr :: Arr D2 (RectiLinear (100 ':- 50 ':- Iz)) Float
arr = undefined

main :: IO ()
main = hspec $ do
  describe "Bounds tests" $ do
    it "Simple rectilinear arrays" $ do
      shouldNotTypecheck $ arr `at` (Proxy @(100 ':- 9 ':- Iz))
      shouldNotTypecheck $ arr `at` (Proxy @(10 ':- 100 ':- Iz))

    it "Transposition" $ do
      let transposed = transpose arr
      shouldNotTypecheck $ transposed `at` (Proxy @(75 ':- 25 ':- Iz))

    it "Upper / lower triangular matrices" $ do
      let (lower, upper) = split @_ @LowerTrig arr
      shouldNotTypecheck $ lower `at` (Proxy @(10 ':- 20 ':- Iz))
      shouldNotTypecheck $ upper `at` (Proxy @(20 ':- 10 ':- Iz))
      shouldNotTypecheck $ upper `at` (Proxy @(10 ':- 10 ':- Iz))
