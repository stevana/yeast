module Main (main) where

import Test.Tasty (testGroup, defaultMain)

import Properties (properties)
import UnitTests  (unitTests)

------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "Tests" [properties, unitTests]
