module Main (main) where

import Test.HUnit

import Data.ByteString.Lazy.Char8 (pack)
import AstGen (code_gen)
import Data.Maybe (isNothing)

test1 :: Test
test1 = TestCase (assert $ not $ isNothing $ code_gen (pack source))
  where source =
          "[\n\
          \{ \"type\": \"access_mode\", \n\
          \   \"named\": true, \n\
          \   \"fields\": {}}  \n\
          \]"

tests :: Test
tests = TestList [TestLabel "test1" test1]

main :: IO Counts
main = runTestTT tests
