module Main (main) where


import Debug.Trace (trace)
import Test.HUnit

import Data.ByteString.Lazy.Char8 (pack)
import AstGen (code_gen)
import Data.Maybe (isNothing)

namedLeaf :: Test
namedLeaf = TestCase (assert $ not $ isNothing $ code_gen (pack source))
  where source =
          "[\n\
          \{ \"type\": \"access_mode\", \n\
          \   \"named\": true, \n\
          \   \"fields\": {}}  \n\
          \]"
anonyLeaf :: Test
anonyLeaf = TestCase (assert $ not $ isNothing $ code_gen (pack source))
  where source =
          "[\n\
          \{ \"type\": \"access_mode\", \n\
          \   \"named\": false, \n\
          \   \"fields\": {}}  \n\
          \]"
internalNode :: Test
internalNode =
  let v = code_gen (pack source)
  in TestCase (assert $ not $ isNothing $ (trace (show v) v))
  where source = "[{ \"type\": \"A\", \"named\": true, \"children\": { \"types\": [ { \"type\": \"B\", \"named\": true } ] }}]"

tests :: Test
tests = TestList [TestLabel "NamedLeaf" namedLeaf,
                  TestLabel "AnonyLeaf" anonyLeaf,
                  TestLabel "Internal Node" internalNode]

main :: IO Counts
main = runTestTT tests
