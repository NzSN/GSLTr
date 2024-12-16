{-# LANGUAGE ForeignFunctionInterface #-}

module GSLTr (someFunc) where

import TreeSitter.Wgsl (tree_sitter_wgsl)

someFunc :: IO ()
someFunc = do
  print tree_sitter_wgsl
