{-# LANGUAGE OverloadedStrings #-}

module AST.Parse where

import Data.ByteString (ByteString)
import Foreign.Ptr (Ptr)
import TreeSitter.Language as TL

import AST.AST (AST)

parseBytestring :: Ptr TL.Language -> ByteString -> IO AST
parseBytestring = undefined
