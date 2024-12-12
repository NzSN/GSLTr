{-# LANGUAGE OverloadedStrings #-}

module AST.Parse where

import Data.ByteString (ByteString)
import Foreign.Ptr (Ptr)
import TreeSitter.Language as TL

parseBytestring :: Ptr TL.Language -> ByteString -> IO a
parseBytestring = undefined
