{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module AstGen (code_gen) where

import Debug.Trace (trace)

import Data.Semigroup (Semigroup(sconcat))
import Data.Monoid (Monoid(mempty,mconcat))

import Data.Aeson (decode, Object, (.:), (.:?))
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Maybe

type Rule     = Object
type NodeType = String
type IsNamed  = Bool

newtype ASTLiteral = ASTLiteral { get_source :: String } deriving (Show, Eq)
instance Semigroup ASTLiteral where
  l <> r = ASTLiteral $ get_source l ++ get_source r
instance Monoid ASTLiteral where
  mempty = ASTLiteral ""
  mconcat = foldr (<>) mempty


code_gen :: ByteString -> Maybe ASTLiteral
code_gen bs = generate (decode bs :: Maybe [Rule])
  where
    generate :: Maybe [Rule] -> Maybe ASTLiteral
    generate Nothing      = Just $ ASTLiteral ""
    generate (Just rules) =
      let a = foldl' (\s r -> s <> generate_rule r) (Just $ ASTLiteral "") rules
      in trace (show a) a

    generate_rule :: Rule -> Maybe ASTLiteral
    generate_rule r =
      let v = flip parseMaybe r $ \obj -> do
                t <- obj .: "type"
                anonymous <- obj .: "named"
                subnodes <- obj .:? "children"

                case (subnodes :: Maybe String) of
                  -- Leaf node
                  Nothing -> return $ generate_leaf t anonymous
                  -- Internal node
                  Just _ -> return $ generate_node r
      in case v of
        Nothing -> Nothing
        Just v'  -> Just $ v'

    generate_leaf :: NodeType -> IsNamed -> ASTLiteral
    generate_leaf = undefined

    generate_node :: Rule -> ASTLiteral
    generate_node = undefined
