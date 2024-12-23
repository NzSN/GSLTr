{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module AstGen (code_gen) where

import Debug.Trace (trace)

import Data.Aeson (decode, Object, (.:), (.:?))
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Maybe (catMaybes, fromJust, isJust)

type Rule     = Object
type NodeType = String
type IsNamed  = Bool

data ASTLiteral = ASTLiteral { get_source :: String } |
                  ASTEmpty deriving (Show, Eq)
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
    generate (Just rules) = foldl' (\s r -> s <> generate_rule r) (Just $ ASTLiteral "") rules

    generate_rule :: Rule -> Maybe ASTLiteral
    generate_rule r =
      let v = flip parseMaybe r $ \obj -> do
                t <- obj .: "type"
                anonymous <- obj .: "named"
                subnodes <- obj .:? "children"

                case ((trace ("Sub:" ++ show subnodes) subnodes) :: Maybe Rule) of
                  -- Leaf node
                  Nothing -> return $ generate_leaf t anonymous
                  -- Internal node
                  Just subnodes_ -> return $ generate_node t subnodes_
      in case v of
        Nothing -> Nothing
        Just v'  -> Just $ v'

    generate_leaf :: NodeType -> IsNamed -> ASTLiteral
    generate_leaf nt True = ASTLiteral $ "data  " ++ nt ++ " a = { ann : a, text : Data.Text.Internal.Text }"
    generate_leaf nt False = ASTLiteral $ "type Anonymous_" ++ anonymousIdent nt ++ " = " ++  "Token " ++ nt

    generate_node :: NodeType -> Rule -> ASTLiteral
    generate_node nt sr =
      let v = flip parseMaybe sr $ \obj -> do
            children_types <- obj .: "types"
            return $ generate_children children_types
      in case v of
        Nothing              -> ASTEmpty
        Just (ASTEmpty)      -> ASTEmpty
        Just (ASTLiteral v') -> ASTLiteral $
          "data " ++ nt ++ " = " ++ "{ ann: a, extraChildren:" ++ v' ++ " }"

    anonymousIdent :: NodeType -> String
    anonymousIdent nt
      | nt == "{"  = "left_brace"
      | nt == "}"  = "right_brace"
      | nt == "|"  = "pipe"
      | nt == "|=" = "pipe_equal"
      | nt == "||" = "pipe_pipe"
      | nt == "~"  = "Tilde"
      | otherwise  = nt


    generate_children :: [Rule] -> ASTLiteral
    generate_children r = do
      foldl' (\s r -> ASTLiteral "AA") (ASTLiteral "") r
