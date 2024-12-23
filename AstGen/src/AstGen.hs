{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module AstGen (code_gen) where

import Data.List as Li (uncons)
import Data.Aeson (decode, Object, (.:), (.:?))
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB_Char
import Data.Maybe (fromJust)

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

code_gen_to_path :: FilePath -> FilePath -> IO ()
code_gen_to_path ipath opath = do
  literal_maybe <- code_gen_from_path ipath
  case literal_maybe of
    Nothing                   -> error_report
    Just ASTEmpty             -> error_report
    Just (ASTLiteral content) -> LB.writeFile opath (LB_Char.pack content)

  where error_report = putStrLn "Failed to generate strongly AST-Type "

code_gen_from_path :: FilePath -> IO (Maybe ASTLiteral)
code_gen_from_path path = LB.readFile path >>= \bs -> return (code_gen bs)

code_gen :: LB.ByteString -> Maybe ASTLiteral
code_gen bs = prologue <> generate (decode bs :: Maybe [Rule])
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

                case subnodes :: Maybe Rule of
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
          "data " ++ nt ++ " = " ++ "{ ann: a, extraChildren: " ++ v' ++ " }"

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
    generate_children r =
      let cs = children r
      in case uncons cs of
        Nothing -> ASTEmpty
        Just (first_child, tail_children) -> ASTLiteral $ "(GHC.Base.NonEmpty (" ++
         first_child ++
         (foldl' (\_ c -> " :+: " ++ c) "" tail_children) ++
         "))"
      where
        children :: [Rule] -> [String]
        children r'' = foldl' (\s r' -> s ++ [child_rule r']) [] r''

        child_rule :: Rule -> String
        child_rule r' = fromJust $ flip parseMaybe r' $ \obj -> obj .: "type"

    prologue :: Maybe ASTLiteral
    prologue = Just $
      ASTLiteral "import qualified GHC.Base\n\
                 \import qualified GHC.Generics\n\
                 \import AST.Token (Token)\n"
