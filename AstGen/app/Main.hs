module Main where

import System.Environment (getArgs)
import System.Console.ParseArgs (
  parseArgs, Args(..), Arg(..),
  ArgsComplete (ArgsComplete),
  getRequiredArg, argDataRequired, Argtype(ArgtypeString))

import qualified AstGen (
  code_gen_from_path,
  code_gen_to_path)


data AstGenArg = NodeTypeJSON |
                 OutputPath
               deriving (Eq, Show, Ord)

argParser :: [String] -> Args AstGenArg
argParser args = parseArgs
  ArgsComplete
  [nodeTypeArg, outputArg]
  "Generate Strongly-typed AST for input node_type.json"
  args

  where nodeTypeArg = Arg
          NodeTypeJSON
          (Just 't')
          (Just "node_type")
          (argDataRequired "NodeType" (\_ -> ArgtypeString Nothing))
          "node_type.js"

        outputArg = Arg
          OutputPath
          (Just 'o')
          (Just "output")
          (argDataRequired "Output" (\_ -> ArgtypeString Nothing))
          "output path"

main :: IO ()
main = do
  args <- getArgs
  let arg = argParser args
  print $ (getRequiredArg arg OutputPath :: String)
