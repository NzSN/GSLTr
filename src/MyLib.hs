{-# LANGUAGE ForeignFunctionInterface #-}

module MyLib (someFunc) where

foreign export ccall triple :: Int -> Int


someFunc :: IO ()
someFunc = putStrLn "someFunc"

triple :: Int -> Int
triple x = 3 * x

