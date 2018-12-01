{-# OPTIONS_GHC -Wall #-}

module Main where

main :: IO ()
main = do
  putStrLn "hello world"

greet :: String -> String
greet name = "Hello " ++ name ++ "!"
