{-# OPTIONS_GHC -Wall #-}

module Util where

(//) :: Maybe a -> a -> a
Just x  // _ = x
Nothing // y = y
