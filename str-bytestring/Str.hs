module Str(module Data.ByteString.Char8, module Str) where

import Prelude hiding (length, null, splitAt)
import Data.ByteString.Char8
import Data.ByteString

type Str = ByteString

splits :: Str -> [(Str, Str)]
splits s = fmap (\n -> splitAt n s) [0..length s]

parts :: Str -> [[Str]]
parts s | null s    = [[]]
        | otherwise = do
            n <- [1..length s]
            let (l, r) = splitAt n s
            fmap (l:) (parts r)
