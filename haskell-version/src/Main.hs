module Main where

import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Lex.Integral
import qualified Data.Vector.Unboxed as V

toInt :: C8.ByteString -> Int
toInt = readDecimal_

{-# INLINE process #-}
process bs = (i, xs V.! i) where
  info = C8.splitWith (=='\t') <$> C8.lines bs
  xs = V.unsafeAccumulate (+) (V.replicate 2009 0) . V.fromList
     $ map (\(_:key:val:_) -> (toInt key, toInt val)) info
  i = V.maxIndex xs

main = print . process =<< C8.readFile "../ngrams.tsv"
