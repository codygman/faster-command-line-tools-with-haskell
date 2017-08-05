module Main where

import qualified Data.ByteString.Char8 as C8
import qualified Data.IntMap.Strict as M
import Data.List
import Data.ByteString.Lex.Integral

toInt :: C8.ByteString -> Int
toInt = readDecimal_

processFile = do
  info <- fmap C8.words . C8.lines <$> C8.readFile "../ngrams.tsv"
  let m = foldl'
             (\acc x -> let (_:key:val:_) = x in M.insertWith (\new old -> old + new) (toInt key) (toInt val) acc)
             M.empty
             info
  pure $ last $ sortOn snd (M.toList m)

main :: IO ()
main = do
  x <- processFile
  print x
