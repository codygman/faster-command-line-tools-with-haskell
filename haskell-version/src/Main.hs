module Main where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.ByteString.Char8 as BS
import Control.Monad (forM_)
import Data.Char (ord)

toInt :: BS.ByteString -> Int
toInt = BS.foldl (\n c -> n*10 + ord c - 48) 0

parseKeyValue :: BS.ByteString -> (Int, Int)
parseKeyValue b = let
    rest          = BS.drop 1 $ BS.dropWhile (/= '\t') b
    (k, rest')    = BS.break (== '\t') rest
    (v, _ )       = BS.break (== '\t') $ BS.drop 1 rest'
  in
    (toInt k, toInt v)
{-# INLINE parseKeyValue #-}

processFile :: BS.ByteString -> IO (VM.IOVector Int)
processFile content = do
  vec <- VM.new 2009
  forM_ (BS.lines content) $ \line -> do
    let (key, val) = parseKeyValue line
    VM.unsafeModify vec (+ val) key
  return vec

main :: IO ()
main = do
  content <- BS.readFile "../ngrams.tsv"
  vec <- processFile content
  vec' <- VU.unsafeFreeze vec
  let answer = VU.maxIndex vec'
  print answer
