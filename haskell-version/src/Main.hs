{-# Language BangPatterns #-}
module Main where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lex.Integral
import Control.Monad (forM_)

processFile :: BS.ByteString -> IO (VM.IOVector Int)
processFile content = do
  vec <- VM.new 2009
  forM_ (BS.lines content) $ \line -> do
    let line' = BS.drop 1 $ BS.dropWhile (/= '\t') line
    let (k, line'') = BS.break (== '\t') line'
    let (v, _ ) = BS.break (== '\t') $ BS.drop 1 line''
    let keyInt = readDecimal_ k
    let valInt = readDecimal_ v
    VM.unsafeModify vec (+ valInt) keyInt
  return $! vec
{-# INLINE processFile #-}

main :: IO ()
main = do
  content <- BS.readFile "../ngrams.tsv"
  vec <- processFile content
  vec' <- VU.unsafeFreeze vec
  let answer = VU.maxIndex vec'
  print answer
