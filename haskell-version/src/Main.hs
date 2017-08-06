{-# Language BangPatterns #-}
module Main where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lex.Integral
import Control.Monad.Primitive
import Control.Monad (forM_)

processFile :: FilePath -> IO (VM.MVector RealWorld Int)
processFile path = do
  content <- BS.readFile path
  vec <- VGM.new 2009
  forM_ (BS.split '\t' <$> BS.lines content) $ \line -> do
    let (_ : k : v : _) = readDecimal_ <$> line
    VGM.modify vec (+v) k
  return vec

main :: IO ()
main = do
  vec <- processFile "../ngrams.tsv"
  vec' <- VU.freeze vec
  let answer = VU.maxIndex vec'
  print answer
