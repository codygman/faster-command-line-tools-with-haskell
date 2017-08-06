{-# Language BangPatterns #-}
module Main where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lex.Integral
import Control.Monad.Primitive
import Control.Monad (forM_)
import Data.Word (Word8)
import qualified Data.ByteString as B

isSpaceWord8 :: Word8 -> Bool
isSpaceWord8 !w = w == 0x09 -- \t
{-# INLINE isSpaceWord8 #-}

mywords :: BS.ByteString -> [BS.ByteString]
mywords bs = B.splitWith isSpaceWord8 bs
{-# INLINE mywords #-}

processFile :: BS.ByteString -> IO (VM.MVector RealWorld Int)
processFile content = do
  vec <- VGM.new 2009
  forM_ (mywords <$> BS.lines content) $ \line -> do
    let (_ : k : v : _) = line
        keyInt = readDecimal_ k
        valInt = readDecimal_ v
    VGM.unsafeModify vec (+ valInt) keyInt
  return vec
{-# INLINE processFile #-}

main :: IO ()
main = do
  content <- BS.readFile "../ngrams.tsv"
  vec <- processFile content
  vec' <- VU.unsafeFreeze vec
  let answer = VU.maxIndex vec'
  print answer
