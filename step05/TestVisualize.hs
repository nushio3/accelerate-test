#!/usr/bin/env runhaskell
{-# OPTIONS -Wall #-}
import qualified Data.Binary.IEEE754 as Bin
import qualified Data.Binary.Put as Bin
import Data.Word
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import System.Environment

width, height, bmpSize, realSize :: Num a => a
width = 1024
height = 768
bmpSize = width * height
realSize = 4

encodeI :: Word32 -> ByteString
encodeI = Bin.runPut . Bin.putWord32le

encodeF :: Float -> ByteString
encodeF = Bin.runPut . Bin.putFloat32le

header :: ByteString
header = BS.concat $ map encodeI [width, height, realSize]

dens, momx, momy, ener :: ByteString
dens = BS.concat $ replicate bmpSize $ encodeF (1.0 :: Float)
momx = BS.concat $ replicate bmpSize $ encodeF (0.2 :: Float)
momy = BS.concat $ replicate bmpSize $ encodeF (0.4 :: Float)
ener = BS.concat $ replicate bmpSize $ encodeF (1.0 :: Float)


main :: IO ()
main = do
  (fn:_) <- getArgs
  BS.writeFile fn $ BS.concat [header, dens, momx, momy, ener]
  

