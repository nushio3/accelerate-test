#!/usr/bin/env runhaskell
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -Wall #-}
import Data.Array.Accelerate (Acc, Scalar, (:.)(..), Z(..), Exp)
import Data.Array.Accelerate.CUDA (run)
import qualified Data.Array.Accelerate.Smart as Smart
import qualified Data.Array.Accelerate as A
import qualified Data.Binary.IEEE754 as Bin
import qualified Data.Binary.Put as Bin
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Time.Clock
import Data.Word
import Prelude hiding (Real)
import System.Environment
import System.IO

width, height, bmpSize, realSize :: Num a => a
width = 1024
height = 768
bmpSize = width * height
realSize = 4

worldShape :: A.DIM2
worldShape =  Z :. width :. height


encodeI :: Word32 -> ByteString
encodeI = Bin.runPut . Bin.putWord32le

encodeR :: Real -> ByteString
encodeR = Bin.runPut . Bin.putFloat32le

header :: ByteString
header = BS.concat $ map encodeI [width, height, realSize]

dens, momx, momy, ener :: Acc (World Real)
dens = createWorld (\_ _ -> 1.0)
momx = createWorld (\_ y -> 0.1 * sin (y/100))
momy = createWorld (\x _ -> 0.1 * cos (x/100))
ener = createWorld (\_ _ -> 4.2)

-- The Reality is Floating. And the World is 2-dimensional! 
type Real  = Float
type World = A.Array A.DIM2

-- Turn a rank-2 indexing expression to a pair of Index.
unindex2 :: A.Exp (Z :. Int :. Int) -> (A.Exp Int, A.Exp Int)
unindex2 ix = let Z :. i :. j = A.unlift ix in (i,j)


createWorld :: (Exp Real -> Exp Real -> Exp Real) -> Acc (World Real)
createWorld f = A.generate (Smart.Const worldShape) (f' . unindex2 )
  where
    f' :: (A.Exp Int,A.Exp Int) -> Exp Real
    f' (i' ,j') = let 
        i = A.fromIntegral i'
        j = A.fromIntegral j'
      in f i j

encodeWorld :: World Real -> ByteString
encodeWorld w = BS.concat $ 
  map encodeR [A.indexArray w (Z:.i:.j)| i<-[0..width-1], j<-[0..height-1] ]

main :: IO ()
main = do
  (fn:_) <- getArgs
  BS.writeFile fn $ BS.concat $ 
      [header] ++ map (encodeWorld . run) [dens, momx, momy, ener]
  

