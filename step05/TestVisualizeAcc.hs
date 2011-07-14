#!/usr/bin/env runhaskell
{-# LANGUAGE TypeOperators, TypeSynonymInstances #-}
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
-- The Reality is Floating. And the World is 2-dimensional! 
type Real  = Float
type World = A.Array A.DIM2
                                
worldShape :: A.DIM2
worldShape =  Z :. width :. height

-- Turn a rank-2 indexing expression to a pair of Index.
unindex2 :: A.Exp (Z :. Int :. Int) -> (A.Exp Int, A.Exp Int)
unindex2 ix = let Z :. i :. j = A.unlift ix in (i,j)

class CEncode a where
  cEncode :: a -> ByteString

instance CEncode Word32 where
  cEncode = Bin.runPut . Bin.putWord32le

instance CEncode Float where
  cEncode = Bin.runPut . Bin.putFloat32le

instance CEncode Double where
  cEncode = Bin.runPut . Bin.putFloat64le

header :: ByteString
header = BS.concat $ map cEncode [width, height, realSize :: Word32]

dens, momx, momy, ener :: Acc (World Real)
dens = createWorld (\_ _ -> 1.0)
momx = createWorld (\_ y -> 0.1 * sin (y/30))
momy = createWorld (\x _ -> 0.1 * cos (x/30))
ener = createWorld (\_ _ -> 4.2)



createWorld :: (Exp Real -> Exp Real -> Exp Real) -> Acc (World Real)
createWorld f = A.generate (Smart.Const worldShape) (f' . unindex2 )
  where
    f' :: (A.Exp Int,A.Exp Int) -> Exp Real
    f' (i' ,j') = let 
        i = A.fromIntegral i'
        j = A.fromIntegral j'
      in f i j

instance (CEncode e)=>CEncode (World e) where
  cEncode w = BS.concat $ map cEncode 
    [A.indexArray w (Z:.i:.j)| j<-[0..height-1],i<-[0..width-1]]

main :: IO ()
main = do
  (fn:_) <- getArgs
  BS.writeFile fn $ BS.concat $ 
      [header] ++ map (cEncode . run) [dens, momx, momy, ener]
  

