#!/usr/bin/env runhaskell
{-# LANGUAGE TypeOperators, TypeSynonymInstances #-}
{-# OPTIONS -Wall #-}
import Data.Array.Accelerate 
  (Acc, Exp, Scalar, Z(..),
   (:.)(..), (?), (<*))
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
sq :: Num a => a -> a
sq x = x*x

-- The Reality is Floating. And the World is 2-dimensional! 
type Real  = Float
type World = A.Array A.DIM2
                                
worldShape :: A.DIM2
worldShape =  Z :. width :. height

-- Turn a rank-2 indexing expression to a pair of Index.
unindex2 :: A.Exp (Z :. Int :. Int) -> (A.Exp Int, A.Exp Int)
unindex2 ix = let Z :. i :. j = A.unlift ix in (i,j)

-- Visualizing tools
class CEncode a where
  cEncode :: a -> ByteString

instance CEncode Word32 where
  cEncode = Bin.runPut . Bin.putWord32le

instance CEncode Float where
  cEncode = Bin.runPut . Bin.putFloat32le

instance CEncode Double where
  cEncode = Bin.runPut . Bin.putFloat64le

instance (CEncode e)=>CEncode (World e) where
  cEncode w = BS.concat $ map cEncode 
    [A.indexArray w (Z:.i:.j)| j<-[0..height-1],i<-[0..width-1]]

header :: ByteString
header = BS.concat $ map cEncode [width, height, realSize :: Word32]

-- Simulation tools

type Cell a = ((a,a,a) , (a,a,a) , (a,a,a) , a)

initWorld :: Acc (World (Cell Real))
initWorld = createWorld f 
  where
    f x y = A.lift(zeros,
                   A.lift (A.constant 0.1, A.constant 0.7, 0.2 + 0.001 * (12*y/height)),
                   zeros,
                   solid x y)
    solid x y = 64*sq(x-height/6) + sq(y-height/2) <* sq(height/24) ? (1,0)
    zeros = A.constant (0,0,0)

createWorld :: (A.Elt a) => (Exp Real -> Exp Real -> Exp a) -> Acc (World a)
createWorld f = A.generate (Smart.Const worldShape) (f' . unindex2 )
  where
    f' (i' ,j') = let 
        i = A.fromIntegral i'
        j = A.fromIntegral j'
      in f i j

density :: Cell Real -> Real
density ((a00,a10,a20),(a01,a11,a21),(a02,a12,22),_) 
  = a00+a10+a20+a01+a11+a21+a02+a12+a22

main :: IO ()
main = do
  (fn:_) <- getArgs
  BS.writeFile fn $ BS.concat $ 
      [header] -- ++ map (cEncode . run) [dens, momx, momy, ener]
  

