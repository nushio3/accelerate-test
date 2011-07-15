#!/usr/bin/env runhaskell
{-# LANGUAGE TypeOperators, TypeSynonymInstances, FlexibleInstances #-}
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

width, height, bmpWidth, bmpHeight, zoom, realSize :: Num a => a
bmpWidth = 1024
bmpHeight = 768
zoom = 3
width = bmpWidth * zoom
height = bmpHeight * zoom

realSize = 4
eps :: (Fractional a) => a
eps = 1e-20
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

instance CEncode (World Real) where
  cEncode w = BS.concat $ map cEncode 
    [(/(zoom*zoom)) $ sum [A.indexArray w (Z:.(i*zoom+zi):.(j*zoom+zj)) | zj <- [0..zoom-1], zi <-[0..zoom-1]]
       | j<-[0..bmpHeight-1],i<-[0..bmpWidth-1]]

header :: ByteString
header = BS.concat $ map cEncode [bmpWidth, bmpHeight, realSize :: Word32]

-- Basic Definitions for Simulation 
type Cell a = ((a,a,a) , (a,a,a) , (a,a,a) , a)
type AWR = Acc (World Real)

instance Eq AWR where
  (==) = undefined
instance Num AWR where
  a+b = A.use $ run $ A.zipWith (+) a b
  a-b = A.use $ run $ A.zipWith (-) a b
  a*b = A.zipWith (*) a b
  abs = A.map abs
  signum = A.map signum
  fromInteger n =  createWorld (\_ _ -> fromInteger n)
instance Fractional AWR where
  (/)   = A.zipWith (/)
  recip = A.map recip
  fromRational x =  createWorld (\_ _ -> fromRational x)



createWorld :: (A.Elt a) => (Exp Real -> Exp Real -> Exp a) -> Acc (World a)
createWorld f = A.generate (Smart.Const worldShape) (f' . unindex2 )
  where
    f' (i' ,j') = let 
        i = A.fromIntegral i'
        j = A.fromIntegral j'
      in f i j

convolute :: [Exp Real->Exp Real] -> Cell AWR -> AWR
convolute fs c = let ((a00,a10,a20),(a01,a11,a21),(a02,a12,a22),_) = c
  in foldl1 (A.zipWith (+)) $ 
     zipWith (\f ar-> A.map f ar) fs
               [a00,a10,a20,a01,a11,a21,a02,a12,a22]

dens,momx,momy,enrg :: Cell AWR -> AWR
dens = convolute (repeat id) 
momx = convolute [((-1)*),const 0,id,((-1)*),const 0,id,((-1)*),const 0,id]
momy = convolute [((-1)*),((-1)*),((-1)*),const 0,const 0,const 0,id,id,id]
enrg = convolute [id,(0.5*),id,
                  (0.5*),const 0,(0.5*),
                  id,(0.5*),id]
-- Implementation of the Initialization
initWorld :: Cell AWR
initWorld = (zeros, mids, zeros, solid)
  where
    mids  = (cw 0.1, cw 0.7, createWorld (\_ y -> 0.2 + 0.001 * (12*y/height)))
    solid = createWorld (\x y -> 64*sq(x-height/6) + sq(y-height/2) <* sq(height/24) ? (1,0))
    cw  a = createWorld (\_ _ -> a)
    zeros = (cw 0,cw 0,cw 0)

-- Implementation of the Evolution 
mix :: AWR -> AWR -> AWR -> AWR -> AWR -> AWR -> AWR
mix w n vx vy ux uy = n*w*sumo
  where
    inp  = vx*ux + vy*uy
    sumo = 1 + 3*inp + 4.5*inp^(2::Integer) - 1.5*(ux*ux + uy*uy)

collision :: Cell AWR -> Cell AWR
collision c = c2
  where
    (_,_,_,solid) = c
    n  = dens c + fromRational eps
    mx = momx c
    my = momy c
    vx = mx / n
    vy = my / n
    b00 = sf $ mix (1/36) n (-1) (-1) vx vy
    b10 = sf $ mix (1/ 9) n   0  (-1) vx vy
    b20 = sf $ mix (1/36) n   1  (-1) vx vy
    b01 = sf $ mix (1/ 9) n (-1)   0  vx vy
    b11 = sf $ mix (4/ 9) n   0    0  vx vy
    b21 = sf $ mix (1/ 9) n   1    0  vx vy
    b02 = sf $ mix (1/36) n (-1)   1  vx vy
    b12 = sf $ mix (1/ 9) n   0    1  vx vy
    b22 = sf $ mix (1/36) n   1    1  vx vy
    sf  = ((1-solid)*)
    c2 = ((b00,b10,b20),(b01,b11,b21),(b02,b12,b22),solid)

proceed :: Cell AWR -> Cell AWR
proceed c = c2
  where
    ((a00,a10,a20),(a01,a11,a21),(a02,a12,a22),solid)=c
    ste00,ste10,ste20,ste01,ste11,ste21,ste02,ste12,ste22 :: A.Stencil3x3 Real -> Exp Real
    ste00 ((x,_,_),_,_) = x
    ste10 ((_,x,_),_,_) = x
    ste20 ((_,_,x),_,_) = x
    ste01 (_,(x,_,_),_) = x
    ste11 (_,(_,x,_),_) = x
    ste21 (_,(_,_,x),_) = x
    ste02 (_,_,(x,_,_)) = x
    ste12 (_,_,(_,x,_)) = x
    ste22 (_,_,(_,_,x)) = x
    [b00,b10,b20,b01,b11,b21,b02,b12,b22] = 
      zipWith3 treat
                [ste22,ste12,ste02,ste21,ste11,ste01,ste20,ste10,ste00]
                [a00,a10,a20,a01,a11,a21,a02,a12,a22]
                [a22,a12,a02,a21,a11,a01,a20,a10,a00]
    treat ste src antisrc = (A.stencil ste A.Wrap solid) * antisrc + A.stencil ste A.Wrap src
    c2=((b00,b10,b20),(b01,b11,b21),(b02,b12,b22),solid)

main :: IO ()
main = do
  (iterStr:fn:_) <- getArgs
  let iter = read iterStr
      nextWorld = update $ initWorld
      update = foldl1 (.) $ replicate iter (proceed . collision)
  BS.writeFile fn $ BS.concat $ 
      [header] ++ map (\f -> cEncode $ run $ f nextWorld) [dens,momx,momy,enrg]

