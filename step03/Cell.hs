#!/usr/bin/env runhaskell
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -Wall #-}
import Data.Array.Accelerate (Acc, Vector, Scalar, (:.)(..), Z(..), Exp)
import Data.Array.Accelerate.CUDA (run)
import qualified Data.Array.Accelerate.Smart as Smart
import qualified Data.Array.Accelerate as A

-- The World is 2-dimensional! 
type World = A.Array A.DIM2

-- Turn a rank-2 indexing expression to a pair of Index.
unindex2 :: A.Exp (Z :. Int :. Int) -> (A.Exp Int,A.Exp Int)
unindex2 ix = let Z :. i :. j = A.unlift ix in (i,j)


worldSize :: Num a => a
worldSize = 10

worldShape :: A.DIM2
worldShape =  Z :. worldSize :. worldSize

initialWorld :: Acc (World Float)
initialWorld = A.generate (Smart.Const worldShape) (gradation . unindex2 )

gradation :: (A.Exp Int,A.Exp Int) -> Exp Float
gradation (i,j) = fi / worldSize + fj / worldSize / worldSize
  where
    fi = A.fromIntegral i
    fj = A.fromIntegral j

main :: IO ()
main = do
  print $ run initialWorld
