#!/usr/bin/env runhaskell
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -Wall #-}
import Data.Array.Accelerate (Acc(..), Vector, Scalar, (:.)(..), Z(..))
import Data.Array.Accelerate.CUDA (run)
import qualified Data.Array.Accelerate.Smart as Smart
import qualified Data.Array.Accelerate as A

type Cell = A.Array A.DIM2

worldShape :: A.DIM2
worldShape =  Z :. 10 :. 10 

initialWorld :: Acc(Cell Int)
initialWorld = A.generate (Smart.Const worldShape) ((\(i,j)-> 10*i + j) . unindex2 )

-- Turn an 'Int' expression into a rank-2 indexing expression.                       

unindex2 :: A.Exp (Z :. Int :. Int) -> (A.Exp Int,A.Exp Int)
unindex2 ix = let Z :. i :. j = A.unlift ix in (i,j)


main :: IO ()
main = do
  print $ run initialWorld
