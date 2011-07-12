#!/usr/bin/env runhaskell
{-# OPTIONS -Wall #-}
import Data.Array.Accelerate (Acc, Vector, Scalar, (:.)(..))
import Data.Array.Accelerate.CUDA (run)
import qualified Data.Array.Accelerate as A

myShape :: A.DIM1
myShape =  A.Z :. 10

xs, ys :: Vector Float
xs = A.fromList myShape [83,72,73,78,82,89,65,75,85,33]
ys = A.fromList myShape [73,75,65,32,77,85,83,85,77,69]

accXs, accYs :: Acc (Vector Float)
accXs = A.use xs
accYs = A.use ys

dotp :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
dotp as bs = A.fold (+) 0 (A.zipWith (*) as bs)

accAns :: Acc (Scalar Float)
accAns = dotp accXs accYs

ans :: Scalar Float
ans = run $ accAns

main :: IO ()
main = do
  putStrLn $ "xs    : " ++ show xs
  putStrLn $ "accXs : " ++ show accXs
  putStrLn $ "accAns: " ++ show accAns
  putStrLn $ "ans   : " ++ show ans

