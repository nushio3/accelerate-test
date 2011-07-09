#!/usr/bin/env runhaskell
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -Wall #-}
import Data.Array.Accelerate (Acc, Scalar, (:.)(..), Z(..), Exp)
import Data.Array.Accelerate.CUDA (run)
import qualified Data.Array.Accelerate.Smart as Smart
import qualified Data.Array.Accelerate as A
import Data.Time.Clock
import Prelude hiding (Real)
import System.IO

-- The World is 2-dimensional! 
type World = A.Array A.DIM2
type Real  = Float

-- Turn a rank-2 indexing expression to a pair of Index.
unindex2 :: A.Exp (Z :. Int :. Int) -> (A.Exp Int,A.Exp Int)
unindex2 ix = let Z :. i :. j = A.unlift ix in (i,j)


solve :: (Int, Int) -> IO (Double, String)
solve (worldSize, iteration) = do
  hPrint stderr $ run $ average finalWorld
  return $ (flop, msg)
  where
    msg = show worldSize ++ " " ++ show iteration

    worldShape :: A.DIM2
    worldShape =  Z :. worldSize :. worldSize

    worldSizeR :: Exp Real
    worldSizeR = fromIntegral worldSize

    initialWorld :: Acc (World Real)
    initialWorld = A.generate (Smart.Const worldShape) (gradation . unindex2 )

    func :: Acc (World Real) -> Acc (World Real)
    func = A.map (\x -> 4 * x * (1-x)) 

    finalWorld :: Acc (World Real)
    finalWorld = foldl (flip ($)) initialWorld $ replicate iteration func

    gradation :: (A.Exp Int,A.Exp Int) -> Exp Real
    gradation (i,j) = let 
        fi = A.fromIntegral i
        fj = A.fromIntegral j
      in fi / worldSizeR + fj / worldSizeR / worldSizeR
       
    average :: Acc (World Real) -> Acc (Scalar Real) 
    average =  (A.map (/(worldSizeR*worldSizeR))) . (A.fold (+) 0) . (A.fold (+) 0)

    flop :: Double
    flop = (fromIntegral worldSize)**2 * (fromIntegral iteration) * 3


benchmark :: IO (Double, String) -> IO ()
benchmark task = do
  t1  <- getCurrentTime
  (flop, msg)<- task
  t2  <- getCurrentTime
  let 
    dt :: Double
    dt = fromRational $ toRational $ diffUTCTime t2 t1
  putStrLn $ unwords $ [ show (flop/dt), show flop, show dt, msg]
  

main :: IO ()
main = do
  mapM_ (benchmark . solve) [(2^i, 4^j)|i<-[10..(14::Int)], j<-[1..(10::Int)]]
