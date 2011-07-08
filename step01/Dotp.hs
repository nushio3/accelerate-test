{-# OPTIONS -Wall #-}
import Data.Array.Accelerate (Acc, Vector, Scalar, (:.)(..))
import Data.Array.Accelerate.CUDA (run)
import qualified Data.Array.Accelerate as A

dotp :: Vector Float -> Vector Float -> Acc (Scalar Float)
dotp as bs = A.fold (+) 0 (A.zipWith (*) (A.use as) (A.use bs))


shape6 :: A.DIM1
shape6 =  A.Z :. 6

xs, ys :: Vector Float
xs = A.fromList shape6 [1,2,4,7,14,28]
ys = A.fromList shape6 $ repeat 1

ans :: Scalar Float
ans = run $ dotp xs ys

main :: IO ()
main = do
  putStrLn "hi"
  print ans
