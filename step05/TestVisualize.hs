#!/usr/bin/env runhaskell
{-# OPTIONS -Wall #-}
import qualified Data.Binary as Bin
import Data.Int
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import System.Environment

width, height, realSize :: Int32
width = 1024
height = 768
realSize = 4

encode :: Bin.Binary a => a -> ByteString
encode = BS.pack . reverse . BS.unpack . Bin.encode

header :: ByteString
header = BS.concat $ map encode [width, height, realSize]

main :: IO ()
main = do
  (fn:_) <- getArgs
  BS.writeFile fn header
  

