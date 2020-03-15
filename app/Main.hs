module Main where

import           GoPro.GPMF

import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.ByteString.Lazy            as BL

showValues :: Int -> [Value] -> IO ()
showValues n = mapM_ showValue
  where
    showValue (GNested x) = showTelemetry (n + 2) x
    showValue x           = putStrLn $ (replicate n ' ') <> show x

showTelemetry :: Int -> (FourCC, [Value]) -> IO ()
showTelemetry n (FourCC (a,b,c,d),vs) =  putStrLn ((replicate n ' ') <> [a,b,c,d]) >> showValues (n+2) vs

main :: IO ()
main = do
  b <- BL.readFile "test.in"
  case A.parse (A.many1 parseGPMF) b of
    A.Fail r x y -> print (r, x,y)
    A.Done _ x   -> mapM_ (showTelemetry 0) x
