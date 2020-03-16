module Main where

import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.ByteString.Lazy            as BL
import           Data.List                       (intercalate)
import qualified Data.Map.Strict                 as Map
import           System.Environment              (getArgs)

import           GoPro.DEVC
import           GoPro.GPMF

fourcc :: FourCC -> String
fourcc (FourCC (a,b,c,d)) = "4cc:" <> [a,b,c,d]

cval :: Value -> String
cval (GFourCC f) = fourcc f
cval x           = show x

showValues :: Int -> [Value] -> IO ()
showValues n = mapM_ showValue
  where
    showValue (GNested x) = showTelemetry (n + 2) x
    showValue (GFourCC f) = putStrLn $ (replicate n ' ') <> fourcc f
    showValue (GComplex f v) = putStrLn $ (replicate n ' ') <> "Complex: " <> f <> ": " <> intercalate ", " (map cval v)
    showValue x           = putStrLn $ (replicate n ' ') <> show x

showTelemetry :: Int -> (FourCC, [Value]) -> IO ()
showTelemetry n (FourCC (a,b,c,d),vs) =  putStrLn ((replicate n ' ') <> [a,b,c,d]) >> showValues (n+2) vs

showDEVC :: DEVC -> IO ()
showDEVC DEVC{..} = do
  putStrLn $ "Device " <> show devID <> ": " <> devName
  mapM_ (\(k,v) -> do
            putStrLn $ "  " <> k
            putStrLn $ "    " <> show v) (Map.assocs devTelems)

main :: IO ()
main = do
  b <- BL.readFile . head =<< getArgs
  case parseGPMF b of
    A.Fail r x y -> print (r, x,y)
    A.Done _ xs   -> do
      let x@(f,v) = head xs
      -- mapM_ (showDEVC . uncurry mkDEVC) xs
      showDEVC (mkDEVC f v)
      mapM_ (showTelemetry 0) [x]
