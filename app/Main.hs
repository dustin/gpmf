module Main where

import qualified Data.ByteString    as BS
import qualified Data.ByteString.Lazy    as BL
import           System.Environment (getArgs)

import           GoPro.DEVC
import           GoPro.GPMF
import           GoPro.Command.DEVCString

main :: IO ()
main = do
  b <- BS.readFile . head =<< getArgs
  case parseGPMF b of
    Left y -> print y
    Right xs   -> do
      -- let x@(f,v) = head xs
      -- showDEVC (mkDEVC f v)
      mapM_ (BL.putStr . showDEVC . uncurry mkDEVC) xs
      -- mapM_ (showTelemetry 0) xs
