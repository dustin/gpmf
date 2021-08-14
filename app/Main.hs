module Main where

import qualified Data.ByteString    as BS
import qualified Data.ByteString.Lazy    as BL
import           System.Environment (getArgs)

import           GoPro.DEVC
import           GoPro.GPMF
import           GoPro.Command.DEVCString

main :: IO ()
main = do
  [fn] <- getArgs
  either print (mapM_ (BL.putStrLn . showDEVC . uncurry mkDEVC)) . parseGPMF =<< BS.readFile fn
