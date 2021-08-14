module Main where

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BL
import           System.Environment       (getArgs)

import           GoPro.Command.DEVCString
import           GoPro.DEVC
import           GoPro.GPMF

main :: IO ()
main = do
  [fn] <- getArgs
  either print (mapM_ (BL.putStrLn . maybe "" showDEVC . uncurry mkDEVC)) . parseGPMF =<< BS.readFile fn
