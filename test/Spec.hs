import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.Golden
import qualified Data.ByteString.Lazy.Char8    as BC
import qualified Data.ByteString.Lazy    as BL
import qualified Data.ByteString    as BS

import           GoPro.DEVC
import           GoPro.GPMF
import           GoPro.Command.DEVCString


tests :: [TestTree]
tests = map one ["basement", "grass", "hero6", "walking"]
  where diff ref new = ["diff", "-u", ref, new]
        go p = either BC.pack (foldMap ((<>"\n") . showDEVC . uncurry mkDEVC)) . parseGPMF <$> BS.readFile p
        one :: String -> TestTree
        one x = goldenVsStringDiff x diff ("test/" <> x <> ".golden" ) $ go ("samples/" <> x <> ".gpmf")

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
