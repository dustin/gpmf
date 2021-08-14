import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BC
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck      as QC

import           GoPro.Command.DEVCString
import           GoPro.DEVC
import           GoPro.GPMF


tests :: [TestTree]
tests = map one ["basement", "grass", "hero6", "walking"]
  where diff ref new = ["diff", "-u", ref, new]
        go p = either BC.pack (foldMap ((<>"\n") . maybe "XXX" showDEVC . uncurry mkDEVC)) . parseGPMF <$> BS.readFile p
        one :: String -> TestTree
        one x = goldenVsStringDiff x diff ("test/" <> x <> ".golden" ) $ go ("samples/" <> x <> ".gpmf")

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
