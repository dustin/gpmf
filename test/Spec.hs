import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BC
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck      as QC
import           Control.Monad.State              (StateT, evalStateT, get, lift, put)
import qualified Data.Attoparsec.ByteString       as A

import           GoPro.Command.DEVCString
import           GoPro.DEVC
import           GoPro.GPMF

staticExamples :: [(Char, BS.ByteString, Value)]
staticExamples =
    [ ('F', BS.pack [71, 80, 77, 70], GFourCC "GPMF")
    , ('f', BS.pack [64, 73, 15, 219], GFloat [3.1415927])
    , ('L', BS.pack [0, 0, 0, 42], GUint32 [42])
    , ('l', BS.pack [255, 255, 255, 214], GInt32 [-42])
    , ('B', BS.pack [255], GUint8 [255])
    , ('b', BS.pack [214], GInt8 [-42])
    , ('S', BS.pack [0, 42], GUint16 [42])
    , ('s', BS.pack [0, 42], GUint16 [42])
    ]

staticTests :: [TestTree]
staticTests = map one staticExamples
  where one (c, bs, v) = testCase (show c) $ Right v @=? A.parseOnly (evalStateT (p c) "") bs
        p :: Char -> StateT String A.Parser Value
        p c = do
          let (_, parser) = singleParser c
          lift parser

tests :: [TestTree]
tests = staticTests <> map one ["basement", "grass", "hero6", "walking"]
  where diff ref new = ["diff", "-u", ref, new]
        go p = either BC.pack (foldMap ((<>"\n") . maybe "XXX" showDEVC . uncurry mkDEVC)) . parseGPMF <$> BS.readFile p
        one :: String -> TestTree
        one x = goldenVsStringDiff x diff ("test/" <> x <> ".golden" ) $ go ("samples/" <> x <> ".gpmf")

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
