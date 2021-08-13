{-|
Module: GoPro.GPMF
Description: Parser for GoPro GPMF telemetry data.
Copyright: (c) Dustin Sallings, 2020
License: BSD3
Maintanier: dustin@spy.net
Stability: experimental

A low-level parser for <https://github.com/gopro/gpmf-parser GPMF> telemetry data.
-}

{-# LANGUAGE TupleSections #-}

module GoPro.GPMF (parseGPMF, Value(..), FourCC(..)) where

import           Control.Monad                    (replicateM)
import           Control.Monad.State              (StateT, evalStateT, get, lift, put)
import           Data.Attoparsec.Binary           (anyWord16be, anyWord32be, anyWord64be)
import qualified Data.Attoparsec.ByteString       as A
import qualified Data.Attoparsec.ByteString.Char8 as AC
import           Data.Binary.Get                  (runGet)
import           Data.Binary.IEEE754              (getFloat32be)
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as BL
import           Data.Int                         (Int16, Int32, Int64, Int8)
import           Data.String                      (IsString (..))
import           Data.Time.Clock                  (UTCTime)
import           Data.Time.Format                 (defaultTimeLocale, parseTimeM)
import           Data.Word                        (Word16, Word32, Word64, Word8)
{-
Type Char	Definition	typedef	Comment
b	single byte signed integer	int8_t	-128 to 127
B	single byte unsigned integer	uint8_t	0 to 255
c	single byte 'c' style ASCII character string	char	Optionally NULL terminated - size/repeat sets the length
d	64-bit double precision (IEEE 754)	double
f	32-bit float (IEEE 754)	float
F	32-bit four character key -- FourCC	char fourcc[4]
G	128-bit ID (like UUID)	uint8_t guid[16]
j	64-bit signed unsigned number	int64_t
J	64-bit unsigned unsigned number	uint64_t
l	32-bit signed integer	int32_t
L	32-bit unsigned integer	uint32_t
q	32-bit Q Number Q15.16	uint32_t	16-bit integer (A) with 16-bit fixed point (B) for A.B value (range -32768.0 to 32767.99998)
Q	64-bit Q Number Q31.32	uint64_t	32-bit integer (A) with 32-bit fixed point (B) for A.B value.
s	16-bit signed integer	int16_t	-32768 to 32768
S	16-bit unsigned integer	uint16_t	0 to 65536
U	UTC Date and Time string	char utcdate[16]	Date + UTC Time format yymmddhhmmss.sss - (years 20xx covered)
?	data structure is complex	TYPE	Structure is defined with a preceding TYPE
null	Nested metadata	uint32_t	The data within is GPMF structured KLV data
-}

newtype FourCC = FourCC (Char, Char, Char, Char) deriving (Show, Eq)

instance IsString FourCC where
  fromString [a,b,c,d] = FourCC (a,b,c,d)
  fromString _         = error "invalid FourCC"

data Value = GInt8 [Int8]
    | GUint8 [Word8]
    | GString String
    | GDouble Double
    | GFloat [Float]
    | GFourCC FourCC
    | GUUID [Word8]
    | GInt64 [Int64]
    | GUint64 [Word64]
    | GInt32 [Int32]
    | GUint32 [Word32]
    | GQ32 [Word32]
    | GQ64 [Word64]
    | GInt16 [Int16]
    | GUint16 [Word16]
    | GTimestamp UTCTime
    | GComplex String [Value]
    | GNested (FourCC, [Value])
    | GUnknown (Char, Int, Int, [[Word8]])
    deriving (Show)

type Parser = StateT String A.Parser

int8 :: A.Parser Int8
int8 = fromIntegral <$> A.anyWord8

-- | Parse GPMF data from a telemetry stream.  A successful return
-- value contains a list of FourCC tagged value lists.
--
-- Note that the input is the telemetry stream itself, not the
-- container that contains it.
parseGPMF :: BS.ByteString -> Either String [(FourCC, [Value])]
parseGPMF = A.parseOnly (evalStateT (A.many1 parseNested) "")

parseNested :: Parser (FourCC, [Value])
parseNested = do
  fourcc <- lift parseFourCC
  t <- lift AC.anyChar
  ss <- fromIntegral <$> lift A.anyWord8
  rpt <- fromIntegral <$> lift anyWord16be
  let padding = (4 - (fromIntegral ss * rpt) `mod` 4) `mod` 4

  stuffs <- parseValue t ss rpt

  case (fourcc, stuffs) of
    (FourCC ('T', 'Y', 'P', 'E'), [GString x]) -> put x
    _                                          -> pure ()

  _ <- lift $ replicateM padding A.anyWord8
  pure (fourcc, stuffs)

parseString :: Int -> A.Parser Value
parseString l = GString . reverse  . dropWhile (== '\0') . reverse <$> replicateM l AC.anyChar

parseFloat :: A.Parser Float
parseFloat = runGet getFloat32be . BL.fromStrict <$> A.take 4

replicatedParser :: Int -> Int -> Int -> A.Parser a -> ([a] -> Value) -> Parser [Value]
replicatedParser 0 l rpt _ _ = lift $ replicateM (l*rpt) A.anyWord8 >> pure []
replicatedParser one l rpt p cons = do
  ns <- lift $ replicateM rpt (replicateM (l `div` one) p)
  pure (cons <$> ns)

parseTimestamp :: A.Parser UTCTime
parseTimestamp = parseTimeM False defaultTimeLocale "%y%m%d%H%M%S%Q" =<< replicateM 16 AC.anyChar

singleParser :: Char -> (Int, A.Parser Value)
singleParser 'F' = (4, GFourCC <$> parseFourCC)
singleParser 'f' = (4, GFloat . (:[]) <$> parseFloat)
singleParser 'L' = (4, GUint32 . (:[]) <$> anyWord32be)
singleParser 'B' = (1, GUint8 . (:[]) <$> A.anyWord8)
singleParser 'b' = (1, GInt8 . (:[]) . fromIntegral <$> A.anyWord8)
singleParser x   = error ("unsupported parser: " <> show x)

parseComplex :: Int -> Int -> Parser [Value]
parseComplex l rpt = do
  fmt <- get
  let sz = foldr (\x o -> (fst . singleParser) x + o) 0 fmt
  let parsers = traverse (snd . singleParser) fmt
  replicatedParser sz l rpt parsers (GComplex fmt . mconcat)

parseValue :: Char -> Int -> Int -> Parser [Value]
parseValue '\0' l rpt = do
  inp <- lift $ A.take (l * rpt)
  t <- get
  xs <- case A.parseOnly (evalStateT (A.many1 parseNested) t) inp of
          Left y   -> fail y
          Right xs -> pure xs
  pure (GNested <$> xs)
parseValue 'F' 4 rpt = lift $ replicateM rpt (GFourCC <$> parseFourCC)
parseValue 'L' l rpt = replicatedParser 4 l rpt anyWord32be GUint32
parseValue 'l' l rpt = replicatedParser 4 l rpt (fromIntegral <$> anyWord32be) GInt32
parseValue 'c' l rpt = (:[]) <$> (lift . parseString $ (l * rpt))
parseValue 's' l rpt = replicatedParser 2 l rpt (fromIntegral <$> anyWord16be) GInt16
parseValue 'S' l rpt = replicatedParser 2 l rpt anyWord16be GUint16
parseValue 'J' l rpt = replicatedParser 8 l rpt anyWord64be GUint64
parseValue 'f' l rpt = replicatedParser 4 l rpt parseFloat GFloat
parseValue 'b' l rpt = lift $ replicateM rpt (GInt8 <$> replicateM l int8)
parseValue 'B' l rpt = lift $ replicateM rpt (GUint8 <$> replicateM l A.anyWord8)
parseValue 'U' 16 1 = (:[]) . GTimestamp <$> lift parseTimestamp
parseValue '?' l rpt = parseComplex l rpt
parseValue x l rpt = do
  u <- lift $ replicateM rpt (replicateM l A.anyWord8)
  pure [GUnknown (x, l, rpt, u)]

parseFourCC :: A.Parser FourCC
parseFourCC = do
  a <- AC.anyChar
  b <- AC.anyChar
  c <- AC.anyChar
  d <- AC.anyChar
  pure $ FourCC (a,b,c,d)

