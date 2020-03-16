module GoPro.DEVC where

import           Control.Monad         (guard)
import           Data.List             (transpose)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import           Data.Maybe            (fromMaybe, mapMaybe)
import           Data.Time.Clock       (UTCTime (..))
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Word             (Word64)
import           Debug.Trace

import           GoPro.GPMF

data DEVC = DEVC {
  devID     :: Int,
  devName   :: String,
  devTelems :: Map String Telemetry
  } deriving Show

data Telemetry = Telemetry {
  teleStmp   :: Word64,
  teleTsmp   :: Int,
  teleName   :: String,
  teleValues :: TVals
  } deriving Show

data TVals = TVUnknown [Value]
  | TVAccl Accelerometer
  | TVGyro Gyroscope
  | TVFaces [Face]
  | TVGPS GPS
  | TVAudioLevel AudioLevel
  | TVScene [Map Location Float]
  deriving Show

data Accelerometer = Accelerometer {
  accTemp :: Float,
  accVals :: [(Float,Float,Float)]
  } deriving Show

data Gyroscope = Gyroscope {
  gyroTemp :: Float,
  gyroVals :: [(Float, Float, Float)]
  } deriving Show

data Face = Face {
  faceID    :: Int,
  faceX     :: Float,
  faceY     :: Float,
  faceW     :: Float,
  faceH     :: Float,
  faceSmile :: Float
  } deriving Show

data GPS = GPS {
  gpsP        :: Int,
  gpsTime     :: UTCTime,
  gpsReadings :: [GPSReading]
  } deriving Show

data GPSReading = GPSReading {
  gpsrLat     :: Double,
  gpsrLon     :: Double,
  gpsrAlt     :: Double,
  gpsrSpeed2D :: Double,
  gpsrSpeed3D :: Double
  } deriving Show

data AudioLevel = AudioLevel {
  audioRMS  :: [Int],
  audioPeak :: [Int]
  } deriving Show

data Location = Snow | Urban | Indoor | Water | Vegetation | Beach deriving (Show, Eq, Ord)

mkDEVC :: FourCC -> [Value] -> DEVC
mkDEVC (FourCC ('D','E','V','C')) = foldr addItem (DEVC 0 "" mempty)
  where
    addItem (GNested (FourCC ('D','V','I','D'), [GUint32 [x]])) o            = o {devID=fromIntegral x}
    addItem (GNested (FourCC ('D','V','N','M'), [GString x]))   o            = o {devName=x}
    addItem (GNested (FourCC ('S','T','R','M'), vals))          o@(DEVC{..}) = o {devTelems=addTelem devTelems vals}
    addItem _ o                                                              = o

    addTelem m vals = let t =  foldr updTele (Telemetry 0 0 "" tvals) vals in
                        Map.insert (teleName t) t m
      where
        updTele (GNested (FourCC ('S','T','M','P'), [GUint64 [x]])) o = o {teleStmp = x}
        updTele (GNested (FourCC ('T','S','M','P'), [GUint32 [x]])) o = o {teleTsmp = fromIntegral x}
        updTele (GNested (FourCC ('S','T','N','M'), [GString x])) o = o {teleName = x}
        updTele _ o = o

        tvals :: TVals
        tvals = ($ vals) . foldr findGrokker TVUnknown . concatMap four $ vals
          where
            four (GNested (x, _)) = [x]
            four _                = []

            findGrokker (FourCC ('A','C','C','L')) _ = TVAccl . grokAccl
            findGrokker (FourCC ('G','Y','R','O')) _ = TVGyro . grokGyro
            findGrokker (FourCC ('F','A','C','E')) _ = TVFaces . grokFaces
            findGrokker (FourCC ('G','P','S','5')) _ = TVGPS . grokGPS
            findGrokker (FourCC ('A','A','L','P')) _ = TVAudioLevel . grokAudioLevel
            findGrokker (FourCC ('S','C','E','N')) _ = TVScene . grokScene
            findGrokker _ o                          = o

mkDEVC f = error ("I can't make a DEVC out of " <> show f)

findVal :: FourCC -> [Value] -> Maybe [Value]
findVal f = go
  where go [] = Nothing
        go (GNested (fc, vs):xs) = if fc == f
                                   then Just vs
                                   else go xs
        go (_:xs) = go xs

findAll :: FourCC -> [Value] -> [[Value]]
findAll f = go
  where go [] = []
        go (GNested (fc, vs):xs) = if fc == f
                                   then vs : go xs
                                   else go xs
        go (_:xs) = go xs

shead :: [a] -> Maybe a
shead []    = Nothing
shead (x:_) = Just x

grokSens :: FourCC -> (Float -> [(Float, Float, Float)] -> a) -> [Value] -> a
grokSens sens cons vals = fromMaybe (cons 0 []) $ do
  GFloat templ <- shead =<< findVal (FourCC ('T','M','P','C')) vals
  GInt16 scall <- shead =<< findVal (FourCC ('S','C','A','L')) vals
  readings <- findVal sens vals

  temp <- shead templ
  scal <- realToFrac <$> shead scall

  let scaled = map (\(GInt16 [a,b,c]) -> (realToFrac a / scal, realToFrac b / scal, realToFrac c / scal)) readings

  pure $ cons temp scaled

grokAccl :: [Value] -> Accelerometer
grokAccl = grokSens (FourCC ('A','C','C','L')) Accelerometer

grokGyro :: [Value] -> Gyroscope
grokGyro = grokSens (FourCC ('G','Y','R','O')) Gyroscope

grokFaces :: [Value] -> [Face]
grokFaces = mapMaybe mkFace . findAll (FourCC ('F','A','C','E'))
    where
      mkFace :: [Value] -> Maybe Face
      mkFace [GComplex "Lffffff" [GUint32 [fid], GFloat [x], GFloat [y], GFloat [w], GFloat [h], _, GFloat [s]]] =
        Just (Face (fromIntegral fid) x y w h s)
      mkFace [GComplex "Lffff" [GUint32 [fid], GFloat [x], GFloat [y], GFloat [w], GFloat [h]]] =
        Just (Face (fromIntegral fid) x y w h 0)
      mkFace f = traceShowM f >> Nothing

grokGPS :: [Value] -> GPS
grokGPS vals = fromMaybe (GPS 9999 (posixSecondsToUTCTime 0) []) $ do
  GUint16 [gpsp] <- shead =<< findVal (FourCC ('G','P','S','P')) vals
  GTimestamp time <- shead =<< findVal (FourCC ('G','P','S','U')) vals
  scals <- fmap (\(GInt32 [x]) -> realToFrac x) <$> findVal (FourCC ('S','C','A','L')) vals
  g5s <- findVal (FourCC ('G','P','S','5')) vals

  pure $ GPS (fromIntegral gpsp) time (convg5s scals g5s)

  where
    convg5s :: [Double] -> [Value] -> [GPSReading]
    convg5s scals =
      concatMap (\(GInt32 ns) -> case zipWith (\s n -> realToFrac n / s) scals ns of
                   [gpsrLat,gpsrLon,gpsrAlt,gpsrSpeed2D,gpsrSpeed3D] -> [GPSReading{..}]
                   _ -> []
                )

grokAudioLevel :: [Value] -> AudioLevel
grokAudioLevel vals = fromMaybe (AudioLevel [] []) $ do
  alps <- transpose . fmap (\(GInt8 xs) -> fmap fromIntegral xs) <$> findVal (FourCC ('A','A','L','P')) vals
  guard $ length alps == 2
  pure $ AudioLevel (alps !! 0) (alps !! 1)

grokScene :: [Value] -> [Map Location Float]
grokScene = fmap mkScene . findAll (FourCC ('S','C','E','N'))
  where
    mkScene :: [Value] -> Map Location Float
    mkScene = Map.fromList . mapMaybe mkOne

    mkOne :: Value -> Maybe (Location, Float)
    mkOne (GComplex "Ff" [GFourCC f, GFloat [p]]) = l f >>= \x -> Just (x, p)
    mkOne _                                       = Nothing

    l :: FourCC -> Maybe Location
    l (FourCC ('S','N','O','W')) = Just Snow
    l (FourCC ('U','R','B','A')) = Just Urban
    l (FourCC ('I','N','D','O')) = Just Indoor
    l (FourCC ('W','A','T','R')) = Just Water
    l (FourCC ('V','E','G','E')) = Just Vegetation
    l (FourCC ('B','E','A','C')) = Just Beach
    l _                          = Nothing
