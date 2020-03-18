{-# LANGUAGE TemplateHaskell #-}

module GoPro.DEVC where

import           Control.Lens    hiding (cons)
import           Control.Monad   (guard)
import           Data.List       (transpose)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe, mapMaybe)
import           Data.Time.Clock (UTCTime (..))
import           Data.Word       (Word64)

import           GoPro.GPMF

data Accelerometer = Accelerometer {
  _acc_temp :: Float,
  _acc_vals :: [(Float,Float,Float)]
  } deriving Show

makeLenses ''Accelerometer

data Gyroscope = Gyroscope {
  _gyro_temp :: Float,
  _gyro_vals :: [(Float, Float, Float)]
  } deriving Show

makeLenses ''Gyroscope

data Face = Face {
  _face_id    :: Int,
  _face_x     :: Float,
  _face_y     :: Float,
  _face_w     :: Float,
  _face_h     :: Float,
  _face_smile :: Float
  } deriving Show

makeLenses ''Face

data GPSReading = GPSReading {
  _gpsr_lat     :: Double,
  _gpsr_lon     :: Double,
  _gpsr_alt     :: Double,
  _gpsr_speed2d :: Double,
  _gpsr_speed3d :: Double
  } deriving Show

makeLenses ''GPSReading

data GPS = GPS {
  _gps_p        :: Int,
  _gps_time     :: UTCTime,
  _gps_readings :: [GPSReading]
  } deriving Show

makeLenses ''GPS

data AudioLevel = AudioLevel {
  _audio_rms  :: [Int],
  _audio_peak :: [Int]
  } deriving Show

makeLenses ''AudioLevel

data Location = Snow | Urban | Indoor | Water | Vegetation | Beach deriving (Show, Eq, Ord)

makePrisms ''Location

data TVals = TVUnknown [Value]
  | TVAccl Accelerometer
  | TVGyro Gyroscope
  | TVFaces [Face]
  | TVGPS GPS
  | TVAudioLevel AudioLevel
  | TVScene [Map Location Float]
  deriving Show

makePrisms ''TVals

data Telemetry = Telemetry {
  _tele_stmp   :: Word64,
  _tele_tsmp   :: Int,
  _tele_name   :: String,
  _tele_values :: TVals
  } deriving Show

makeLenses ''Telemetry

data DEVC = DEVC {
  _dev_id     :: Int,
  _dev_name   :: String,
  _dev_telems :: Map String Telemetry
  } deriving Show

makeLenses ''DEVC

mkDEVC :: FourCC -> [Value] -> DEVC
mkDEVC (FourCC ('D','E','V','C')) = foldr addItem (DEVC 0 "" mempty)
  where
    addItem (GNested (FourCC ('D','V','I','D'), [GUint32 [x]])) o            = o {_dev_id=fromIntegral x}
    addItem (GNested (FourCC ('D','V','N','M'), [GString x]))   o            = o {_dev_name=x}
    addItem (GNested (FourCC ('S','T','R','M'), vals))          o@(DEVC{..}) = o {_dev_telems=addTelem _dev_telems vals}
    addItem _ o                                                              = o

    addTelem m vals = let t =  foldr updTele (Telemetry 0 0 "" tvals) vals in
                        Map.insert (_tele_name t) t m
      where
        updTele (GNested (FourCC ('S','T','M','P'), [GUint64 [x]])) o = o {_tele_stmp = x}
        updTele (GNested (FourCC ('T','S','M','P'), [GUint32 [x]])) o = o {_tele_tsmp = fromIntegral x}
        updTele (GNested (FourCC ('S','T','N','M'), [GString x])) o = o {_tele_name = x}
        updTele _ o = o

        tvals :: TVals
        tvals = (fromMaybe (TVUnknown vals) . ($ vals)) . foldr findGrokker (const Nothing) . concatMap four $ vals
          where
            four (GNested (x, _)) = [x]
            four _                = []

            findGrokker (FourCC ('A','C','C','L')) _ = fmap TVAccl . grokAccl
            findGrokker (FourCC ('G','Y','R','O')) _ = fmap TVGyro . grokGyro
            findGrokker (FourCC ('F','A','C','E')) _ = fmap TVFaces . grokFaces
            findGrokker (FourCC ('G','P','S','5')) _ = fmap TVGPS . grokGPS
            findGrokker (FourCC ('A','A','L','P')) _ = fmap TVAudioLevel . grokAudioLevel
            findGrokker (FourCC ('S','C','E','N')) _ = fmap TVScene . grokScene
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

grokSens :: FourCC -> (Float -> [(Float, Float, Float)] -> a) -> [Value] -> Maybe a
grokSens sens cons vals = do
  GFloat templ <- shead =<< findVal (FourCC ('T','M','P','C')) vals
  GInt16 scall <- shead =<< findVal (FourCC ('S','C','A','L')) vals
  readings <- findVal sens vals

  temp <- shead templ
  scal <- realToFrac <$> shead scall

  let scaled = map (\(GInt16 [a,b,c]) -> (realToFrac a / scal, realToFrac b / scal, realToFrac c / scal)) readings

  pure $ cons temp scaled

grokAccl :: [Value] -> Maybe Accelerometer
grokAccl = grokSens (FourCC ('A','C','C','L')) Accelerometer

grokGyro :: [Value] -> Maybe Gyroscope
grokGyro = grokSens (FourCC ('G','Y','R','O')) Gyroscope

grokFaces :: [Value] -> Maybe [Face]
grokFaces = Just . mapMaybe mkFace . findAll (FourCC ('F','A','C','E'))
    where
      mkFace :: [Value] -> Maybe Face
      mkFace [GComplex "Lffffff" [GUint32 [fid], GFloat [x], GFloat [y], GFloat [w], GFloat [h], _, GFloat [s]]] =
        Just (Face (fromIntegral fid) x y w h s)
      mkFace [GComplex "Lffff" [GUint32 [fid], GFloat [x], GFloat [y], GFloat [w], GFloat [h]]] =
        Just (Face (fromIntegral fid) x y w h 0)
      mkFace _ = Nothing

grokGPS :: [Value] -> Maybe GPS
grokGPS vals = do
  GUint16 [gpsp] <- shead =<< findVal (FourCC ('G','P','S','P')) vals
  GTimestamp time <- shead =<< findVal (FourCC ('G','P','S','U')) vals
  scals <- fmap (\(GInt32 [x]) -> realToFrac x) <$> findVal (FourCC ('S','C','A','L')) vals
  g5s <- findVal (FourCC ('G','P','S','5')) vals
  rs <- mconcat <$> traverse (readings scals) g5s

  pure $ GPS (fromIntegral gpsp) time rs

  where
    readings scals (GInt32 ns) = case zipWith (\s n -> realToFrac n / s) scals ns of
                                   [_gpsr_lat,_gpsr_lon,_gpsr_alt,_gpsr_speed2d,_gpsr_speed3d] -> Just [GPSReading{..}]
                                   _ -> Nothing
    readings _ _ = Nothing

grokAudioLevel :: [Value] -> Maybe AudioLevel
grokAudioLevel vals = do
  alps <- transpose . fmap (\(GInt8 xs) -> fmap fromIntegral xs) <$> findVal (FourCC ('A','A','L','P')) vals
  guard $ length alps == 2
  pure $ AudioLevel (alps !! 0) (alps !! 1)

grokScene :: [Value] -> Maybe [Map Location Float]
grokScene = Just . fmap mkScene . findAll (FourCC ('S','C','E','N'))
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
