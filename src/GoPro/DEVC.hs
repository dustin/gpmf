{-|
Module: GoPro.DEVC
Description: Higher level representation of GPMF DEVC data.
Copyright: (c) Dustin Sallings, 2020
License: BSD3
Maintanier: dustin@spy.net
Stability: experimental

DEVC is one of the GPMF data types that contains the bulk of
interesting telemetry data from within GPMF streams.  This module
doesn't currently provide high level access to *all* DEVC data (some
of it remains low level), but it currently has useful representations
of things that seemed interesting to the author.
-}

{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module GoPro.DEVC (
  mkDEVC,
  DEVC(..), dev_id, dev_name, dev_telems,
  Accelerometer(..), acc_temp, acc_vals,
  Gyroscope(..), gyro_temp, gyro_vals,
  Face(..), face_id, face_x, face_y, face_w, face_h, face_smile,
  GPSReading(..), gpsr_lat, gpsr_lon, gpsr_alt, gpsr_speed2d, gpsr_speed3d, gpsr_time, gpsr_dop, gpsr_fix, gpsReadings,
  AudioLevel(..), audio_rms, audio_peak,
  Location(..), _Snow, _Urban, _Indoor, _Water, _Vegetation, _Beach,
  TVals(..), _TVUnknown, _TVAccl, _TVGyro, _TVFaces, _TVGPS5, _TVGPS9, _TVAudioLevel, _TVScene,
  Telemetry(..), tele_stmp, tele_tsmp, tele_name, tele_values
  ) where

import           Control.Lens      hiding (cons)
import           Data.Foldable     (fold)
import           Data.List         (transpose)
import           Data.Map.Strict   (Map)
import qualified Data.Map.Strict   as Map
import           Data.Maybe        (fromMaybe, mapMaybe)
import           Data.Time         (UTCTime (..), addDays, addUTCTime, fromGregorian)
import           Data.Word         (Word64)

import           GoPro.GPMF
import           GoPro.GPMF.Lenses

data Accelerometer = Accelerometer
    { _acc_temp :: Float
    , _acc_vals :: [(Float, Float, Float)]
    }
    deriving Show

makeLenses ''Accelerometer

data Gyroscope = Gyroscope
    { _gyro_temp :: Float
    , _gyro_vals :: [(Float, Float, Float)]
    }
    deriving Show

makeLenses ''Gyroscope

data Face = Face
    { _face_id    :: Int
    , _face_x     :: Float
    , _face_y     :: Float
    , _face_w     :: Float
    , _face_h     :: Float
    , _face_smile :: Float
    }
    deriving Show

makeLenses ''Face

data GPSReading = GPSReading
    { _gpsr_lat     :: Double
    , _gpsr_lon     :: Double
    , _gpsr_alt     :: Double
    , _gpsr_speed2d :: Double
    , _gpsr_speed3d :: Double
    , _gpsr_time    :: UTCTime
    , _gpsr_dop     :: Double
    , _gpsr_fix     :: Int
    }
    deriving Show

makeLenses ''GPSReading

data AudioLevel = AudioLevel
    { _audio_rms  :: [Int]
    , _audio_peak :: [Int]
    }
    deriving Show

makeLenses ''AudioLevel

data Location = Snow
    | Urban
    | Indoor
    | Water
    | Vegetation
    | Beach
    deriving (Show, Read, Eq, Ord)

makePrisms ''Location

data TVals = TVUnknown [Value]
    | TVAccl Accelerometer
    | TVGyro Gyroscope
    | TVFaces [Face]
    | TVGPS5 [GPSReading]
    | TVGPS9 [GPSReading]
    | TVAudioLevel AudioLevel
    | TVScene [Map Location Float]
    deriving Show

makePrisms ''TVals

data Telemetry = Telemetry
    { _tele_stmp   :: Word64
    , _tele_tsmp   :: Int
    , _tele_name   :: String
    , _tele_values :: TVals
    }
    deriving Show

makeLenses ''Telemetry

data DEVC = DEVC
    { _dev_id     :: Int
    , _dev_name   :: String
    , _dev_telems :: Map String Telemetry
    }
    deriving Show

makeLenses ''DEVC

-- | Get the best GPS readings from a DEVC.
-- This will attempt to return any TVGPS9 readings,
-- but will fall back to GPS5 readings if available.
gpsReadings :: Getter DEVC [GPSReading]
gpsReadings = to (foldr f [] . _dev_telems)
  where
    f (Telemetry _ _ _ (TVGPS9 v)) _  = v
    f (Telemetry _ _ _ (TVGPS5 v)) [] = v
    f _ o                             = o

-- | Given a FourCC value (specifically, DEVC) and a list of Values,
-- produce a DEVC value.
mkDEVC :: FourCC -> [Value] -> Maybe DEVC
mkDEVC "DEVC" = Just . foldr addItem (DEVC 0 "" mempty)
  where
    addItem (GNested ("DVID", [GUint32 [x]])) o            = o {_dev_id=fromIntegral x}
    addItem (GNested ("DVNM", [GString x]))   o            = o {_dev_name=x}
    addItem (GNested ("STRM", vals))          o@(DEVC{..}) = o {_dev_telems=addTelem _dev_telems vals}
    addItem _ o                                            = o

    addTelem m vals = let t =  foldr updTele (Telemetry 0 0 "" tvals) vals in
                        Map.insert (_tele_name t) t m
      where
        updTele (GNested ("STMP", [GUint64 [x]])) o = o {_tele_stmp = x}
        updTele (GNested ("TSMP", [GUint32 [x]])) o = o {_tele_tsmp = fromIntegral x}
        updTele (GNested ("STNM", [GString x])) o   = o {_tele_name = x}
        updTele _ o                                 = o

        tvals :: TVals
        tvals = (fromMaybe (TVUnknown vals) . ($ vals)) . foldr findGrokker (const Nothing) . foldMap four $ vals
          where
            four (GNested (x, _)) = [x]
            four _                = []

            findGrokker "ACCL" _ = fmap TVAccl . grokAccl
            findGrokker "GYRO" _ = fmap TVGyro . grokGyro
            findGrokker "FACE" _ = fmap TVFaces . grokFaces
            findGrokker "GPS5" _ = fmap TVGPS5 . grokGPS5
            findGrokker "GPS9" _ = fmap TVGPS9 . grokGPS9
            findGrokker "AALP" _ = fmap TVAudioLevel . grokAudioLevel
            findGrokker "SCEN" _ = fmap TVScene . grokScene
            findGrokker _ o      = o

mkDEVC _ = const Nothing

findVal :: FourCC -> [Value] -> Maybe [Value]
findVal f = exactlyOne . findAll f

findAll :: FourCC -> [Value] -> [[Value]]
findAll f = toListOf (folded . _GNested . filtered (\(d,_) -> d == f) . _2)

exactlyOne :: [a] -> Maybe a
exactlyOne [a] = Just a
exactlyOne _   = Nothing

grokSens :: FourCC -> (Float -> [(Float, Float, Float)] -> a) -> [Value] -> Maybe a
grokSens sens cons vals = do
  GFloat templ <- exactlyOne =<< findVal "TMPC" vals
  GInt16 scall <- exactlyOne =<< findVal "SCAL" vals
  readings <- mapMaybe ungint <$> findVal sens vals

  temp <- exactlyOne templ
  scal <- realToFrac <$> exactlyOne scall
  scaled <- traverse (trip . fmap (\x -> realToFrac x / scal)) readings

  pure $ cons temp scaled

  where ungint (GInt16 xs) = Just xs
        ungint _           = Nothing
        trip [a,b,c] = Just (a,b,c)
        trip _       = Nothing

grokAccl :: [Value] -> Maybe Accelerometer
grokAccl = grokSens "ACCL" Accelerometer

grokGyro :: [Value] -> Maybe Gyroscope
grokGyro = grokSens "GYRO" Gyroscope

grokFaces :: [Value] -> Maybe [Face]
grokFaces = Just . mapMaybe mkFace . findAll "FACE"
    where
      mkFace :: [Value] -> Maybe Face
      mkFace [GComplex "Lffffff" [GUint32 [fid], GFloat [x], GFloat [y], GFloat [w], GFloat [h], _, GFloat [s]]] =
        Just (Face (fromIntegral fid) x y w h s)
      mkFace [GComplex "Lffff" [GUint32 [fid], GFloat [x], GFloat [y], GFloat [w], GFloat [h]]] =
        Just (Face (fromIntegral fid) x y w h 0)
      mkFace _ = Nothing

findSCAL :: Fractional b => [Value] -> Maybe [b]
findSCAL vals = mapMaybe (fmap realToFrac . anInt) <$> findVal "SCAL" vals
  where
    anInt (GInt32 [x]) = Just x
    anInt _            = Nothing

grokGPS5 :: [Value] -> Maybe [GPSReading]
grokGPS5 vals = do
  GUint16 [gpsp] <- exactlyOne =<< findVal "GPSP" vals
  GUint32 [gpsf] <- exactlyOne =<< findVal "GPSF" vals
  GTimestamp time <- exactlyOne =<< findVal "GPSU" vals
  scals <- findSCAL vals
  g5s <- findVal "GPS5" vals
  let timestamps = [addUTCTime (n * 1/realToFrac @Int 10) time | n <- [0, 1 ..]]
  fold <$> traverse (readings scals (fromIntegral gpsp) (fromIntegral gpsf)) (zip g5s timestamps)

  where
    readings scals p f (GInt32 ns, ts) = case zipWith (\s n -> realToFrac n / s) scals ns of
                                     [_gpsr_lat,_gpsr_lon,_gpsr_alt,_gpsr_speed2d,_gpsr_speed3d]
                                       -> let _gpsr_time=ts; _gpsr_dop=p; _gpsr_fix=f in Just [GPSReading{..}]
                                     _ -> Nothing
    readings _ _ _ _ = Nothing

grokGPS9 :: [Value] -> Maybe [GPSReading]
grokGPS9 vals = do
  scals <- findSCAL vals
  gps9 <- findVal "GPS9" vals
  traverse (oneGPS9 scals) gps9

  where
    baseDay = fromGregorian 2000 1 1

    oneGPS9 :: [Double] -> Value -> Maybe GPSReading
    oneGPS9 [lats, lons, alts, s2ds, s3ds, 1, 1000, dops, 1] (GComplex "lllllllSS" [GInt32 [lati], GInt32 [loni], GInt32 [alti], GInt32 [speed2di], GInt32 [speed3di], GInt32 [daysi], GInt32 [secsi], GUint16 [dopi], GUint16 [fixi]]) =
      Just GPSReading{
        _gpsr_time = UTCTime (addDays (fromIntegral daysi) baseDay) (realToFrac secsi / 1000),
        _gpsr_lat = realToFrac lati / lats,
        _gpsr_lon = realToFrac loni / lons,
        _gpsr_alt = realToFrac alti / alts,
        _gpsr_speed2d = realToFrac speed2di / s2ds,
        _gpsr_speed3d = realToFrac speed3di / s3ds,
        _gpsr_dop = realToFrac dopi / dops,
        _gpsr_fix = fromIntegral fixi
      }
    oneGPS9 _ _ = Nothing

grokAudioLevel :: [Value] -> Maybe AudioLevel
grokAudioLevel vals = do
  [l1, l2] <- transpose . mapMaybe de <$> findVal "AALP" vals
  pure $ AudioLevel l1 l2

  where de (GInt8 xs)      = Just $ fmap fromIntegral xs
        de (GComplex _ xs) = Just . fold . mapMaybe de $ xs
        de _               = Nothing

grokScene :: [Value] -> Maybe [Map Location Float]
grokScene = Just . fmap mkScene . findAll "SCEN"
  where
    mkScene :: [Value] -> Map Location Float
    mkScene = Map.fromList . mapMaybe mkOne

    mkOne :: Value -> Maybe (Location, Float)
    mkOne (GComplex "Ff" [GFourCC f, GFloat [p]]) = l f >>= \x -> Just (x, p)
    mkOne _                                       = Nothing

    l :: FourCC -> Maybe Location
    l "SNOW" = Just Snow
    l "URBA" = Just Urban
    l "INDO" = Just Indoor
    l "WATR" = Just Water
    l "VEGE" = Just Vegetation
    l "BEAC" = Just Beach
    l _      = Nothing
