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

{-# LANGUAGE TemplateHaskell #-}

module GoPro.DEVC (
  mkDEVC,
  DEVC(..), dev_id, dev_name, dev_telems,
  Accelerometer(..), acc_temp, acc_vals,
  Gyroscope(..), gyro_temp, gyro_vals,
  Face(..), face_id, face_x, face_y, face_w, face_h, face_smile,
  GPSReading(..), gpsr_lat, gpsr_lon, gpsr_alt, gpsr_speed2d, gpsr_speed3d,
  GPS(..), gps_p, gps_time, gps_readings,
  AudioLevel(..), audio_rms, audio_peak,
  Location(..), _Snow, _Urban, _Indoor, _Water, _Vegetation, _Beach,
  TVals(..), _TVUnknown, _TVAccl, _TVGyro, _TVFaces, _TVGPS, _TVAudioLevel, _TVScene,
  Telemetry(..), tele_stmp, tele_tsmp, tele_name, tele_values
  ) where

import           Control.Lens    hiding (cons)
import           Control.Monad   (guard)
import           Data.Foldable   (fold)
import           Data.List       (transpose)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe, listToMaybe, mapMaybe)
import           Data.Time.Clock (UTCTime (..))
import           Data.Word       (Word64)

import           GoPro.GPMF

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
    }
    deriving Show

makeLenses ''GPSReading

data GPS = GPS
    { _gps_p        :: Int
    , _gps_time     :: UTCTime
    , _gps_readings :: [GPSReading]
    }
    deriving Show

makeLenses ''GPS

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
    | TVGPS GPS
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
        tvals = (fromMaybe (TVUnknown vals) . ($ vals)) . foldr findGrokker (const Nothing) . concatMap four $ vals
          where
            four (GNested (x, _)) = [x]
            four _                = []

            findGrokker "ACCL" _ = fmap TVAccl . grokAccl
            findGrokker "GYRO" _ = fmap TVGyro . grokGyro
            findGrokker "FACE" _ = fmap TVFaces . grokFaces
            findGrokker "GPS5" _ = fmap TVGPS . grokGPS
            findGrokker "AALP" _ = fmap TVAudioLevel . grokAudioLevel
            findGrokker "SCEN" _ = fmap TVScene . grokScene
            findGrokker _ o      = o

mkDEVC _ = const Nothing

findVal :: FourCC -> [Value] -> Maybe [Value]
findVal f = listToMaybe . findAll f

findAll :: FourCC -> [Value] -> [[Value]]
findAll f = mapMaybe g
  where
    g (GNested (fc, vs)) | fc == f = Just vs
    g _                            = Nothing

grokSens :: FourCC -> (Float -> [(Float, Float, Float)] -> a) -> [Value] -> Maybe a
grokSens sens cons vals = do
  GFloat templ <- listToMaybe =<< findVal "TMPC" vals
  GInt16 scall <- listToMaybe =<< findVal "SCAL" vals
  readings <- mapMaybe ungint <$> findVal sens vals

  temp <- listToMaybe templ
  scal <- realToFrac <$> listToMaybe scall
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

grokGPS :: [Value] -> Maybe GPS
grokGPS vals = do
  GUint16 [gpsp] <- listToMaybe =<< findVal "GPSP" vals
  GTimestamp time <- listToMaybe =<< findVal "GPSU" vals
  scals <- fmap (\(GInt32 [x]) -> realToFrac x) <$> findVal "SCAL" vals
  g5s <- findVal "GPS5" vals
  rs <- mconcat <$> traverse (readings scals) g5s

  pure $ GPS (fromIntegral gpsp) time rs

  where
    readings scals (GInt32 ns) = case zipWith (\s n -> realToFrac n / s) scals ns of
                                   [_gpsr_lat,_gpsr_lon,_gpsr_alt,_gpsr_speed2d,_gpsr_speed3d]
                                     -> Just [GPSReading{..}]
                                   _ -> Nothing
    readings _ _ = Nothing

grokAudioLevel :: [Value] -> Maybe AudioLevel
grokAudioLevel vals = do
  alps <- transpose . mapMaybe de <$> findVal "AALP" vals
  guard $ length alps == 2
  pure $ AudioLevel (alps !! 0) (alps !! 1)

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
