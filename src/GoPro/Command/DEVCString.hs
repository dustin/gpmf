module GoPro.Command.DEVCString where

import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BC
import           Data.Foldable              (toList)
import qualified Data.Map.Strict            as Map

import           GoPro.DEVC
import           GoPro.GPMF

fourcc :: FourCC -> BL.ByteString
fourcc (FourCC (a,b,c,d)) = "4cc:" <> BC.pack [a,b,c,d]

cval :: Value -> BL.ByteString
cval (GFourCC f) = fourcc f
cval x           = tshow x

showValues :: Int -> [Value] -> BL.ByteString
showValues n = joinMap showValue
  where
    showValue (GNested x) = showTelemetry (n + 2) x
    showValue (GFourCC f) = (BC.pack $ replicate n ' ') <> fourcc f
    showValue (GComplex f v) = (BC.pack $ replicate n ' ') <> "Complex: " <> BC.pack f <> ": " <> BL.intercalate ", " (map cval v)
    showValue x           = (BC.pack $ replicate n ' ') <> tshow x

showTelemetry :: Int -> (FourCC, [Value]) -> BL.ByteString
showTelemetry n (FourCC (a,b,c,d),vs) =  (BC.pack $ (replicate n ' ') <> [a,b,c,d]) <> "\n" <> showValues (n+2) vs

tshow :: Show a => a -> BL.ByteString
tshow = BC.pack . show

joinMap :: Foldable f => (a -> BL.ByteString) -> f a -> BL.ByteString
joinMap f = BL.intercalate "\n" . fmap f . toList

showDEVC :: DEVC -> BL.ByteString
showDEVC DEVC{..} = "Device " <> tshow _dev_id <> ": " <> BC.pack _dev_name <> "\n" <> joinMap showT _dev_telems

  where
    showT Telemetry{..} = "  " <> BC.pack _tele_name <> " - stmp:" <> tshow _tele_stmp <> " tsmp:" <> tshow _tele_tsmp <> "\n" <> showTVal _tele_values

    showTVal (TVUnknown vals)           = "    Unknown type\n" <> showValues 6 vals
    showTVal (TVAccl Accelerometer{..}) = "    Temp: " <> tshow _acc_temp <> " and "
                                          <> tshow (length _acc_vals) <> " values like " <> (tshow.head) _acc_vals
    showTVal (TVGyro Gyroscope{..})     = "    Temp: " <> tshow _gyro_temp <> " and "
                                          <> tshow (length _gyro_vals) <> " values like " <> (tshow.head) _gyro_vals
    showTVal (TVFaces fs)               = "    Faces:" <> (if null fs then "" else "\n") <>
                                          joinMap ((\Face{..} -> "        "
                                                     <> tshow _face_x <> "x" <> tshow _face_y
                                                     <> ", " <> tshow _face_w <> "x" <> tshow _face_h
                                                     <> ", smile=" <> tshow _face_smile)) fs
    showTVal (TVGPS5 readings)          = "    GPS5\n\t" <> BC.intercalate "\n\t" (showGPS <$> readings)
    showTVal (TVGPS9 readings)          = "    GPS9\n\t" <> BC.intercalate "\n\t" (showGPS <$> readings)
    showTVal (TVScene ss)               = "   Scenes, found " <> tshow (length ss) <> ", first:\n" <>
                                          joinMap ((\(k,v) ->
                                                      "        " <> tshow k <> "=" <> tshow v))
                                           (Map.assocs (head ss))
    showTVal (TVAudioLevel AudioLevel{..}) = "    Audio levels:\n" <>
                                             "      rms:  " <> (tshow _audio_rms) <> "\n" <>
                                             "      peak: " <> (tshow _audio_peak)

showGPS :: GPSReading -> BL.ByteString
showGPS GPSReading{..} = "(time=" <> tshow _gpsr_time <> " - (" <> tshow _gpsr_lat <> "," <> tshow _gpsr_lon <> ") alt=" <> tshow _gpsr_alt
                         <> " spd2d=" <> tshow _gpsr_speed2d <> " spd3d=" <> tshow _gpsr_speed3d
