module Main where

import qualified Data.ByteString    as BS
import           Data.List          (intercalate)
import qualified Data.Map.Strict    as Map
import           System.Environment (getArgs)

import           GoPro.DEVC
import           GoPro.GPMF

fourcc :: FourCC -> String
fourcc (FourCC (a,b,c,d)) = "4cc:" <> [a,b,c,d]

cval :: Value -> String
cval (GFourCC f) = fourcc f
cval x           = show x

showValues :: Int -> [Value] -> IO ()
showValues n = mapM_ showValue
  where
    showValue (GNested x) = showTelemetry (n + 2) x
    showValue (GFourCC f) = putStrLn $ (replicate n ' ') <> fourcc f
    showValue (GComplex f v) = putStrLn $ (replicate n ' ') <> "Complex: " <> f <> ": " <> intercalate ", " (map cval v)
    showValue x           = putStrLn $ (replicate n ' ') <> show x

showTelemetry :: Int -> (FourCC, [Value]) -> IO ()
showTelemetry n (FourCC (a,b,c,d),vs) =  putStrLn ((replicate n ' ') <> [a,b,c,d]) >> showValues (n+2) vs



showDEVC :: DEVC -> IO ()
showDEVC DEVC{..} = do
  putStrLn $ "Device " <> show _dev_id <> ": " <> _dev_name
  mapM_ showT _dev_telems

  where
    showT Telemetry{..} = do
      putStrLn $ "  " <> _tele_name <> " - stmp:" <> show _tele_stmp <> " tsmp:" <> show _tele_tsmp
      showTVal _tele_values

    showTVal (TVUnknown vals)           = putStrLn   "    Unknown type" >> showValues 6 vals
    showTVal (TVAccl Accelerometer{..}) = putStrLn $ "    Temp: " <> show _acc_temp <> " and "
                                          <> show (length _acc_vals) <> " values like " <> (show.head) _acc_vals
    showTVal (TVGyro Gyroscope{..})     = putStrLn $ "    Temp: " <> show _gyro_temp <> " and "
                                          <> show (length _gyro_vals) <> " values like " <> (show.head) _gyro_vals
    showTVal (TVFaces fs)               = putStrLn "    Faces:" >>
                                          mapM_ (\Face{..} -> putStrLn $ "        "
                                                              <> show _face_x <> "x" <> show _face_y
                                                              <> ", " <> show _face_w <> "x" <> show _face_h
                                                              <> ", smile=" <> show _face_smile) fs
    showTVal (TVGPS GPS{..})            = putStrLn ("    GPS time=" <> show _gps_time <> " p=" <> show _gps_p
                                                    <> ", len(rs)=" <> show (length _gps_readings)
                                                    <> "\n        hd=" <> showGPS (head _gps_readings))
    showTVal (TVScene ss)                = putStrLn ("   Scenes, found " <> show (length ss) <> ", first:") >>
                                           mapM_ (\(k,v) ->
                                                    putStrLn $ "        " <> show k <> "=" <> show v)
                                           (Map.assocs (head ss))
    showTVal (TVAudioLevel AudioLevel{..}) = putStrLn "    Audio levels:" >>
                                             putStrLn ("      rms:  " <> (show _audio_rms)) >>
                                             putStrLn ("      peak: " <> (show _audio_peak))

showGPS :: GPSReading -> String
showGPS GPSReading{..} = "(" <> show _gpsr_lat <> "," <> show _gpsr_lon <> ") alt=" <> show _gpsr_alt
                         <> " spd2d=" <> show _gpsr_speed2d <> " spd3d=" <> show _gpsr_speed3d

main :: IO ()
main = do
  b <- BS.readFile . head =<< getArgs
  case parseGPMF b of
    Left y -> print y
    Right xs   -> do
      -- let x@(f,v) = head xs
      -- showDEVC (mkDEVC f v)
      mapM_ (showDEVC . uncurry mkDEVC) xs
      -- mapM_ (showTelemetry 0) xs
