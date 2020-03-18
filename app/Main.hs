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
  putStrLn $ "Device " <> show devID <> ": " <> devName
  mapM_ showT devTelems

  where
    showT Telemetry{..} = do
      putStrLn $ "  " <> teleName <> " - stmp:" <> show teleStmp <> " tsmp:" <> show teleTsmp
      showTVal teleValues

    showTVal (TVUnknown vals)           = putStrLn   "    Unknown type" >> showValues 6 vals
    showTVal (TVAccl Accelerometer{..}) = putStrLn $ "    Temp: " <> show accTemp <> " and "
                                          <> show (length accVals) <> " values like " <> (show.head) accVals
    showTVal (TVGyro Gyroscope{..})     = putStrLn $ "    Temp: " <> show gyroTemp <> " and "
                                          <> show (length gyroVals) <> " values like " <> (show.head) gyroVals
    showTVal (TVFaces fs)               = putStrLn "    Faces:" >>
                                          mapM_ (\Face{..} -> putStrLn $ "        "
                                                              <> show faceX <> "x" <> show faceY
                                                              <> ", " <> show faceW <> "x" <> show faceH
                                                              <> ", smile=" <> show faceSmile) fs
    showTVal (TVGPS GPS{..})            = putStrLn ("    GPS time=" <> show gpsTime <> " p=" <> show gpsP
                                                    <> ", len(rs)=" <> show (length gpsReadings)
                                                    <> "\n        hd=" <> showGPS (head gpsReadings))
    showTVal (TVScene ss)                = putStrLn ("   Scenes, found " <> show (length ss) <> ", first:") >>
                                           mapM_ (\(k,v) ->
                                                    putStrLn $ "        " <> show k <> "=" <> show v)
                                           (Map.assocs (head ss))
    showTVal (TVAudioLevel AudioLevel{..}) = putStrLn "    Audio levels:" >>
                                             putStrLn ("      rms:  " <> (show audioRMS)) >>
                                             putStrLn ("      peak: " <> (show audioPeak))

showGPS :: GPSReading -> String
showGPS GPSReading{..} = "(" <> show gpsrLat <> "," <> show gpsrLon <> ") alt=" <> show gpsrAlt
                         <> " spd2d=" <> show gpsrSpeed2D <> " spd3d=" <> show gpsrSpeed3D

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
