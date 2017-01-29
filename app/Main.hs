module Main where

import Lib
import Control.Concurrent
import Control.Monad
import Data.Scientific

main :: IO ()
main = do
  -- Find the thermal sensors connected to this system
  serials <- thermalSerials

  -- Continously poll the sensors
  forever $ do
    mapM_
      (\serial -> do
        -- Read the sensor
        maybeCelsius <- thermalSensorCelsius serial
        maybe
          (return ())
          (\celsius -> do
            putStr $ show serial
            putStr ":"
            putStr $ show celsius
            putStr $ "C ("
            putStr $ show $ farenheitFromCelsius celsius
            putStrLn ")")
          maybeCelsius)
      serials
    threadDelay 1000000

-- | Convert temperature in Celsius to temperature in Farenheit
farenheitFromCelsius :: Scientific -> Scientific
farenheitFromCelsius celsius = celsius * 1.8 + 32

