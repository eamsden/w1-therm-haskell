{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( ThermalSerial(..)
    , thermalSerials
    , thermalSensorCelsius
    ) where

import           Control.Applicative   ((<|>))
import           Data.Attoparsec.Text
import           Data.Monoid           ((<>))
import           Data.Scientific       (Scientific())
import           Data.String.Utils     (startswith)
import qualified Data.ByteString       as ByteString
import           Data.Text             (Text)
import qualified Data.Text.IO          as Text
import qualified Data.Text.Encoding    as Text
import           Data.List             (delete)
import           System.IO             (FilePath, withFile, IOMode(..), hSetBuffering, BufferMode(..))
import           System.FilePath.Posix ((</>))
import           System.Directory      (listDirectory)
import           System.Posix.Signals  (getSignalMask, setSignalMask, addSignal, sigVTALRM, sigALRM)
import           Text.Parser.Char      (hexDigit)

-- | Where the thermal devices are listed in the filesystem
thermalDevicesPath :: FilePath
thermalDevicesPath = "/sys/bus/w1/devices"

-- | Serial number of a thermal sensor
newtype ThermalSerial = ThermalSerial { _thermalSerial :: FilePath } deriving (Show)

-- | List the thermal serials
thermalSerials :: IO [ThermalSerial]
thermalSerials = do
  serials <- map ThermalSerial <$> filter (not . startswith "w1_bus_master") <$> listDirectory thermalDevicesPath
  mapM print serials
  return serials

-- | Read a thermal sensor's celsius temperature
thermalSensorCelsius :: ThermalSerial -> IO (Maybe Scientific)
thermalSensorCelsius (ThermalSerial serial) = do
  w1SensorOutput <- withFile (thermalDevicesPath </> serial </> "w1_slave") ReadMode
    $ (\handle -> do
          hSetBuffering handle NoBuffering
          Text.decodeUtf8 <$> (withNoRTSTicks $ ByteString.hGetSome handle 76))
  return $ either (const Nothing) id $ parseOnly thermalSensorParser w1SensorOutput
  where
    thermalSensorParser :: Parser (Maybe Scientific)
    thermalSensorParser =
         skipSpace
      *> nineHexPairs
      *> ":"
      *> skipSpace
      *> "crc"
      *> skipSpace
      *> "="
      *> skipSpace
      *> hexPair
      *> skipSpace
      *> (    ("NO"  *> skipSpace *> pure Nothing)
          <|> (   "YES"
               *> skipSpace
               *> nineHexPairs
               *> "t"
               *> skipSpace
               *> "="
               *> skipSpace
               *> (Just <$> (/ 1000) <$> fromIntegral <$> decimal)) <* skipSpace)

    hexPair = hexDigit *> hexDigit *> skipSpace
    nineHexPairs = count 9 hexPair

-- | Block RTS tick signals during the body of this block
withNoRTSTicks :: IO a -> IO a
withNoRTSTicks action = do
  currentMask <- getSignalMask
  setSignalMask $ sigALRM `addSignal` sigVTALRM `addSignal` currentMask
  result <- action
  setSignalMask currentMask
  return result
