{-# LANGUAGE OverloadedStrings #-}
module System.Hardware.OneWire.Thermal
    ( ThermalSerial(..)
    , thermalSerials
    , thermalSensorCelsius
    ) where

import           Control.Applicative   ((<|>))
import           Control.Concurrent
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
import           Text.Parser.Char      (hexDigit)

-- | Where the thermal devices are listed in the filesystem
thermalDevicesPath :: FilePath
thermalDevicesPath = "/sys/bus/w1/devices"

-- | Serial number of a thermal sensor
newtype ThermalSerial = ThermalSerial { _thermalSerial :: FilePath } deriving (Show)

-- | List the thermal serials
thermalSerials :: IO [ThermalSerial]
thermalSerials = map ThermalSerial <$> filter (not . startswith "w1_bus_master") <$> listDirectory thermalDevicesPath

-- | Read a thermal sensor's celsius temperature
thermalSensorCelsius :: ThermalSerial -> IO (Maybe Scientific)
thermalSensorCelsius (ThermalSerial serial) = do
  w1SensorOutput <- withFile (thermalDevicesPath </> serial </> "w1_slave") ReadMode
    $ (\handle -> do
          hSetBuffering handle NoBuffering
          Text.decodeUtf8 <$> nonBlockingGet handle)
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
    nonBlockingGet handle = do
      mv <- newEmptyMVar
      forkOS $ do
        bs <- ByteString.hGetSome handle 76
        putMVar mv bs
      takeMVar mv


