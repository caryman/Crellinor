{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, LambdaCase #-}
module Config where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Monoid
import Control.Monad.IO.Class
import Data.Typeable
import Data.Yaml
import System.IO
import System.IO.Error
import Data.Text (Text)
import Data.ByteString (append, ByteString)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString as B
import qualified Data.Text as T
import System.FilePath ((</>))
import System.Directory (copyFile)
import UI.NCurses


fileName :: String
fileName = "config.yaml"

data CrellinorConfig = CrellinorConfig
  {  ccEntityCount :: Int
  ,  ccEntitySpeed :: Int
  } deriving (Eq, Show)

data Configuration = Configuration
  {  configurationPath :: FilePath
  ,  configurationVar  :: MVar CrellinorConfig
  }

newConfiguration :: FilePath -> IO Configuration
newConfiguration path = do
  configValue <- readConfiguration path
  configVar <- newMVar configValue
  return Configuration
    {  configurationPath = path
    ,  configurationVar = configVar
    }

entityCount :: Text
entityCount = "entity-count"

entitySpeed :: Text
entitySpeed = "entity-speed"

programVersion :: String
programVersion = "1.0.0"

instance FromJSON CrellinorConfig where
  parseJSON (Object v) = CrellinorConfig
    <$> v .:  entityCount
    <*> v .:  entitySpeed
  parseJSON _ = mzero

instance ToJSON CrellinorConfig where
  toJSON x = object
    [ entityCount .= ccEntityCount x
    , entitySpeed .= ccEntitySpeed x
    ]

data ConfigurationFileError
  = ConfigurationFileTooLarge
  | ConfigurationFileOther SomeException
  deriving (Show, Typeable)

instance Exception ConfigurationFileError

configurationFileSizeLimit :: Int
configurationFileSizeLimit = 32 * 1024 -- 32 kilobytes

-- This function only reads a configuration file and decodes data.
-- If the file does not exist it throws the DoesNotExist IOError.
readConfiguration :: (FromJSON a) => FilePath -> IO a
readConfiguration path = do
  try (withFile path ReadMode readHandle) >>= \case
    Left e -> do
      throwIO $ ConfigurationFileOther e
    Right x ->
      return x

readConfigurationGeneric :: (FromJSON a, ToJSON a) => FilePath -> IO a
readConfigurationGeneric path = do
  r <- tryJust (guard . isDoesNotExistError) $ withFile path ReadMode readHandle
  case r of
    Left _ -> do
      copyDefault
      r' <- try $ withFile path ReadMode readHandle
      case r' of
        Left e -> do
          throw $ ConfigurationFileOther e
        Right x ->
          writeConfigurationSilent path x
          >> return x
    Right x ->
      writeConfigurationSilent path x
      >> return x
  where
    copyDefault = do
      let defaultName = path <> ".default"
      copyFile defaultName path

readHandle :: (FromJSON a) => Handle -> IO a
readHandle h = do
  sz <- hFileSize h
  if sz > fromIntegral configurationFileSizeLimit
    then throwIO ConfigurationFileTooLarge
    else B.hGetContents h >>= \bs -> case decodeEither' bs of
      Left e -> throwIO . ConfigurationFileOther $ SomeException e
      Right x -> return x

writeConfiguration :: ToJSON a => FilePath -> a -> IO ()
writeConfiguration path val = do
  writeConfigurationSilent path val
  print("Wrote configuration to: " <> T.pack path)

writeConfigurationSilent :: ToJSON a => FilePath -> a -> IO ()
writeConfigurationSilent path = B.writeFile path . addVersion . encode where
  addVersion :: ByteString -> ByteString
  addVersion = append . pack $ "# Version " ++ programVersion ++ "\n"

writeLog :: FilePath -> String -> IO ()
writeLog path s = B.writeFile path (pack s)
