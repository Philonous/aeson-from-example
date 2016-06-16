{-# LANGUAGE OverloadedStrings #-}
module Run where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           System.Exit
import           System.IO

import           CodeGen
import           Parse
import           Types

run :: BSL.ByteString -> IO [Record]
run bsl = do
  case eitherDecode bsl of
    Left e -> do
      Text.hPutStr stderr "Error parsing JSON: "
      hPutStrLn stderr e
      exitFailure
    Right (o@Object{}) -> do
      res <- parse "recordname" o
      case res of
        Left e -> do
          Text.hPutStrLn stderr $ "Error: " <> e
          exitFailure
        Right r -> return r


test = do
  bsl <- BSL.readFile "test.json"
  records <- run bsl
  Text.putStrLn . Text.intercalate "\n\n" $ printRecord <$> records
