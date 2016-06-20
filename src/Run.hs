{-# LANGUAGE OverloadedStrings #-}
module Run where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Options.Applicative
import           System.Exit
import           System.IO

import           CodeGen
import           Parse
import           Types

parseRecords :: Text -> BSL.ByteString -> IO [Record]
parseRecords name bsl = do
  case eitherDecode bsl of
    Left e -> do
      Text.hPutStr stderr "Error parsing JSON: "
      hPutStrLn stderr e
      exitFailure
    Right (o@Object{}) -> do
      res <- parse name o
      case res of
        Left e -> do
          Text.hPutStrLn stderr $ "Error: " <> e
          exitFailure
        Right r -> return r

data Options = Options { inFile :: Maybe String
                       , outFile :: Maybe String
                       , revertObjects :: Bool
                       , rootName :: String
                       } deriving (Show)

optionsParser :: Parser Options
optionsParser =
  Options <$> (optional $ strOption (short 'i'  <> help "input file"))
          <*> (optional $ strOption (short 'o'  <> help "output file"))
          <*> (flag False True (long "reverse" <> help "revert order of type definitions") )
          <*> (strOption (short 'r' <> help "name of the base record"))

getOptions :: IO Options
getOptions = execParser (info optionsParser mempty)

run :: IO ()
run = do
  opts <- getOptions
  bsl <- case inFile opts of
    Nothing -> BSL.hGetContents stdin
    Just filename -> BSL.readFile filename
  let name = Text.pack $ rootName opts
  records <- (if (revertObjects opts)
              then reverse
              else id)
    <$> parseRecords name bsl
  let out = Text.intercalate "\n\n" $ printRecord <$> records
  case outFile opts of
    Nothing -> Text.hPutStrLn stdout out
    Just outfile -> Text.writeFile outfile out
