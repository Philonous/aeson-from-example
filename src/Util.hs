{-# LANGUAGE OverloadedStrings #-}
module Util where

import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as Text


upcase :: Text -> Text
upcase = Text.pack . go . Text.unpack
  where
    go [] = []
    go (c:cs) = toUpper c : cs

downcase :: Text -> Text
downcase = Text.pack . go . Text.unpack
  where
    go [] = []
    go (c:cs) = toLower c : cs

toCamelCase :: Text -> Text
toCamelCase = Text.pack . go . Text.unpack
  where
    go [] = []
    go (c:cs) | isAlphaNum c = c : go cs
              | otherwise =
                  let rest = dropWhile (not . isAlphaNum) cs
                  in case rest of
                    [] -> []
                    (d:ds) -> toUpper d : go ds
