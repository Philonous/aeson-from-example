{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import           Control.Lens
import           Control.Lens.TH
import           Data.Text (Text)
import qualified Data.Text as Text

data Type = TypeNumber
          | TypeText
          | TypeBool
          | TypeUnknown
          | TypeArray Type
          | TypeRecord !Text
            deriving (Show, Eq, Ord)

data RecordField =
  RecordField { recordFieldName :: !Text
              , recordFieldJsonName :: !Text
              , recordFieldType' :: !Type
              } deriving (Show)

makeLensesWith camelCaseFields ''RecordField

data Record = Record { recordName :: !Text
                     , recordFields :: ![RecordField]
                     } deriving (Show)

makeLensesWith camelCaseFields ''Record

data Block = Block { blockStart, blockStartCommented
                   , blockLineStart, blockLineStartCommmented
                   , blockLineStartTrue :: !Text
                   , blockFromLine :: Int -> RecordField -> Text
                   }

makeLensesWith camelCaseFields ''Block
