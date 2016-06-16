{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse where

import           Control.Lens
import           Control.Monad.Writer
import           Data.Aeson
import           Data.Char
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V

import           Types
import           Util

makeRecordName :: Text -> Text
makeRecordName = upcase . toCamelCase

makeFieldName :: Text -> Text -> Text
makeFieldName prefix name = downcase prefix <> upcase (toCamelCase  name)

toType :: MonadWriter [Record] m => Text -> Value -> m Type
toType _ String{}       = return TypeText
toType _ Bool{}         = return TypeBool
toType _ Number{}       = return TypeNumber
toType name (Array arr) =
  TypeArray <$> case arr ^? _head of
                  Nothing -> return TypeUnknown
                  Just t -> toType name t
toType _ Null           = return TypeUnknown
toType name (Object o)  = handleObject name o

toField :: MonadWriter [Record] m => Text -> (Text, Value) -> m RecordField
toField prefix (name, v) = do
  tp <- toType name v
  return RecordField { recordFieldName = makeFieldName prefix name
                     , recordFieldType' = tp
                     }

handleObject :: MonadWriter [Record] m => Text -> Object -> m Type
handleObject name o = do
  let oName = makeRecordName name
  fields <- forM (HMap.toList o) $ toField oName
  let r = Record { recordName = toRecordTypeName oName
                 , recordFields = fields
                 }
  tell [r]
  return . TypeRecord $ toRecordTypeName oName

toRecordTypeName :: Text -> Text
toRecordTypeName = upcase

parse :: (Monad m) =>
         Text
      -> Value
      -> m (Either Text [Record])
parse name (Object o) = do
  (_, records) <- runWriterT (handleObject name o)
  return . Right $ reverse records
parse _ _ = return $ Left "Expected a top level object"
