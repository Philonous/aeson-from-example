{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module CodeGen where

import           Control.Lens
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Lens

import           Types
import           Util

for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap

renderType :: Type -> Text
renderType TypeNumber        = "Scientific"
renderType TypeText          = "Text"
renderType TypeBool          = "Bool"
renderType TypeUnknown       = "Unknown"
renderType (TypeArray tp)    = Text.concat ["[", renderType tp, "]"]
renderType (TypeRecord name) = renderRecordName name

unknown :: RecordField -> Bool
unknown r = go $ r ^. type'
  where
    go TypeUnknown = True
    go (TypeArray t) = go t
    go _ = False

printRecord :: Record -> Text
printRecord r =
  in Text.unlines $
     [ Text.concat [ "data "
                   , r ^. name
                   , " = "
                   , r ^. name
                   ]
     ]
     ++ (recordLines $ r ^. fields)
     ++ [ "  }"
        , "  deriving (Show, Typeable, Data, Generic)"
        ]

recordLines [] = ["  {"]
recordLines (r:rs) | unknown r =
  Text.concat ["  { -- ", printRecordField r ]
  : unknowns rs
                   | otherwise =
  Text.concat ["  { ", printRecordField r ]
  : knowns rs
  where
    unknowns [] = []
    unknowns (r:rs) | unknown r =
      Text.concat ["--  , ", printRecordField r]
        : unknowns rs
                    | otherwise =
      Text.concat ["    ", printRecordField r]
        : knowns rs
    knowns [] = []
    knowns (r:rs) | unknown r =
      Text.concat ["--  , ", printRecordField r]
      : knowns rs
                  | otherwise =
      Text.concat ["  , ", printRecordField r]
      : knowns rs

printRecordField :: RecordField -> Text
printRecordField rf =
  Text.concat [ rf ^. name
              , " :: "
              , renderType $ rf ^. type'
              ]
