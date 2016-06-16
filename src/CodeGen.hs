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
renderType (TypeRecord name) = name

unknown :: RecordField -> Bool
unknown r = go $ r ^. type'
  where
    go TypeUnknown = True
    go (TypeArray t) = go t
    go _ = False

printRecord :: Record -> Text
printRecord r = Text.unlines $
     [ Text.concat [ "data "
                   , r ^. name
                   , " = "
                   , r ^. name
                   ]
     ]
     ++ (recordLines $ r ^. fields)
     ++ [ "  }"
        , "  deriving (Show, Typeable, Data, Generic)"
        , ""
        , "deriveJSON (aesonOptions \"" <> downcase (r ^.name) <>"\") ''"
          <> (r ^. name)
        , "makeLensesWith camelCaseFields ''" <> r ^. name
        ]

recordLines [] = ["  {"]
recordLines rss@(r:rs) | unknown r =
  Text.concat ["  { -- ", printAlign r ]
  : unknowns rs
                   | otherwise =
  Text.concat ["  { ", printAlign r ]
  : knowns rs
  where
    alignTo = maximum $ (Text.length . view name) <$> rss
    printAlign r = printRecordField (alignTo - Text.length (r ^. name)) r

    unknowns [] = []
    unknowns (r:rs) | unknown r =
      Text.concat ["--  , ", printAlign r]
        : unknowns rs
                    | otherwise =
      Text.concat ["    ", printAlign r]
        : knowns rs
    knowns [] = []
    knowns (r:rs) | unknown r =
      Text.concat ["--  , ", printAlign r]
      : knowns rs
                  | otherwise =
      Text.concat ["  , ", printAlign r]
      : knowns rs

printRecordField :: Int -> RecordField -> Text
printRecordField align rf =
  Text.concat [ rf ^. name
              , Text.replicate align " "
              , " :: "
              , renderType $ rf ^. type'
              ]
