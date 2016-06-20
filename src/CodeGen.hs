{-# LANGUAGE TemplateHaskell #-}
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

indentedListBlock :: Int
                  -> (Int -> RecordField -> Text)
                  -> Text
                  -> Text
                  -> Block
indentedListBlock indent print start sep =
  let ind = Text.replicate indent " "
      printer alignTo r = print (alignTo - Text.length (r ^. name)) r
  in Block { blockStart = ind <> start <> " "
           , blockStartCommented = ind <> start <> " -- "
           , blockLineStartTrue = ind <> "  "
           , blockLineStart = ind <> sep <> " "
           , blockLineStartCommmented = "--"<> ind <> sep <> " "
           , blockFromLine = printer
           }

-- General generation of list-like things (records, list etc) with possibly
-- commented lines
recordLines :: Block -> [RecordField] -> [Text]
recordLines block [] = [block ^. start]
recordLines block rss@(r:rs) | unknown r =
  Text.concat [block ^. startCommented, renderLine r ]
  : unknowns rs
                   | otherwise =
  Text.concat [block ^. start, renderLine r ]
  : knowns rs
  where
    alignTo = maximum $ (Text.length . view name) <$> rss
    renderLine = (block ^. fromLine) alignTo
    unknowns [] = []
    unknowns (r:rs) | unknown r =
      Text.concat [block ^. lineStartCommmented, renderLine r]
        : unknowns rs
                    | otherwise =
      Text.concat [block ^. lineStartTrue, renderLine r]
        : knowns rs
    knowns [] = []
    knowns (r:rs) | unknown r =
      Text.concat [block ^. lineStartCommmented, renderLine r]
      : knowns rs
                  | otherwise =
      Text.concat [block ^. lineStart, renderLine r]
      : knowns rs

--------------------------------------------------------------------------------
-- Record Definitions ----------------------------------------------------------
--------------------------------------------------------------------------------

printRecordField :: Int -> RecordField -> Text
printRecordField align rf =
  Text.concat [ rf ^. name
              , Text.replicate align " "
              , " :: "
              , renderType $ rf ^. type'
              ]

definitionBlock :: Block
definitionBlock = indentedListBlock 2 printRecordField "{" ","

--------------------------------------------------------------------------------
-- ToJSON ----------------------------------------------------------------------
--------------------------------------------------------------------------------

printRecordParams :: Int -> RecordField -> Text
printRecordParams align rf =
  Text.concat [ rf ^. name
              , Text.replicate align " "
              , " = "
              , rf ^. name
              ]

paramBlock :: Int -> Block
paramBlock indent = indentedListBlock indent printRecordParams "{" ","

printObjectLine :: Int -> RecordField -> Text
printObjectLine align rf =
  Text.concat [ "\"", rf ^. jsonName , "\""
              , Text.replicate align " "
              , " .= "
              , rf ^. name
              ]

toObjectBlock :: Block
toObjectBlock = indentedListBlock 4 printObjectLine "[" ","

printToJson :: Record -> Text
printToJson record = Text.unlines . map Text.concat $
  [ ["instance ToJSON ", record ^. name, " where "]
  , ["  toJSON ", record ^. name]
  ]
  ++ (map (:[]) $ recordLines (paramBlock 4) (record ^. fields))
  ++ [[ "    } ="]
     ,[ "    object"]
     ]
  ++ (map (:[]) $ recordLines toObjectBlock (record ^. fields))
  ++ [[ "    ]"]]

--------------------------------------------------------------------------------
-- FromJSON --------------------------------------------------------------------
--------------------------------------------------------------------------------

parseObjectBlock indent = indentedListBlock (indent - 2)  printer " " " "
  where
    printer indent f =
      Text.concat [ (f ^. name)
                  , Text.replicate indent " "
                  , " <- o .: \""
                  , f ^. jsonName
                  , "\""
                  ]

printFromJson :: Record -> Text
printFromJson r = Text.unlines . map Text.concat $
  [ ["instance FromJSON ", r ^. name, " where "]
  , ["  parseJSON = withObject \"", r ^. name, "\" $ \\o -> do "]
  ]
  ++ (map (:[]) $ recordLines (parseObjectBlock 4) (r ^. fields))
  ++ [["    return ", r ^. name]]
  ++ (map (:[]) $ recordLines (paramBlock 6) (r ^. fields))
  ++ [["      }"]]


--------------------------------------------------------------------------------
-- Print Record ----------------------------------------------------------------
--------------------------------------------------------------------------------

printRecord :: Record -> Text
printRecord r = Text.unlines $
     [ Text.concat [ "data "
                   , r ^. name
                   , " = "
                   , r ^. name
                   ]
     ]
     ++ (recordLines definitionBlock $ r ^. fields)
     ++ [ "  }"
        , "  deriving (Show, Typeable, Data, Generic)"
        , ""
        ]
     ++ [printToJson r]
     ++ ["", printFromJson r]
     ++ [ "makeLensesWith camelCaseFields ''" <> r ^. name
        ]
