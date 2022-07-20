{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Lib.IDS where

import Data.Attoparsec.Text
import Data.Attoparsec.Text qualified as AP
import Data.List.NonEmpty qualified as NEL
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Printf (printf)

import Lib.Kanji (explodeKanji)

ids2, ids3 :: String
ids2 = "⿰⿱⿴⿵⿶⿷⿸⿹⿺⿻"
ids3 = "⿲⿳"

convTable :: [(Text, Text, Bool)]
convTable =
  [ ("⿰冫枼", "𠗨", False)
  , ("⿰疊刂", "𠠯", False)
  , ("⿱⿰未攵厂", "𠩺", False)
  , ("⿰土貟", "𡎖", False)
  , ("⿰土禀", "𡒄", False)
  , ("⿱宀⿺元攵", "𡨥", False)
  , ("⿺辶𩠐寸", "𡭎", False)
  , ("⿺廴囘", "𢌞", False)
  , ("⿱頁心", "𢝊", False)
  , ("𢵧", "𢵧", False)
  , ("⿱日夨", "𣅔", False)
  , ("⿰漢鳥", "𤅩", False)
  , ("⿱竹閒", "𥳑", False)
  , ("⿰籥頁", "𥸤", False)
  , ("⿰月㫐", "𦚢", False)
  , ("⿰⺼雹", "𦡕", False)
  , ("⿰角乚", "𧢳", False)
  , ("⿺走乚", "𧺇", False)
  , ("⿰足厘", "𨂷", False)
  , ("⿺辶𩠐", "𨕥", False)
  , ("⿰叠毛", "𬇇", False)
  , ("⿱臼工", "𬛸", False)
  , ("⿰氵囘", "𭰁", False)
  , ("⿺辶囘", "𮞉", False)

  ]

convMap :: Map Text Text
convMap = M.fromList . map (\(f, t, _) -> (f, t)) . filter (\(_, _, d) -> d) $ convTable

convMapFull :: Map Text Text
convMapFull = M.fromList . map (\(f, t, _) -> (f, t)) $ convTable

convIdsWith :: Map Text Text -> Text -> Either String Text
convIdsWith convMap t = do
  ks <- explodeKanji t
  return $ mconcat $ map (\k -> fromMaybe k $ M.lookup k convMap) ks

convIds :: Text -> Either String Text
convIds = convIdsWith convMap

convIdsFull :: Text -> Either String Text
convIdsFull = convIdsWith convMapFull
