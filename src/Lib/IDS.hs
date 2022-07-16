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
  [ ("⿰⺼雹", "𦡕", True)
  , ("⿰土禀", "𡒄", True)
  , ("⿰叠毛", "𬇇", True)
  , ("⿱竹閒", "𥳑", True)
  , ("⿰籥頁", "𥸤", True)
  , ("⿰足厘", "𨂷", True)
  , ("⿺走乚", "𧺇", True)
  , ("⿰漢鳥", "𤅩", True)
  , ("⿰冫枼", "𠗨", True)
  , ("⿱宀⿺元攵", "𡨥", True)
  , ("⿱日夨", "𣅔", True)
  , ("⿰角乚", "𧢳", True)
  , ("⿱頁心", "𢝊", True)


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
