{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Lib
    ( convertCsvToTex
    , readCsvFile
    ) where

import Control.Arrow (left)
import Data.ByteString.Lazy qualified as BL
import Data.Csv qualified as Csv
import Data.Either (partitionEithers)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Text.Printf (printf)

import Lib.Entry (sectionsFromRows, groupToTex, groupSections)
import Lib.Row (parseRow)
import Lib.Index.GroupIndex (groupsToIndexTex)
import Lib.Index.RadicalIndex (radicalSectionsToTex, sectionsToRadicalSections)
import Lib.Index.ReadingIndex (readingSectionsToTex, sectionsToReadingSections)
import Lib.Index.SikrokIndex (sikrokSectionsToTex, sectionsToSikrokSections)
import Lib.Index.UnicodeIndex (unicodeSectionsToTex, sectionsToUnicodeSections)
import Lib.PhoneticRadical (parsePhoneticRadical)
-- import Control.Monad.Trans.Writer.Strict (Writer)
-- type Dictionary = List

contineShowingErrors :: [(Int, Either String a)] -> IO [a]
contineShowingErrors res = do
  let (errors, successes) = partitionEithers . map (\(i, r) -> left (printf "%d: %s" i) r) $ res
  mapM_ putStrLn errors
  return successes

convertCsvToTex :: FilePath -> FilePath -> FilePath -> IO ()
convertCsvToTex inRowPath inPhoneticRadicalPath outPath = do

  Right rawRows <- readCsvFile inRowPath
  Right rawPhoneticRadicals <- readCsvFile inPhoneticRadicalPath
  let rows = [(2 :: Int)..]
  parsedRows <- contineShowingErrors . zip rows . map (uncurry parseRow) . zip rows . V.toList $ rawRows
  parsedPhoneticRadicals <- contineShowingErrors . zip rows . map (uncurry parsePhoneticRadical) . zip rows . V.toList $ rawPhoneticRadicals
  let (errors2, sections) = sectionsFromRows parsedPhoneticRadicals . catMaybes $ parsedRows
  mapM_ putStrLn errors2
  let
    groups = groupSections $ sections
    mainText = T.unlines $ map groupToTex groups
    sikrokIndexText = sikrokSectionsToTex $ sectionsToSikrokSections sections
    radicalIndexText = radicalSectionsToTex $ sectionsToRadicalSections sections
    readingIndexText = readingSectionsToTex $ sectionsToReadingSections sections
    unicodeIndexText = unicodeSectionsToTex $ sectionsToUnicodeSections sections
    groupIndexText = groupsToIndexTex groups
  T.writeFile (outPath <> "-main.tex") $ mainText
  T.writeFile (outPath <> "-index-sikrok.tex") $ sikrokIndexText
  T.writeFile (outPath <> "-index-radical.tex") $ radicalIndexText
  T.writeFile (outPath <> "-index-reading.tex") $ readingIndexText
  T.writeFile (outPath <> "-index-unicode.tex") $ unicodeIndexText
  T.writeFile (outPath <> "-index-groups.tex") $ groupIndexText

readCsvFile
  :: FilePath -> IO (Either String (Vector (Map Text Text)))
readCsvFile path = do
  str <- BL.readFile path
  return . fmap snd $ Csv.decodeByName str
