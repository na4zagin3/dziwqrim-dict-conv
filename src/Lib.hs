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
import GHC.Generics (Generic)

import Lib.Entry (sectionsFromRows, sectionToTex)
import Lib.Row (parseRow)
import Lib.Index.RadicalIndex (radicalSectionsToTex, sectionsToRadicalSections)
import Lib.Index.SikrokIndex (sikrokSectionsToTex, sectionsToSikrokSections)
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
  let outText = T.intercalate "\n"
                [ T.unlines . map sectionToTex $ sections
                , sikrokSectionsToTex $ sectionsToSikrokSections sections
                , radicalSectionsToTex $ sectionsToRadicalSections sections
                ]
  T.writeFile outPath outText

readCsvFile
  :: FilePath -> IO (Either String (Vector (Map Text Text)))
readCsvFile path = do
  str <- BL.readFile path
  return . fmap snd $ Csv.decodeByName str
