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
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Text.Printf (printf)
import GHC.Generics (Generic)

import Lib.Entry (sectionsFromRows, sectionToTex)
import Lib.Row (parseRow)
-- import Control.Monad.Trans.Writer.Strict (Writer)
-- type Dictionary = List

convertCsvToTex :: FilePath -> FilePath -> IO ()
convertCsvToTex inPath outPath = do
  Right rawRows <- readCsvFile inPath
  let rows = [(2 :: Int)..]
  let parsedResults =  zip rows .map (uncurry parseRow) . zip rows . V.toList $ rawRows
  let (errors, parsedRows) = partitionEithers . map (\(i, r) -> left (printf "%d: %s" i) r) $ parsedResults
  mapM_ putStrLn errors
  let sections = sectionsFromRows $ parsedRows
  mapM_ putStrLn errors
  let outText = T.unlines . map sectionToTex $ sections
  T.writeFile outPath outText


readCsvFile
  :: FilePath -> IO (Either String (Vector (Map Text Text)))
readCsvFile path = do
  str <- BL.readFile path
  return . fmap snd $ Csv.decodeByName str
