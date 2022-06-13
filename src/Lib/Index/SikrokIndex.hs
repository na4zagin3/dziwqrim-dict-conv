{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Lib.Index.SikrokIndex where

import Codec.QRCode qualified as QR
import Control.Arrow (first)
import Data.Either (partitionEithers)
import Data.Foldable qualified as Foldable
import Data.Function (on)
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NEL
import Data.Map qualified as M
import Data.Map.Strict qualified as MS
import Data.Maybe (fromMaybe, catMaybes, fromMaybe, maybeToList)
import Data.Maybe qualified as Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEV
import GHC.Generics (Generic)
import Lib.Entry (renderSikrokToTex, Entry(..), Section(..), entryToLabel)
import Lib.Row (ShapeVariant(..), ShapeVariants(..))
import Text.Printf (printf)

data SikrokEntry = SikrokEntry
  { sk_e_sikrok :: !(Text, Text)
  , sk_e_entry :: !Text
  , sk_e_label :: !Text
  , sk_e_headword :: !(Maybe Text)
  }
  deriving (Read, Show, Eq, Ord, Generic)

data SikrokSection = SikrokSection
  { sk_s_header :: !Text
  , sk_s_entries :: ![SikrokEntry]
  }
  deriving (Read, Show, Eq, Ord, Generic)

sikrokEntryToTex :: SikrokEntry -> Text
sikrokEntryToTex SikrokEntry
  { sk_e_sikrok = sk
  , sk_e_entry = e
  , sk_e_headword = hw
  , sk_e_label = l
  } = mconcat contents
  where
    contents =
      [ "\\SikrokEntry"
      , "{"
      , renderSikrokToTex sk
      , "}"
      , fromMaybe "" $ fmap (\x -> "[" <> x <> "]") hw
      , "{"
      , e
      , "}"
      , "{"
      , l
      , "}"
      ]

sikrokSectionToTex :: SikrokSection -> Text
sikrokSectionToTex SikrokSection
              { sk_s_header = h
              , sk_s_entries = es
              } = T.intercalate "\n" $ header : contents
  where
    header = mconcat
      [ "\\SikrokSection{"
      , h
      , "}"
      ]
    contents = map sikrokEntryToTex es

sikrokSectionsToTex :: [SikrokSection] -> Text
sikrokSectionsToTex sss = T.intercalate "\n" $ concat [[header], contents, [footer]]
  where
    header = mconcat
      [ "\\begin{SikrokIndex}"
      ]
    contents = map sikrokSectionToTex sss
    footer = mconcat
      [ "\\end{SikrokIndex}"
      ]

entryToSikrokEntries :: Entry -> [SikrokEntry]
entryToSikrokEntries e = concat $ concat
  [ [ indexSikroks svParent ]
  , map (indexSikrokVariants svParent) $ s_選 sv
  , map (indexSikrokVariants svParent) $ s_簡 sv
  , map (indexSikrokVariants svParent) $ s_日 sv
  , map (indexSikrokVariants svParent) $ s_异 sv
  ]
  where
    indexSikroks ShapeVariant{s_字 = z, s_四角 = sks} = map (indexSikrok z) $ NEL.toList sks
    indexSikrok z sk = SikrokEntry
      { sk_e_sikrok = sk
      , sk_e_entry = z
      , sk_e_label = label
      , sk_e_headword = Nothing
      }
    label = entryToLabel e
    svParent = s_親 sv
    sv = e_shapeVariants $ e
    indexSikrokVariants ShapeVariant{s_字 = zp, s_四角 = _} ShapeVariant{s_字 = z, s_四角 = sks} =
      map (indexSikrokVariant zp z) $ NEL.toList sks
    indexSikrokVariant zp z sk = SikrokEntry
      { sk_e_sikrok = sk
      , sk_e_entry = z
      , sk_e_label = label
      , sk_e_headword = Just zp
      }

sectionsToSikrokSections :: [Section] -> [SikrokSection]
sectionsToSikrokSections ss = [section]
  where
    ses = concatMap entryToSikrokEntries es
    es :: [Entry]
    es = concatMap (concat . sec_entries) ss
    section = SikrokSection
      { sk_s_header = "test"
      , sk_s_entries = ses
      }
