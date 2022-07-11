{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Lib.Index.RadicalIndex where

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
import Lib.PathTree qualified as PT
import Lib.Entry (renderSikrokToTex, Entry(..), EntrySortKey(..), Section(..), entryToLabel, entrySortKey)
import Lib.Row (ShapeVariant(..), ShapeVariants(..), Part(..), Shape部畫(..))
import Text.Printf (printf)

data RadicalEntry = RadicalEntry
  { sk_r_radical :: !Shape部畫
  , sk_r_entry :: !Text
  , sk_r_label :: !Text
  , sk_r_sortKey :: !EntrySortKey
  }
  deriving (Read, Show, Eq, Ord, Generic)

data RadicalSection = RadicalSection
  { sk_r_header :: !Text
  , sk_r_entries :: ![RadicalEntry]
  }
  deriving (Read, Show, Eq, Ord, Generic)

radicalEntryToTex :: RadicalEntry -> Text
radicalEntryToTex RadicalEntry
  { sk_r_radical = r
  , sk_r_entry = e
  , sk_r_label = l
  } = mconcat
      [ "\\RadicalEntry"
      , "{"
      , l
      , "}"
      , "{"
      , s_部 r
      , "}"
      , "{"
      , T.pack . show $ s_畫 r
      , "}"
      , "{"
      , e
      , "}"
      ]

radicalSectionToTex :: RadicalSection -> Text
radicalSectionToTex RadicalSection
              { sk_r_header = h
              , sk_r_entries = es
              } = T.intercalate "\n" $ header : contents
  where
    header = mconcat
      [ "\\RadicalSection{"
      , h
      , "}"
      ]
    contents = map radicalEntryToTex es

radicalSectionToIndexTex :: RadicalSection -> Text
radicalSectionToIndexTex RadicalSection
              { sk_r_header = h
              , sk_r_entries = es
              } = mconcat
                  [ "{"
                  , h
                  , "}"
                  ]

radicalSectionsToTex :: [RadicalSection] -> Text
radicalSectionsToTex sss = T.intercalate "\n" $ concat [[header], contents, [footer]]
  where
    header = mconcat
      [ "\\begin{RadicalIndex}"
      , "{"
      , mconcat $ map radicalSectionToIndexTex sss
      , "}"
      ]
    contents = map radicalSectionToTex sss
    footer = mconcat
      [ "\\end{RadicalIndex}"
      ]

entryToRadicalEntries :: [Part] -> Entry -> [RadicalEntry]
entryToRadicalEntries p e = indexRadicals svParent (e_部畫 e)
  where
    indexRadicals ShapeVariant{s_字 = z, s_四角 = _} Nothing = []
    indexRadicals ShapeVariant{s_字 = z, s_四角 = _} (Just r) =
      [ RadicalEntry
        { sk_r_radical = r
        , sk_r_entry = z
        , sk_r_label = label
        , sk_r_sortKey = entrySortKey e
        }
      ]
    label = entryToLabel e
    svParent = s_親 sv
    sv = e_shapeVariants $ e

sectionsToRadicalSections :: [Section] -> [RadicalSection]
sectionsToRadicalSections ss = sections
  where
    ses = M.fromListWith (++) . map (\e -> (s_部 $ sk_r_radical e, [e])) $ concatMap (uncurry entryToRadicalEntries) es
    es :: [([Part], Entry)]
    es = concatMap (concatMap (\(p, vs) -> map (\v -> (p, v)) vs) . PT.toList . sec_entries) ss
    sections =
      map (\(p, es) -> RadicalSection
            { sk_r_header = p
            , sk_r_entries = L.sortOn (\e -> (s_畫 $ sk_r_radical e, sk_r_sortKey e)) es
            }) $ M.toList ses
