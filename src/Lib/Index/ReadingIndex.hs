{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Lib.Index.ReadingIndex where

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
import Lib.Entry (renderSikrokToTex, Entry(..), Entry音義(..), EntrySortKey(..), Section(..), entryToLabel, entrySortKey)
import Lib.Row (ShapeVariant(..), ShapeVariants(..), Part(..), Shape部畫(..))
import Text.Printf (printf)
import Data.Text.ICU qualified as ICU

data ReadingEntry = ReadingEntry
  { sk_r_reading :: !Text
  , sk_r_entry :: !Text
  , sk_r_label :: !Text
  , sk_r_sortKey :: !EntrySortKey
  }
  deriving (Read, Show, Eq, Ord, Generic)

data ReadingSection = ReadingSection
  { sk_r_header :: !Text
  , sk_r_note :: !(Maybe Text)
  , sk_r_entries :: ![ReadingEntry]
  }
  deriving (Read, Show, Eq, Ord, Generic)

readingEntryToTex :: ReadingEntry -> Text
readingEntryToTex ReadingEntry
  { sk_r_reading = r
  , sk_r_entry = e
  , sk_r_label = l
  } = mconcat
      [ "\\ReadingEntry"
      , "{"
      , l
      , "}"
      , "{"
      , r
      , "}"
      , "{"
      , e
      , "}"
      ]

readingSectionToTex :: ReadingSection -> Text
readingSectionToTex ReadingSection
              { sk_r_header = h
              , sk_r_note = n
              , sk_r_entries = es
              } = T.intercalate "\n" $ concat [[header], contents, [footer]]
  where
    header = mconcat
      [ "\\begin{ReadingSection}{"
      , h
      , "}"
      , fromMaybe "" $ fmap (\nstr -> "[" <> nstr <> "]") n
      ]
    contents = map readingEntryToTex es
    footer = mconcat
      [ "\\end{ReadingSection}"
      ]

readingSectionsToTex :: [ReadingSection] -> Text
readingSectionsToTex sss = T.intercalate "\n" $ concat [[header], contents, [footer]]
  where
    header = mconcat
      [ "\\begin{ReadingIndex}"
      , "{"
      , mconcat . map (\h -> "{" <> h <> "}"). L.sortOn (ICU.sortKey collator) $ map sk_r_header sss
      , "}"
      ]
    contents = map readingSectionToTex sss
    footer = mconcat
      [ "\\end{ReadingIndex}"
      ]

entryToReadingEntries :: [Part] -> Entry -> [ReadingEntry]
entryToReadingEntries p e = map (indexReadings svParent) $ e_音義 e
  where
    indexReadings ShapeVariant{s_字 = z, s_四角 = _} Entry音義{e_隋音 = r} =
      ReadingEntry
      { sk_r_reading = r
      , sk_r_entry = z
      , sk_r_label = label
      , sk_r_sortKey = entrySortKey e
      }

    label = entryToLabel e
    svParent = s_親 sv
    sv = e_shapeVariants $ e

sectionsToReadingSections :: [Section] -> [ReadingSection]
sectionsToReadingSections ss = sections
  where
    ses = M.fromListWith (++) . map (\e -> (takeInitial $ sk_r_reading e, [e])) $ concatMap (uncurry entryToReadingEntries) es
    takeInitial t | T.take 2 t `elem` ["dz", "kh", "ph", "qh", "sh", "th", "zh"] = T.take 2 t
                  | otherwise = T.take 1 t
    es :: [([Part], Entry)]
    es = concatMap (concatMap (\(p, vs) -> map (\v -> (p, v)) vs) . PT.toList . sec_entries) ss
    sections =
      map (\(p, es) -> lookupSection p es) . L.sortOn (\(k, _) -> ICU.sortKey collator k) $ M.toList ses
    lookupSection p es = ReadingSection
      { sk_r_header = h
      , sk_r_note = Just n
      , sk_r_entries = L.sortOn (\e -> (ICU.sortKey collator . normalizeForCollation $ sk_r_reading e, sk_r_sortKey e)) es
      }
      where
        (h, n) = fromMaybe (error $ printf "Undefined header: %s" p) $ MS.lookup p headerMap

normalizeForCollation :: Text -> Text
normalizeForCollation = T.replace "'" "ʼ"

headerMap :: MS.Map Text (Text, Text)
headerMap = MS.fromList
  [ ("b", ("B", "並奉"))
  , ("c", ("C", "精莊"))
  , ("ch", ("Ch", "清初"))
  , ("d", ("D", "定澄禪"))
  , ("dz", ("Dz", "從崇"))
  , ("g", ("G", "群"))
  , ("j", ("J", "以"))
  , ("k", ("K", "見"))
  , ("kh", ("Kh", "溪"))
  , ("l", ("L", "來"))
  , ("m", ("M", "明微"))
  , ("n", ("N", "泥娘日"))
  , ("ŋ", ("Ŋ", "疑"))
  , ("p", ("P", "幫非"))
  , ("ph", ("Ph", "滂敷"))
  , ("q", ("Q", "影"))
  , ("qh", ("Qh", "曉"))
  , ("s", ("S", "心生"))
  , ("sh", ("Sh", "書"))
  , ("t", ("T", "端知章"))
  , ("th", ("Th", "透徹昌"))
  , ("x", ("X", "匣云"))
  , ("z", ("Z", "邪崇"))
  , ("zh", ("Zh", "船"))
  ]

-- | ICU collator
collator :: ICU.Collator
collator = case ICU.collatorFrom rules Nothing Nothing of
             Right c -> c
             Left e -> error $ "Failed to interpret ICU collation rules: " <> show e
  where
    rules =
      "&a < b < c < ch < d < dz < e < ə < g < i < j < k < kh < l < m < n < ŋ < o < p < ph < q < qh < r < s < sh < t < th < u < x < y < z < zh\
      \& [last tertiary ignorable] <<< ʼ\
      \"
