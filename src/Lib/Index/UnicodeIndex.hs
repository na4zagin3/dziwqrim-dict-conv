{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Lib.Index.UnicodeIndex where

import Codec.QRCode qualified as QR
import Control.Arrow (first)
import Data.Char as Char
import Data.Either (fromRight, partitionEithers)
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
import Lib.IDS (convIdsFull)
import Lib.Row (ShapeVariant(..), ShapeVariants(..), Part(..))
import Text.Printf (printf)

data UnicodeBlock =
  CJKUnified
  | CJKUnifiedExtA
  | CJKUnifiedExtB
  | CJKUnifiedExtC
  | CJKUnifiedExtD
  | CJKUnifiedExtE
  | CJKUnifiedExtF
  | CJKUnifiedExtG
  | Other
  deriving (Read, Show, Eq, Ord, Generic)

data UnicodeEntry = UnicodeEntry
  { sk_e_unicode :: ![Char]
  , sk_e_entry :: !Text
  , sk_e_label :: !Text
  , sk_e_sortKey :: !EntrySortKey
  , sk_e_headword :: !(Maybe Text)
  }
  deriving (Read, Show, Eq, Ord, Generic)

data UnicodeSection = UnicodeSection
  { sk_s_header :: !Text
  , sk_s_entries :: ![UnicodeEntry]
  }
  deriving (Read, Show, Eq, Ord, Generic)

unicodeEntryToTex :: UnicodeEntry -> Text
unicodeEntryToTex UnicodeEntry
  { sk_e_unicode = sk
  , sk_e_entry = e
  , sk_e_headword = hw
  , sk_e_label = l
  } = mconcat contents
  where
    contents =
      [ "\\UnicodeEntry"
      , "{"
      , l
      , "}"
      , "{"
      , T.intercalate " " $ map charToUnicodeNotations sk
      , "}"
      , fromMaybe "" $ fmap (\x -> "[" <> x <> "]") hw
      , "{"
      , e
      , "}"
      ]

unicodeSectionToTex :: UnicodeSection -> Text
unicodeSectionToTex UnicodeSection
              { sk_s_header = h
              , sk_s_entries = es
              } = T.intercalate "\n" $ concat [[header], contents, [footer]]
  where
    header = mconcat
      [ "\\begin{UnicodeSection}{"
      , h
      , "}"
      ]
    contents = map unicodeEntryToTex es
    footer = mconcat
      [ "\\end{UnicodeSection}"
      ]

unicodeSectionsToTex :: [UnicodeSection] -> Text
unicodeSectionsToTex sss = T.intercalate "\n" $ concat [[header], contents, [footer]]
  where
    header = mconcat
      [ "\\begin{UnicodeIndex}"
      ]
    contents = map unicodeSectionToTex sss
    footer = mconcat
      [ "\\end{UnicodeIndex}"
      ]

entryToUnicodeEntries :: [Part] -> Entry -> [UnicodeEntry]
entryToUnicodeEntries p e = concat $ concat
  [ [ indexUnicodes svParent ]
  , map (indexUnicodeVariants "\\textHans" svParent) $ s_選 sv
  , map (indexUnicodeVariants "\\textHans" svParent) $ s_簡 sv
  , map (indexUnicodeVariants "\\textJapn" svParent) $ s_日 sv
  , map (indexUnicodeVariants "\\textKor"  svParent) $ s_韓 sv
  , map (indexUnicodeVariants "\\textHans" svParent) $ s_异 sv
  ]
  where
    convIdsExc = (fromRight (error "BUG: invalid IDS found")) . convIdsFull
    indexUnicodes ShapeVariant{s_字 = z} = [indexUnicode z]
    indexUnicode z = UnicodeEntry
      { sk_e_unicode = T.unpack $ convIdsExc z
      , sk_e_entry = z
      , sk_e_label = label
      , sk_e_sortKey = entrySortKey e
      , sk_e_headword = Nothing
      }
    label = entryToLabel e
    svParent = s_親 sv
    sv = e_shapeVariants $ e
    indexUnicodeVariants com ShapeVariant{s_字 = zp} ShapeVariant{s_字 = z} =
      [indexUnicodeVariant com zp z]
    indexUnicodeVariant com zp z = UnicodeEntry
      { sk_e_unicode = T.unpack $ convIdsExc z
      , sk_e_entry = mconcat [com, "{", z, "}"]
      , sk_e_label = label
      , sk_e_sortKey = entrySortKey e
      , sk_e_headword = Just zp
      }

charToUnicodeNotations :: Char -> Text
charToUnicodeNotations = charConv . Char.ord
  where
    charConv c = T.pack $ printf "%04X" c

blockToHeader :: UnicodeBlock -> Text
blockToHeader CJKUnified = "CJK統合漢字"
blockToHeader CJKUnifiedExtA = "CJK統合漢字拡張A"
blockToHeader CJKUnifiedExtB = "CJK統合漢字拡張B"
blockToHeader CJKUnifiedExtC = "CJK統合漢字拡張C"
blockToHeader CJKUnifiedExtD = "CJK統合漢字拡張D"
blockToHeader CJKUnifiedExtE = "CJK統合漢字拡張E"
blockToHeader CJKUnifiedExtF = "CJK統合漢字拡張F"
blockToHeader CJKUnifiedExtG = "CJK統合漢字拡張G"
blockToHeader Other = "その他"

stringToBlock :: String -> UnicodeBlock
stringToBlock (c:_) | Char.ord c >= 0x4e00 && Char.ord c <= 0x9ffc = CJKUnified
                    | Char.ord c >= 0x3400 && Char.ord c <= 0x4dbf = CJKUnifiedExtA
                    | Char.ord c >= 0x20000 && Char.ord c <= 0x2a6dd = CJKUnifiedExtB
                    | Char.ord c >= 0x2a700 && Char.ord c <= 0x2b734 = CJKUnifiedExtC
                    | Char.ord c >= 0x2b740 && Char.ord c <= 0x2b81d = CJKUnifiedExtD
                    | Char.ord c >= 0x2b820 && Char.ord c <= 0x2cea1 = CJKUnifiedExtE
                    | Char.ord c >= 0x2ceb0 && Char.ord c <= 0x2ebe0 = CJKUnifiedExtF
                    | Char.ord c >= 0x30000 && Char.ord c <= 0x3a34a = CJKUnifiedExtG
stringToBlock _ = Other

sectionsToUnicodeSections :: [Section] -> [UnicodeSection]
sectionsToUnicodeSections ss = sections
  where
    ses = L.sortOn (\e -> (sk_e_unicode e, sk_e_sortKey e)) $ concatMap (uncurry entryToUnicodeEntries) es
    es :: [([Part], Entry)]
    es = concatMap (concatMap (\(p, vs) -> map (\v -> (p, v)) vs) . PT.toList . sec_entries) ss
    eToMap e = M.singleton (stringToBlock $ sk_e_unicode e) [e]
    pairToSection (b, es) = UnicodeSection
      { sk_s_header = blockToHeader b
      , sk_s_entries = es
      }
    sections = map pairToSection .  M.toList . M.unionsWith mappend $ map eToMap ses
