{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Lib.Index.GroupIndex where

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
import Lib.Entry (soundPartNumberToTex, Group(..), Section(..), entryToLabel, entrySortKey)
import Lib.IDS (convIdsFull)
import Lib.Row (ShapeVariant(..), ShapeVariants(..), Part(..))
import Text.Printf (printf)


groupToEntryTex :: Group -> Text
groupToEntryTex g = mconcat
    [ "\\GroupIndexEntry"
    , "{"
    , grp_mainRhyme_text g
    , "}"
    , "{"
    , mconcat . map sectionToTex . Foldable.toList $ grp_sections g
    , "}"
    ]
  where
    sectionToTex :: Section -> Text
    sectionToTex s = mconcat
      [ "{"
      , "{"
      , soundPartNumberToTex $ sec_諧符位 s
      , "}"
      , "{"
      , sec_諧符部 s
      , "}"
      , "}"
      ]

groupsToIndexTex :: [Group] -> Text
groupsToIndexTex gs = T.intercalate "\n" $ concat [[header], contents, [footer]]
  where
    header = mconcat
      [ "\\begin{GroupIndex}"
      ]
    contents = map groupToEntryTex gs
    footer = mconcat
      [ "\\end{GroupIndex}"
      ]
