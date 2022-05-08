{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Lib.Entry where

import Control.Arrow (first)
import Data.Either (partitionEithers)
import Data.Foldable qualified as Foldable
import Data.Function (on)
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NEL
import Data.Map qualified as M
import Data.Map.Strict qualified as MS
import Data.Maybe (fromMaybe, catMaybes, fromMaybe)
import Data.Maybe qualified as Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEV
import GHC.Generics (Generic)
import Text.Printf (printf)

import Lib.Row
import Lib.PathTree (PathTree)
import Lib.PathTree qualified as PT

data Entry音義 = Entry音義
  { e_隋音 :: !Text
  , e_pronunciation :: !Pronunciation
  , e_義 :: !(Maybe Text)
  }
  deriving (Read, Show, Eq, Ord, Generic)

data Entry音 = Entry音
  deriving (Read, Show, Eq, Ord, Generic)

-- Entry
data Entry = Entry
  { e_position :: !(Set Position)
  , e_字 :: !Text
  , e_shapeVariants :: !ShapeVariants
  , e_部畫 :: !(Maybe Shape部畫)
  , e_parts :: !Parts
  , e_音義 :: ![Entry音義]
  }
  deriving (Read, Show, Eq, Ord, Generic)

data Section = Section
  { sec_諧符位 :: !Int
  , sec_諧符部 :: !Text
  , sec_entries :: !(PathTree Part [Entry])
  }
  deriving (Read, Show, Eq, Generic)

-- | Group items with key generated by the specified function
--
-- Examples

groupBy :: (Ord k) => (a -> k) -> [a] -> MS.Map k (NonEmptyVector a)
groupBy f xs = MS.unionsWith (<>) ms
  where
    ms = map (\x -> MS.singleton (f x) (NEV.singleton x)) xs

addEntryToTree :: Entry -> PathTree Part [Entry] -> PathTree Part [Entry]
addEntryToTree e t = PT.insertWith (<>) path [e] t
  where
    path = p_parts $ e_parts e

sectionsFromRows :: (Foldable f) => f Row -> ([String], [Section])
sectionsFromRows rs = first concat . unzip . map renderSection $ MS.toList sectionMap
  where
    sectionMap = groupBy (\r -> (p_諧符位 $ r_parts r, p_諧符部 $ r_parts r)) $ Foldable.toList rs
    renderSection (k, v) = (errors, section)
      where
        (errors, f_entries) = entriesFromRows $ NEV.toList v
        section = Section
                  { sec_諧符位 = fst k
                  , sec_諧符部 = snd k
                  , sec_entries = foldr addEntryToTree PT.empty f_entries
                  }

entriesFromRows :: (Functor f, Foldable f) => f Row -> ([String], [Entry])
entriesFromRows rs = (errors, L.sortBy (compare `on` e_position) groupedEntries)
  where
    undedupedEntries = Foldable.toList $ fmap entryFromRow rs
    entriesByHeadword = MS.map (NEV.foldl1' liftedMergeEntry . NEV.map Right) $ groupBy e_字 undedupedEntries
    liftedMergeEntry :: Either String Entry -> Either String Entry -> Either String Entry
    liftedMergeEntry = (\ma mb -> ma >>= \a -> mb >>= \b -> mergeEntry a b)
    (errors, groupedEntries) = partitionEithers . map snd . MS.toList $ entriesByHeadword
    entryFromRow r = Entry
                     { e_position = S.singleton $ r_position r
                     , e_字 = r_字 r
                     , e_shapeVariants = r_shapeVariants r
                     , e_部畫 = r_部畫 r
                     , e_parts = r_parts r
                     , e_音義 = [Entry音義
                                 { e_隋音 = r_隋音 r
                                 , e_pronunciation = r_pronunciation r
                                 , e_義 = r_義 r
                                 }]
                     }

compareAndTakeOne :: (Show pos, Eq b, Show b) => pos -> String -> (a -> b) -> a -> a -> Either String b
compareAndTakeOne pos label f a b =
  if f a == f b
  then Right $ f a
  else Left $ printf "%s: Mismatched %s: %s vs. %s" (show pos) label (show $ f a) (show $ f b)

mergeEntry :: Entry -> Entry -> Either String Entry
mergeEntry a b = do
  let f_position = e_position a <> e_position b
  f_字 <- compareAndTakeOne f_position "字" e_字 a b
  f_shapeVariants <- compareAndTakeOne f_position "shape variants" e_shapeVariants a b
  f_部畫 <- compareAndTakeOne f_position "部畫" e_部畫 a b
  f_parts <- compareAndTakeOne f_position "parts" e_parts a b
  let f_音義 = e_音義 a <> e_音義 b
  return $ Entry
    { e_position = f_position
    , e_字 = f_字
    , e_shapeVariants = f_shapeVariants
    , e_部畫 = f_部畫
    , e_parts = f_parts
    , e_音義 = f_音義
    }

variantToTex :: Text -> [ShapeVariant] -> Text -> Maybe Text
variantToTex _ [] _ = Nothing
variantToTex label shapes pre = Just $ mconcat
    [ label
    , "作"
    , T.intercalate "、" quoted
    ]
  where
    quoted = map (\x -> "“" <> pre <> "{" <> s_字 x <> "}”") shapes

shapeVariantsToTex :: ShapeVariants -> Text
shapeVariantsToTex s = mconcat
    [ "  ", renderSikrok . s_四角 . s_親 $ s
    , "\\quad "
    , T.intercalate "，又" variantDescs
    ]
  where
    renderSikrok (sm, ss) = "\\Sikrok{" <> sm <> "}{" <> ss <> "}"
    variantDescs = Maybe.catMaybes $ map (\(l, c, p) -> variantToTex l c p)
      [ ("選\\footnote{what is 選?}", s_選 s, "")
      , ("簡", s_簡 s, "\\textHans") -- TODO Special case where Simplified and Japanese shapes are identical
      , ("日", s_日 s, "\\textJapn")
      , ("或", s_异 s, "") -- TODO
      ]

shape部畫ToTex :: Shape部畫 -> Text
shape部畫ToTex s = mconcat
  [ "（"
  , s_部 s
  , "部"
  , T.pack . show . s_畫 $ s
  , "畫）"
  ]

soundPartToTex :: Part -> Text
soundPartToTex Part{ p_玉篇部首位 = (i, p), p_部外 = e} = T.pack $ printf "\\SoundPart{%d%s}{%s}" i (primes p) e
  where
    primes :: Int -> String
    primes 0 = ""
    primes 1 = "′"
    primes 2 = "″"
    primes 3 = "‴"
    primes 4 = "⁗"
    primes n = error $ "Unsupported number of primes: " <> show n

soundPartsToTex :: Parts -> Text
soundPartsToTex sps = case sps of
    Parts{ p_variant = False, p_parts = [] } -> "\\SoundPartN{0}"
    Parts{ p_variant = True, p_parts = [] } -> "\\SoundPartN{0′}"
    Parts{ p_variant = False, p_parts = ps } -> texSoundParts ps
    Parts{ p_variant = True, p_parts = ps } -> "\\SoundPartN{0′} " <> texSoundParts ps
  where
    texSoundParts ps = T.intercalate " " $ map soundPartToTex ps

booksToTex :: NonEmpty Text -> Text
booksToTex = mconcat . NEL.toList . NEL.map (\b -> "\\Book{" <> b <> "}")


-- | Parse a 反切 as TeX
--
-- Examples
--
-- >>> pronunciation反切ToTex (Pronunciation反切 {pr_反切 = Nothing, pr_反切_suffix = "\21453", pr_反切_comment = Just "\12394\12375", pr_反切_books = NEL.singleton "\21453\20999"})
-- "\\Book{\21453\20999}\12394\12375"

pronunciation反切ToTex :: Pronunciation反切 -> Maybe Text
pronunciation反切ToTex Pronunciation反切
  { pr_反切 = Nothing
  , pr_反切_suffix = _
  , pr_反切_comment = Nothing
  , pr_反切_books = _
  } = Nothing
pronunciation反切ToTex Pronunciation反切
  { pr_反切 = Nothing
  , pr_反切_suffix = _
  , pr_反切_comment = Just com
  , pr_反切_books = bs
  } = Just $ booksToTex bs <> com
pronunciation反切ToTex Pronunciation反切
  { pr_反切 = Just pc
  , pr_反切_suffix = suf
  , pr_反切_comment = Nothing
  , pr_反切_books = bs
  } = Just $ booksToTex bs <> pc <> suf

pronunciation反切ToTex Pronunciation反切
  { pr_反切 = Just pc
  , pr_反切_suffix = suf
  , pr_反切_comment = Just com
  , pr_反切_books = bs
  } = Just $ booksToTex bs <> pc <> suf <> "（" <> com <> "）"

pronunciation反切集ToTex :: Pronunciation反切集 -> [Text]
pronunciation反切集ToTex Pronunciation反切集
  { pr_切韵反切 = pC
  , pr_王韵反切 = pU
  , pr_廣韵反切 = pK
  , pr_集韵反切 = pDz
  } = catMaybes $ map (>>= pronunciation反切ToTex) [pC, pU, pK, pDz]

pronunciationToTex :: Pronunciation -> Text
pronunciationToTex Pronunciation
  { -- pr_韵部 :: !Text
  -- ,
    pr_反切集 = pc
  -- , pr_漢辭海 :: !(Maybe Pronunciation漢辭海)
  -- , pr_辭源韵 :: !(Maybe Pronunciation辭源韵)
  } = T.intercalate "，" $ pronunciation反切集ToTex pc

entryToTex :: Entry -> Text
entryToTex e = mconcat
    [ "\\noindent"
    , "\\Position{"
    , T.intercalate "; " . map (T.pack . printf "%d" . pos_row) . S.toList $ e_position e
    , "}"
    , "\n\n"
    , "\\begin{Entry}{", e_字 e, "}{", e_字 e, "}\n"
    , soundPartsToTex $ e_parts e
    , fromMaybe "" . fmap shape部畫ToTex $ e_部畫 e
    , "  \\\\\n"
    , "  ", shapeVariantsToTex $ e_shapeVariants e, "\n"
    , "  \\begin{Sound}\n"
    , T.intercalate "\n" tex音Items
    , "\n"
    -- , "    \\SoundItem{toŋ}《P3798》都宗反，端冬平，上平聲二冬\n"
    , "  \\end{Sound}\n"
    , if null tex義Items
      then ""
      else "\\begin{Sense}\n" <> T.intercalate "\n" tex義Items <> "\n\\end{Sense}"
    , "\\end{Entry}\n"
    ]
  where
    tex音Items = map (\sp -> "\\SoundItem{" <> e_隋音 sp <> "}" <> (pronunciationToTex $ e_pronunciation sp)) $ e_音義 e
    tex義Items = catMaybes . map render義Item $ e_音義 e
    render義Item sp = do
      s <- e_義 sp
      return $ "\\SenseItem{" <> e_隋音 sp <> "}" <> s

unfoldTreeToTex :: (Ord a) => Bool -> PathTree a Text -> Text
unfoldTreeToTex isRoot t = fromMaybe "" (PT.root t) <> brackettedChildrenStr
  where
    children = M.toList $ PT.children t
    childrenStr = mconcat (map (unfoldTreeToTex False . snd) $ children)
    brackettedChildrenStr = if null children || isRoot
      then childrenStr
      else "（" <> childrenStr <> "）"

entriesToHeadingsTex :: (Ord a) => PathTree a [Entry] -> Text
entriesToHeadingsTex pt = unfoldTreeToTex True $ fmap renderEntries pt
  where
    renderEntries es = T.concat $ map renderEntry es
    renderEntry e = "\\refEntry{" <> e_字 e <> "}"

sectionToTex :: Section -> Text
sectionToTex s = mconcat
    [ "\\PartHeader{"
    , sec_諧符部 s
    , "符"
    , "}{"
    , entriesToHeadingsTex $ sec_entries s
    , "}"
    , T.intercalate "\n" . map entryToTex . concatMap snd . PT.toList $ sec_entries s
    , "\n\n"
    ]