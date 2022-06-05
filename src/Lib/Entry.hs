{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Lib.Entry where

import Control.Arrow (first)
import Codec.QRCode qualified as QR
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
  { sec_諧符位 :: !(Int, Int)
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
    sectionMap = groupBy (\r -> (p_諧聲位 $ r_parts r, p_諧聲部 $ r_parts r)) $ Foldable.toList rs
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
    , T.intercalate "、" quoted
    ]
  where
    quoted = map (\x -> "“" <> pre <> "{" <> s_字 x <> "}”") shapes

renderSikrokToTex :: (Text, Text) -> Text
renderSikrokToTex (sm, ss) = "\\Sikrok{" <> sm <> "}{" <> ss <> "}"

renderSikroksToTex :: NonEmpty (Text, Text) -> Text
renderSikroksToTex sks = T.intercalate "又" $ map renderSikrokToTex $ NEL.toList sks

shapeVariantsToTex :: ShapeVariants -> Text
shapeVariantsToTex s = mconcat
    [ renderSikroksToTex . s_四角 . s_親 $ s
    , "\\quad "
    , T.intercalate "，" $ variantDescs <> maybeToList variantDescsPost
    ]
  where
    variantDescs = Maybe.catMaybes $ map (\(l, c, p) -> variantToTex l c p) $
      [ ("選用", s_選 s, "") ]
      <> variantSimpJapn (s_簡 s) (s_日 s)
    variantDescsPost = variantToTex "或作" (s_异 s) ""
    variantSimpJapn [simp] [japn] | simp == japn = [("簡・日作", [simp], "\\textHans")]
    variantSimpJapn simp japn =
      [ ("簡作", simp, "\\textHans")
      , ("日作", japn, "\\textJapn")
      ]

shape部畫ToTex :: Shape部畫 -> Text
shape部畫ToTex s = mconcat
  [ "（"
  , s_部 s
  , "部"
  , T.pack . show . s_畫 $ s
  , "畫）"
  ]

soundPartNumberToTex :: (Int, Int) -> Text
soundPartNumberToTex (i, p) = T.pack $ printf "%d%s" i (primes p)
  where
    primes :: Int -> String
    primes 0 = ""
    primes 1 = "′"
    primes 2 = "″"
    primes 3 = "‴"
    primes 4 = "⁗"
    primes n = error $ "Unsupported number of primes: " <> show n

soundPartToTex :: Bool -> Part -> Text
soundPartToTex mainText Part{ p_玉篇部首位 = k, p_部外 = e} =
    if mainText
    then T.pack $ printf "\\SoundPart{%s}{%s}" (soundPartNumberToTex k) e
    else T.pack $ printf "\\SoundPartNI{%s}" (soundPartNumberToTex k)

-- | Generate a text content for phonetic parts
--
-- >>> putStrLn . T.unpack $ soundPartsToTex True $ Parts {p_諧聲部 = "夋", p_諧聲位 = 80, p_parts = [Part {p_玉篇部首位 = (23, 0), p_部外 = "人"}], p_variants = S.fromList [1]}
-- \SoundParts{(80 + \SoundPart{23}{人})′}
-- >>> putStrLn . T.unpack $ soundPartsToTex False $ Parts {p_諧聲部 = "夋", p_諧聲位 = 80, p_parts = [Part {p_玉篇部首位 = (23, 0), p_部外 = "人"}], p_variants = S.fromList [1]}
-- \SoundParts{(80+\SoundPartNI{23})′}
--
soundPartsToTex :: Bool -> Parts -> Text
soundPartsToTex mainText sps = "\\SoundParts{" <> content <> "}"
  where
    content = parenthesize mainText texSoundPartsEither
    texSoundPartsEither = rightIndexes (p_variants sps) texSoundParts
    texSoundParts = (soundPartNumberToTex $ p_諧聲位 sps) : (map (soundPartToTex mainText) $ p_parts sps)
    concatParts = if mainText then T.intercalate " " else T.concat
    command = if mainText then "\\SoundPartN" else "\\SoundPartNI"

parenthesize
  :: (IsString c, Semigroup c) =>
     Bool -> [Either c c] -> c
parenthesize False = parenthesize' (\x y -> x <> "+" <> y) . reverse
parenthesize True = parenthesize' (\x y -> x <> " + " <> y) . reverse

parenthesize' app = f
  where
    f [] = ""
    f [Right x] = x <> "′"
    f [Left x] = x
    f (Right x:xs) = "(" <> app (f xs) x <> ")′"
    f (Left x:xs) = app (f xs) x

rightIndexes :: Set Int -> [a] -> [Either a a]
rightIndexes is xs = map f $ zip [0..] xs
  where
    f (i, x) | i `S.member` is = Right x
             | otherwise = Left x

booksToTex :: NonEmpty Text -> Text
booksToTex = mconcat . NEL.toList . NEL.map (\b -> "\\Book{" <> b <> "}")


-- | Parse a 反切 as TeX
--
-- Examples
--
-- >>> pronunciation反切ToTex True (Pronunciation反切 {pr_反切 = Nothing, pr_反切_comment = Just "\12394\12375", pr_反切_books = NEL.singleton "\21453\20999"})
-- Just "\\Book{\21453\20999}\12394\12375"
--
-- >>> pronunciation反切ToTex False (Pronunciation反切 {pr_反切 = Nothing, pr_反切_comment = Just "\12394\12375", pr_反切_books = NEL.singleton "\21453\20999"})
-- Just "\12394\12375"

pronunciation反切ToTex :: Bool -> Pronunciation反切 -> Maybe Text
pronunciation反切ToTex showBook fq =
    addBooks $ pronunciation反切ContentToTex fq
  where
    addBooks = if showBook
      then fmap (booksToTex (pr_反切_books fq) <>)
      else id

pronunciation反切ContentToTex :: Pronunciation反切 -> Maybe Text
pronunciation反切ContentToTex Pronunciation反切
  { pr_反切 = Nothing
  , pr_反切_comment = Nothing
  , pr_反切_books = _
  } = Nothing
pronunciation反切ContentToTex Pronunciation反切
  { pr_反切 = Nothing
  , pr_反切_comment = Just com
  , pr_反切_books = _
  } = Just com
pronunciation反切ContentToTex Pronunciation反切
  { pr_反切 = Just (pc, suf)
  , pr_反切_comment = Nothing
  , pr_反切_books = _
  } = Just $ pc <> suf

pronunciation反切ContentToTex Pronunciation反切
  { pr_反切 = Just (pc, suf)
  , pr_反切_comment = Just com
  , pr_反切_books = _
  } = Just $ pc <> suf <> "（" <> com <> "）"

pronunciation漢辭海ToTex :: Pronunciation漢辭海 -> Text
pronunciation漢辭海ToTex Pronunciation漢辭海
  { pr_漢辭海_聲 = c
  , pr_漢辭海_韵 = v
  , pr_漢辭海_調 = t
  } = c <> v <> t

pronunciation辭源韵ToTex :: Pronunciation辭源韵 -> Text
pronunciation辭源韵ToTex Pronunciation辭源韵
  { pr_辭源韵_韵字 = v
  , pr_辭源韵_用例 = Just com
  } = v <> "（" <> com <> "）"
pronunciation辭源韵ToTex Pronunciation辭源韵
  { pr_辭源韵_韵字 = v
  , pr_辭源韵_用例 = Nothing
  } = v

pronunciation反切集ToTex :: Pronunciation反切集 -> [Text]
pronunciation反切集ToTex Pronunciation反切集
  { pr_切韵反切 = pC
  , pr_王韵反切 = pUs
  , pr_廣韵反切 = pK
  , pr_集韵反切 = pDz
  } = map (T.intercalate "，") $ filter (not . null)
      [ f "" pC
      , f "王韵" pUs
      , f "廣韵" pK
      , f "" pDz
      ]
  where
    f :: Text -> [Pronunciation反切] -> [Text]
    f _ [] = []
    f b [p] | pr_反切_books p == NEL.singleton b = maybeToList $ pronunciation反切ToTex False p
            | otherwise = maybeToList $ pronunciation反切ToTex True p
    f _ ps = catMaybes $ map (pronunciation反切ToTex True) ps

pronunciationToTex :: Pronunciation -> Text
pronunciationToTex Pronunciation
  { -- pr_韵部 :: !Text
  -- ,
    pr_反切集 = pc
  , pr_漢辭海 = h
  -- , pr_辭源韵 = i
  , pr_略韵 = li
  , pr_字音補注 = n
  } = T.intercalate "。" . (<> [""]) $ filter (not . T.null) cs
  where
    fqs :: [Text]
    fqs = pronunciation反切集ToTex pc
    cs :: [Text]
    cs = fqs <> (maybeToList $ fmap pronunciation漢辭海ToTex h) <> (maybeToList li) <> (maybeToList . fmap stripPeriod $ n)
    stripPeriod x | T.takeEnd 1 x == "。" = T.dropEnd 1 x
                  | otherwise = x

indexTex :: Text -> [Text] -> Text
indexTex name inds = "\\index[" <> name <> "]{" <> T.intercalate "!" inds <> "}"

numberKey :: Int -> Text
numberKey i = T.pack $ printf "%08d" i

generateRadicalIndicesTex :: Entry -> [Text]
generateRadicalIndicesTex e = [indexMaybeRadical $ e_部畫 e]
  where
    indexRadical Shape部畫{ s_部 = r, s_畫 = s}= indexTex "radical"
      [ r <> "部"
      , numberKey s <> "@" <> (T.pack $ show s) <> "畫"
      , e_字 e
      ]
    indexMaybeRadical Nothing = indexTex "radical" [ "不明", e_字 e ]
    indexMaybeRadical (Just r) = indexRadical r

generateSikrokIndicesTex :: Entry -> [Text]
generateSikrokIndicesTex e = concat
  [ [ indexSikroks svParent ]
  , map (indexSikrokVariants svParent) $ s_選 sv
  , map (indexSikrokVariants svParent) $ s_簡 sv
  , map (indexSikrokVariants svParent) $ s_日 sv
  , map (indexSikrokVariants svParent) $ s_异 sv
  ]
  where
    indexSikroks ShapeVariant{s_字 = z, s_四角 = sks} = mconcat . map (indexSikrok z) $ NEL.toList sks
    indexSikrok z sk@(skM, skS) = indexTex "sikrok"
      [ skM <> "." <> skS <> "@" <> renderSikrokToTex sk
      , z
      ]
    sv = e_shapeVariants $ e
    svParent = s_親 sv
    indexSikrokVariants ShapeVariant{s_字 = zp, s_四角 = _} ShapeVariant{s_字 = z, s_四角 = sks} =
      mconcat . map (indexSikrokVariant zp z) $ NEL.toList sks
    indexSikrokVariant zp z sk@(skM, skS) = indexTex "sikrok"
      [ skM <> "." <> skS <> "@" <> renderSikrokToTex sk
      , z <> "（" <> zp <> "）"
      ]

unfoldIndexSet :: a -> a -> Int -> Set Int -> [a]
unfoldIndexSet vf vt l is = map f . take l $ [0..]
  where
    f i | i `S.member` is = vt
        | otherwise = vf

-- | Generate an index for phonetic parts
--
-- >>> generatePhoneticIndicesTex "傻" $ Parts {p_諧聲部 = "夋", p_諧聲位 = 80, p_parts = [Part {p_玉篇部首位 = (23, 0), p_部外 = "人"}], p_variants = S.fromList [1]}
-- ["\\index[phonetic]{00000080 00000023 00000000-01@\22795\\SoundParts{(80+\\SoundPartNI{23})\8242}!\20667}"]
generatePhoneticIndicesTex :: Text -> Parts -> [Text]
generatePhoneticIndicesTex z pps = [indexPhonetic]
  where
    indexPhonetic = indexTex "phonetic"
      [ keyStr <> "@" <> word
      , z
      ]
    keyPart Part{ p_玉篇部首位 = (i, p) } = [i, p]
    keyPhonetic (i, p) = [i, p]
    numKeys = (keyPhonetic $ p_諧聲位 pps) <> concatMap keyPart (p_parts pps)
    numKeyStr = T.intercalate " " . map numberKey $ numKeys
    keyStr = numKeyStr <> "-" <> mconcat (unfoldIndexSet "0" "1" ((+ 1) . length $ p_parts pps) $ p_variants pps)
    word = p_諧聲部 pps <> soundPartsToTex False pps

generateZyevioIndicesTex :: Entry -> [Text]
generateZyevioIndicesTex e = map indexZyevio $ e_音義 e
  where
    indexZyevio Entry音義{e_隋音 = p} = indexTex "zyevio"
      [ p
      , z
      ]
    z = s_字 svParent
    svParent = s_親 sv
    sv = e_shapeVariants $ e

generateIndicesTex :: Entry -> [Text]
generateIndicesTex e = concat
  [ generateRadicalIndicesTex e
  , generateSikrokIndicesTex e
  , generatePhoneticIndicesTex (s_字 . s_親 . e_shapeVariants $ e) (e_parts e)
  , generateZyevioIndicesTex e
  ]

entryToQrContent :: Entry -> Text
entryToQrContent e = mconcat . L.nub $ mconcat contents
  where
    sv = e_shapeVariants $ e
    contents =
             [ [e_字 e]
             , map s_字 $ s_日 sv
             , map s_字 $ s_選 sv
             , map s_字 $ s_异 sv
             , map s_字 $ s_簡 sv
             ]

entryToTex :: Entry -> Text
entryToTex e = mconcat
    [ "\\noindent"
    , "\\Position{"
    , T.intercalate "; " . map (\p -> T.pack $ printf "%d(%s)" (pos_row p) (pos_ver p)) . S.toList $ e_position e
    , "}"
    , "\n\n"
    , "\\begin{Entry}{", e_字 e, "}{", qrText, "}%\n"
    , "  "
    , T.intercalate "%\n  " $ generateIndicesTex e
    , "%\n"
    , "  "
    , soundPartsToTex True $ e_parts e
    , fromMaybe "" . fmap shape部畫ToTex $ e_部畫 e
    , "\n"
    , "  \\\\\n"
    , "  ", shapeVariantsToTex $ e_shapeVariants e, "\n"
    , "  \\begin{Sound}\n"
    , "    "
    , T.intercalate "\n    " tex音Items
    , "\n"
    , "  \\end{Sound}\n"
    , if null tex義Items
      then ""
      else "\\begin{Sense}\n    " <> T.intercalate "\n    " tex義Items <> "\n\\end{Sense}"
    , "\\end{Entry}\n"
    ]
  where
    tex音Items = map (\sp -> "\\SoundItem{" <> e_隋音 sp <> "}" <> (pronunciationToTex $ e_pronunciation sp)) $ e_音義 e
    tex義Items = catMaybes . map render義Item $ e_音義 e
    render義Item sp = do
      s <- e_義 sp
      return $ "\\SenseItem{" <> e_隋音 sp <> "}" <> escapeTex s
    qrText = qrImageToTex . encodeToQr $ entryToQrContent e

escapeTex :: Text -> Text
escapeTex s = T.concatMap f s
  where
    f '&' = "\\&"
    f '{' = "\\{"
    f '}' = "\\}"
    f '\\' = "\\textbackslash{}"
    f c = T.singleton c

unfoldTreeToTex :: (Ord a) => Bool -> PathTree a Text -> Text
unfoldTreeToTex isRoot t = fromMaybe "" (PT.root t) <> brackettedChildrenStr
  where
    children = M.toList $ PT.children t
    childrenStr = mconcat (map (unfoldTreeToTex False . snd) $ children)
    brackettedChildrenStr = if null children || isRoot
      then childrenStr
      else "（" <> childrenStr <> "）"

unfoldTreeToTex' :: (Ord a) => Bool -> PathTree a Text -> Text
unfoldTreeToTex' isRoot t =
    if null children || isRoot
      then content
      else "（" <> content <> "）"
  where
    children = M.toList $ PT.children t
    childrenStr = mconcat (map (unfoldTreeToTex' False . snd) $ children)
    content = fromMaybe "" (PT.root t) <> childrenStr

entriesToHeadingsTex :: (Ord a) => PathTree a [Entry] -> Text
entriesToHeadingsTex pt = unfoldTreeToTex' True $ fmap renderEntries pt
  where
    renderEntries es = T.concat $ map renderEntry es
    renderEntry e = "\\refEntry{" <> e_字 e <> "}"

sectionToTex :: Section -> Text
sectionToTex s = mconcat
    [ "\\PartHeader{"
    , soundPartNumberToTex $ sec_諧符位 s
    , "}{"
    , sec_諧符部 s
    , "聲"
    , "}{"
    -- reading
    , "}{"
    -- comments
    , "}{"
    , entriesToHeadingsTex $ sec_entries s
    , "}"
    , T.intercalate "\n" . map entryToTex . concatMap snd . PT.toList $ sec_entries s
    , "\n\n"
    ]

-- QR
qrImageToTex :: QR.QRImage -> Text
qrImageToTex img = matStr
  where
    mat = QR.toMatrix 'k' 'w' img
    matStr = "{" <> T.intercalate "}{" mat <> "}"

encodeToQr :: Text -> QR.QRImage
encodeToQr text = case QR.encodeText qropt QR.Utf8WithECI text of
               Just img -> img
               Nothing -> error $ printf "Failed to generate QR code for \"%s\"." text
  where
    qropt = QR.defaultQRCodeOptions QR.H
