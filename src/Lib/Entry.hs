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
import Data.List.NonEmpty (NonEmpty(..))
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
import Lib.PhoneticRadical (PhoneticRadical(..))

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

data EntrySortKey = EntrySortKey
  { es_phoneticPartNumber :: !PhoneticPartNumber
  , es_partPath :: ![Part]
  -- , es_reading :: !Text
  , es_position :: !(Set Position)
  }
  deriving (Read, Show, Eq, Ord, Generic)

data Section = Section
  { sec_諧符位 :: !PhoneticPartNumber
  , sec_諧符部 :: !Text
  , sec_phoneticRadical :: !(Maybe PhoneticRadical)
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

entrySortKey :: Entry -> EntrySortKey
entrySortKey e = EntrySortKey
  { es_phoneticPartNumber = p_諧聲位 $ e_parts e
  , es_partPath = p_parts $ e_parts e
  , es_position = e_position e
  }

sectionsFromRows :: (Foldable f) => [PhoneticRadical] -> f Row -> ([String], [Section])
sectionsFromRows prs rs = first concat . unzip . map renderSection $ MS.toList sectionMap
  where
    phoneticRadicalMap = M.fromList $ map (\pr -> (pr_index pr, pr)) prs
    sectionMap = groupBy (\r -> (p_諧聲位 $ r_parts r, p_諧聲部 $ r_parts r)) $ Foldable.toList rs
    renderSection :: ((PhoneticPartNumber, Text), NEV.NonEmptyVector Row) -> ([String], Section)
    renderSection (k, v) = (errors, section)
      where
        (errors, f_entries) = entriesFromRows $ NEV.toList v
        section = Section
                  { sec_諧符位 = fst k
                  , sec_諧符部 = snd k
                  , sec_phoneticRadical = M.lookup (fst k) phoneticRadicalMap
                  , sec_entries = fmap (L.sortOn entrySortKey) $ foldr addEntryToTree PT.empty f_entries
                  }

entriesFromRows :: (Functor f, Foldable f) => f Row -> ([String], [Entry])
entriesFromRows rs = (errors, map sort音義InEntry groupedEntries)
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

sort音義InEntry :: Entry -> Entry
sort音義InEntry e = e { e_音義 = sort音義 $ e_音義 e }

sort音義 :: [Entry音義] -> [Entry音義]
sort音義 = L.sortOn (f . e_pronunciation)
  where
    f Pronunciation{pr_四聲=Nothing, pr_韵=i, pr_小韵=s } = (1, Nothing, i, s)
    f Pronunciation{pr_四聲=t, pr_韵=i, pr_小韵=s } = (0, t, i, s)

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
-- renderSikroksToTex sks = T.intercalate "、" $ map renderSikrokToTex $ NEL.toList sks
renderSikroksToTex sks = renderSikrokToTex $ NEL.head sks

shapeVariantsToTex :: ShapeVariants -> Text
shapeVariantsToTex s = mconcat
    [ renderSikroksToTex . s_四角 . s_親 $ s
    , "\\quad "
    , if T.null variantDesc then "" else variantDesc <> "。"
    ]
  where
    variantDesc = T.intercalate "，" $ variantDescs <> maybeToList variantDescsPost
    variantDescs = Maybe.catMaybes $ map (\(l, c, p) -> variantToTex l c p) $
      [ ("選用", s_選 s, "") ]
      <> variantSimpJapn (s_簡 s) (s_日 s) (s_韓 s)
    variantDescsPost = variantToTex "或作" (s_异 s) ""
    variantSimpJapn [simp] [japn] [kor] | simp == japn && japn == kor = [("簡・日・韓作", [simp], "\\textHans")]
    variantSimpJapn [simp] [japn] kor | simp == japn =
                                          [("簡・日作", [simp], "\\textHans")
                                          , ("韓作", kor, "\\textKor")
                                          ]
    variantSimpJapn simp [japn] [kor] | japn == kor =
                                          [("簡作", simp, "\\textHans")
                                          , ("日・韓作", [japn], "\\textJapn")
                                          ]
    variantSimpJapn [simp] japn [kor] | simp == kor =
                                          [("簡・韓作", [simp], "\\textHans")
                                          , ("日作", japn, "\\textJapn")
                                          ]
    variantSimpJapn simp japn kor =
      [ ("簡作", simp, "\\textHans")
      , ("日作", japn, "\\textJapn")
      , ("韓作", kor, "\\textKor")
      ]

shape部畫ToTex :: Shape部畫 -> Text
shape部畫ToTex s = mconcat
  [ s_部 s
  , "部"
  , T.pack . show . s_畫 $ s
  , "畫"
  ]

soundPartNumberToTex :: PhoneticPartNumber -> Text
soundPartNumberToTex (PhoneticPartNumber (i, 0)) = T.pack $ printf "%d" i
soundPartNumberToTex (PhoneticPartNumber (i, p)) = T.pack $ printf "%d.%03d" i p

partNumberToTex :: (Int, Int) -> Text
partNumberToTex (i, p) = T.pack $ printf "%d%s" i (primes p)
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
    then T.pack $ printf "\\SoundPart{%s}{%s}" (partNumberToTex k) e
    else T.pack $ printf "\\SoundPartNI{%s}" (partNumberToTex k)

-- | Generate a text content for phonetic parts
--
-- >>> putStrLn . T.unpack $ soundPartsToTex True $ Parts {p_諧聲部 = "夋", p_諧聲位 = PhoneticPartNumber (80, 0), p_parts = [Part {p_玉篇部首位 = (23, 0), p_部外 = "人"}], p_variants = S.fromList [1]}
-- \SoundParts{(80 + \SoundPart{23}{人})′}
-- >>> putStrLn . T.unpack $ soundPartsToTex False $ Parts {p_諧聲部 = "夋", p_諧聲位 = PhoneticPartNumber (80, 0), p_parts = [Part {p_玉篇部首位 = (23, 0), p_部外 = "人"}], p_variants = S.fromList [1]}
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

-- | Generate a text content for an index prefix
--
-- >>> soundPartsToLabelPrefix $ Parts {p_諧聲部 = "夋", p_諧聲位 = PhoneticPartNumber (80, 0), p_parts = [Part {p_玉篇部首位 = (23, 0), p_部外 = "人"}], p_variants = S.fromList [1]}
-- "80+23'"
--
soundPartsToLabelPrefix :: Parts -> Text
soundPartsToLabelPrefix sps = content
  where
    content = T.intercalate "+" $ map f texSoundPartsEither
    f (Left x) = x
    f (Right x) = x <> "'"
    texSoundPartsEither = rightIndexes (p_variants sps) texSoundParts
    texSoundParts = (phoneticPartNumberToIndex $ p_諧聲位 sps) : (map soundPartToIndex $ p_parts sps)
    phoneticPartNumberToIndex (PhoneticPartNumber (n, 0)) = T.pack $ printf "%d" n
    phoneticPartNumberToIndex (PhoneticPartNumber (n, p)) = T.pack $ printf "%d.%d" n p
    soundPartToIndex Part{ p_玉篇部首位 = (n, 0), p_部外 = _} = T.pack $ printf "%d" n
    soundPartToIndex Part{ p_玉篇部首位 = (n, p), p_部外 = _} = T.pack $ printf "%d.%d" n p

entryToLabel :: Entry -> Text
entryToLabel e = soundPartsToLabelPrefix (e_parts e) <> "-" <> (e_字 e)

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
-- >>> pronunciation反切ToTex True (Pronunciation反切本 {pr_反切本_books = NEL.singleton "\21453\20999", pr_反切本_fanqies = Pronunciation反切 {pr_反切 = Nothing, pr_反切_comment = Just "\12394\12375", pr_反切_pre_info=Nothing} :| [] })
-- "\\Book{\21453\20999}\12394\12375"
--
-- >>> pronunciation反切ToTex False (Pronunciation反切本 {pr_反切本_books = NEL.singleton "\21453\20999", pr_反切本_fanqies = Pronunciation反切 {pr_反切 = Nothing, pr_反切_comment = Just "\12394\12375", pr_反切_pre_info=Nothing} :| [] })
-- "\12394\12375"
--
-- >>> pronunciation反切ToTex False (Pronunciation反切本 {pr_反切本_books = NEL.singleton "\21453\20999", pr_反切本_fanqies = Pronunciation反切 {pr_反切 = Nothing, pr_反切_comment = Just "無反語",  pr_反切_pre_info=Just "歌韵"} :| [] })
-- "\27468\38901\28961\21453\35486"
--
-- >>> pronunciation反切ToTex False (Pronunciation反切本 {pr_反切本_books = NEL.singleton "王韵", pr_反切本_fanqies = Pronunciation反切 {pr_反切 = Just ("子句","反"), pr_反切_comment = Nothing, pr_反切_pre_info=Nothing} :| [Pronunciation反切 {pr_反切 = Just ("即具","反"), pr_反切_comment = Nothing, pr_反切_pre_info=Nothing}] })
-- "\23376\21477\21453\12289\21363\20855\21453"

pronunciation反切ToTex :: Bool -> Pronunciation反切本 -> Text
pronunciation反切ToTex showBook fq =
    addBooks . T.intercalate "、" . NEL.toList . fmap pronunciation反切ContentToTex $ pr_反切本_fanqies fq
  where
    addBooks = if showBook
      then (booksToTex (pr_反切本_books fq) <>)
      else id

pronunciation反切ContentToTex :: Pronunciation反切 -> Text
pronunciation反切ContentToTex Pronunciation反切
  { pr_反切 = Nothing
  , pr_反切_pre_info = Nothing
  , pr_反切_comment = Nothing
  } = error "Empty 反切 entry"
pronunciation反切ContentToTex Pronunciation反切
  { pr_反切 = Nothing
  , pr_反切_comment = Just com
  , pr_反切_pre_info = pre
  } = fromMaybe "" pre <> com
pronunciation反切ContentToTex Pronunciation反切
  { pr_反切 = Just (pc, suf)
  , pr_反切_comment = Nothing
  , pr_反切_pre_info = pre
  } = fromMaybe "" pre <> pc <> suf

pronunciation反切ContentToTex Pronunciation反切
  { pr_反切 = Just (pc, suf)
  , pr_反切_comment = Just com
  , pr_反切_pre_info = pre
  } = fromMaybe "" pre <> pc <> suf <> "（" <> com <> "）"

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
    f :: Text -> [Pronunciation反切本] -> [Text]
    f _ [] = []
    f b [p] | pr_反切本_books p == NEL.singleton b = pure $ pronunciation反切ToTex False p
            | otherwise = pure $ pronunciation反切ToTex True p
    f _ ps = map (pronunciation反切ToTex True) ps

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
    , "\n\n"
    , "\\begin{Entry}"
    , "{", e_字 e, "}"
    , "{", qrText, "}{", entryToLabel e, "}"
    , "{"
    , T.intercalate "; " . map (\p -> T.pack $ printf "%d(%s)" (pos_row p) (pos_ver p)) . S.toList $ e_position e
    , "}"
    , "%\n"
    , "  "
    , soundPartsToTex True $ e_parts e
    , "。"
    , fromMaybe "" . fmap ((<> "。") . shape部畫ToTex) $ e_部畫 e
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
    f '~' = "\\textasciitilde{}"
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
    renderEntry e = "\\refEntry{" <> e_字 e <> "}{" <> entryToLabel e <> "}"

sectionToTex :: Section -> Text
sectionToTex s = mconcat
    [ "\\PartHeader{"
    , soundPartNumberToTex $ sec_諧符位 s
    , "}{"
    , sec_諧符部 s
    , "聲"
    , "}{"
    , T.intercalate ", " pronunciations
    , "}{"
    , fromMaybe "" comment
    , "}{"
    , entriesToHeadingsTex $ sec_entries s
    , "}"
    , T.intercalate "\n" . map entryToTex . concatMap snd . PT.toList $ sec_entries s
    , "\n\n"
    ]
  where
    phoneticRadical = sec_phoneticRadical s
    comment = pr_comment <$> phoneticRadical
    pronunciations = maybeToList =<< mapM pr_pronunciation phoneticRadical

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
