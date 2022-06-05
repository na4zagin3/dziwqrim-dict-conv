{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Lib.Row where

import Control.Arrow (left)
import Control.Monad (join, liftM)
import Data.Attoparsec.Text (parseOnly)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NEL
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read as TR
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Lib.Kanji (explodeKanji)
import Lib.Kanji (p_fanqieField)
import Lib.Rhymes (list_平水)
import Text.Printf (printf)



data ShapeVariant = ShapeVariant
  { s_字 :: !Text
  , s_四角 :: !(NonEmpty (Text, Text))
  , s_comment :: !Text
  }
  deriving (Read, Show, Eq, Ord, Generic)

data ShapeVariants = ShapeVariants
  { s_親 :: !ShapeVariant
  , s_選 :: ![ShapeVariant]
  , s_簡 :: ![ShapeVariant]
  , s_日 :: ![ShapeVariant]
  , s_韓 :: ![ShapeVariant]
  , s_异 :: ![ShapeVariant]
  }
  deriving (Read, Show, Eq, Ord, Generic)

data Shape部畫 = Shape部畫
  { s_部 :: !Text
  , s_畫 :: !Int
  }
  deriving (Read, Show, Eq, Ord, Generic)

data Part = Part
  { p_玉篇部首位 :: !(Int, Int)
  , p_部外 :: !Text
  }
  deriving (Read, Show, Eq, Ord, Generic)

data Parts = Parts
  { p_諧聲部 :: !Text
  , p_諧聲位 :: !(Int, Int)
  , p_parts :: ![Part]
  , p_variants :: Set Int
  }
  deriving (Read, Show, Eq, Ord, Generic)

data Pronunciation反切 = Pronunciation反切
  { pr_反切 :: !(Maybe (Text, Text))
  , pr_反切_comment :: !(Maybe Text)
  , pr_反切_books :: !(NonEmpty Text)
  }
  deriving (Read, Show, Eq, Ord, Generic)

data Pronunciation反切集 = Pronunciation反切集
  { pr_切韵反切 :: ![Pronunciation反切]
  , pr_王韵反切 :: ![Pronunciation反切]
  , pr_廣韵反切 :: ![Pronunciation反切]
  , pr_集韵反切 :: ![Pronunciation反切]
  }
  deriving (Read, Show, Eq, Ord, Generic)

-- TODO make stricter
data Pronunciation漢辭海 = Pronunciation漢辭海
  { pr_漢辭海_聲 :: !Text
  , pr_漢辭海_韵 :: !Text
  , pr_漢辭海_調 :: !Text
  }
  deriving (Read, Show, Eq, Ord, Generic)

data Pronunciation辭源韵 = Pronunciation辭源韵
  { pr_辭源韵_調 :: !Text
  , pr_辭源韵_韵目 :: !Text
  , pr_辭源韵_韵字 :: !Text
  , pr_辭源韵_用例 :: !(Maybe Text)
  }
  deriving (Read, Show, Eq, Ord, Generic)

data Pronunciation = Pronunciation
  { -- pr_韵部 :: !Text
  -- ,
    pr_反切集 :: !(Pronunciation反切集)
  , pr_漢辭海 :: !(Maybe Pronunciation漢辭海)
  -- , pr_辭源韵 :: !(Maybe Pronunciation辭源韵)
  , pr_略韵 :: !(Maybe Text)
  , pr_字音補注 :: !(Maybe Text)
  }
  deriving (Read, Show, Eq, Ord, Generic)

data Position = Position
  { pos_row :: !Int
  , pos_ver :: !Text
  }
  deriving (Read, Show, Eq, Ord, Generic)

data Row = Row
  { r_position :: !Position
  , r_字 :: !Text
  , r_shapeVariants :: !ShapeVariants
  , r_部畫 :: !(Maybe Shape部畫)
  , r_parts :: !Parts
  , r_隋音 :: !Text
  , r_義 :: !(Maybe Text)
  , r_pronunciation :: !Pronunciation
  }
  deriving (Read, Show, Eq, Ord, Generic)

p_r_字 :: Text -> Either String Text
p_r_字 "" = Left "Missing 字"
p_r_字 t = Right t

-- | Parse a sikrok field
--
-- Examples
--
-- >>> p_r_四角 "50006"
-- Right ("5000","6")
--
-- >>> p_r_四角 "50006/50006"
-- Left "trailing garbage: /50006"
--
-- >>> p_r_四角 "123456"
-- Left "123456 is too large"

p_r_四角 :: Text -> Either String (Text, Text)
p_r_四角 t = do
  (n, r) <- TR.decimal t :: Either String (Int, Text)
  () <- if T.null r
    then Right ()
    else Left $ printf "trailing garbage: %s" r
  let (dm, ds) = divMod n 10
  if dm > 9999
    then Left $ printf "%s is too large" t
    else Right (T.pack $ printf "%04d" dm, T.pack $ printf "%01d" ds)

-- | Parse shape variant information
--
-- Examples
--
-- >>> p_r_shapeVariant "" ""
-- Right []
-- >>> p_r_shapeVariant "中" "50006"
-- Right [ShapeVariant {s_字 = "\20013", s_四角 = ("5000","6") :| [], s_comment = ""}]
-- >>> p_r_shapeVariant "吶㕯" "64027|40227"
-- Right [ShapeVariant {s_字 = "\21558", s_四角 = ("6402","7") :| [], s_comment = ""},ShapeVariant {s_字 = "\13679", s_四角 = ("4022","7") :| [], s_comment = ""}]
-- >>> p_r_shapeVariant "大" "40030/40800"
-- Right [ShapeVariant {s_字 = "\22823", s_四角 = ("4003","0") :| [("4080","0")], s_comment = ""}]
-- >>> p_r_shapeVariant "皐臯" "26409|26409"
-- Right [ShapeVariant {s_字 = "\30352", s_四角 = ("2640","9") :| [], s_comment = ""},ShapeVariant {s_字 = "\33263", s_四角 = ("2640","9") :| [], s_comment = ""}]
-- >>> p_r_shapeVariant "⿰叠毛" "12115/12114/72115/72114"
-- Right [ShapeVariant {s_字 = "\12272\21472\27611", s_四角 = ("1211","5") :| [("1211","4"),("7211","5"),("7211","4")], s_comment = ""}]
-- >>> p_r_shapeVariant "一二三" "10000|10100"
-- Left "Inconsistent length of \23383 ([\"\\19968\","\\\20108\",\"\\19977\"]) and \22235\35282 ([[(\"1000\",\"0\")],[(\"1010\",\"0\")]])"
-- >>> p_r_shapeVariant "一二三" "10000/10100"
-- Left "Inconsistent length of \23383 ([\"\\19968\","\\\20108\",\"\\19977\"]) and \22235\35282 ([[(\"1000\",\"0\"),(\"1010\",\"0\")]])"


p_r_shapeVariant :: Text -> Text -> Either String [ShapeVariant]
p_r_shapeVariant csStr ssRaw = do
  let p_sk "" = Right []
      p_sk str = mapM p_r_四角 $ T.split (`T.elem` "/") str
  sss <- mapM p_sk $ T.split (`T.elem` "|") ssRaw
  cs <- left (printf "p_r_shapeVariant: %s") $ explodeKanji csStr
  case (cs, sss) of
    ([], []) -> Right $ []
    ([], [[]]) -> Right $ []
    (_, []) -> Left "missing 四角"
    ([], _) -> Left "missing 字 for 四角"
    (_, _) ->
      if length cs == length sss
        then mapM f $ zip cs sss
        else Left $ printf "Inconsistent length of 字 (%s) and 四角 (%s)" (show cs) (show sss)
        where
          f (_, []) = Left "missing 四角"
          f (c, (s:ss)) = Right $ ShapeVariant
            { s_字 = c
            , s_四角 = s NEL.:| ss
            , s_comment = ""
            }

p_r_部畫 :: Text -> Text -> Either String (Maybe Shape部畫)
p_r_部畫 "n" "n" = Right Nothing
p_r_部畫 p s = do
  (sn, r) <- TR.decimal s :: Either String (Int, Text)
  () <- if T.null r
    then Right ()
    else Left $ printf "trailing garbage: %s" r
  return . Just $ Shape部畫
    { s_部 = p
    , s_畫 = sn
    }

p_r_partNumber :: Text -> Either String (Int, Int)
p_r_partNumber pIRaw = do
  (n, r) <- TR.decimal pIRaw :: Either String (Int, Text)
  case r of
    "" -> Right (n, 0)
    ".999" -> Right (n, 1)
    _ -> Left $ printf "trailing garbage: %s" r

p_r_part :: Text -> Text -> Either String Part
p_r_part pIRaw pE = do
  pI <- left (printf "符外位: (%s): %s" pE) $ p_r_partNumber pIRaw
  Right $ Part
    { p_玉篇部首位 = pI
    , p_部外 = pE
    }

-- | Parse parts info
--
-- Examples
--
-- >>> p_r_parts "26" "充" [] "0" "0"
-- Right (Parts {p_諧聲部 = "\20805", p_諧聲位 = (26,0), p_parts = [], p_variants = fromList []})
--
-- >>> p_r_parts "26" "充" [("0", "0")] "0" "0"
-- Right (Parts {p_諧聲部 = "\20805", p_諧聲位 = (26,0), p_parts = [], p_variants = fromList []})
--
-- >>> p_r_parts "26" "充" [("0", "0")] "⌥" "0"
-- Right (Parts {p_諧聲部 = "\20805", p_諧聲位 = (26,0), p_parts = [], p_variants = fromList [0]})
--
-- >>> p_r_parts "26" "充" [("269", "金")] "0" "0"
-- Right (Parts {p_諧聲部 = "\20805", p_諧聲位 = (26,0), p_parts = [Part {p_玉篇部首位 = (269,0), p_部外 = "\37329"}], p_variants = fromList []})
--
-- >>> p_r_parts "32" "竹筑" [("73.999", "巩")] "0" "0"
-- Right (Parts {p_諧聲部 = "\31481", p_諧聲位 = (32,0), p_parts = [Part {p_玉篇部首位 = (73,1), p_部外 = "\24041"}], p_variants = fromList []})
--
-- >>> p_r_parts "1" "⿱圥囧" [("91", "言")] "0" "0"
-- Right (Parts {p_諧聲部 = "\12273\22309\22247", p_諧聲位 = (1,0), p_parts = [Part {p_玉篇部首位 = (91,0), p_部外 = "\35328"}], p_variants = fromList []})
-- >>> p_r_parts "1" "竹筑" [("1", "⿱圥囧")] "0" "0"
-- Right (Parts {p_諧聲部 = "\31481", p_諧聲位 = (1,0), p_parts = [Part {p_玉篇部首位 = (1,0), p_部外 = "\12273\22309\22247"}], p_variants = fromList []})

p_r_parts :: Text -> Text -> [(Text, Text)] -> Text -> Text -> Either String Parts
p_r_parts ki kpsRaw ps vk vpsRaw = do
  let parseDecimal s = do
        (res, rem) <- TR.decimal s
        if T.null rem
          then Right res
          else Left $ printf "trailing garbage: %s" rem
  kin <- left (printf "諧聲位: %s") $ p_r_partNumber ki

  vps' <- left (printf "聲外分: %s") . liftM S.fromList . mapM parseDecimal . filter (not . T.null) $ T.split (`T.elem` ",") vpsRaw
  vps <- case vk of
        x | x `elem` ["⌥", "分", "略", "异"] -> Right $ S.singleton 0 `S.union` vps'
          | x `elem` ["", "0"] -> Right $ S.delete 0 vps'
        _ -> Left $ printf "Unknown 聲分 (%s)" vk
  f_parts <- mapM (uncurry p_r_part) $ filter (not . (`elem` [("0", "0"), ("", "")])) ps
  kps <- explodeKanji kpsRaw
  kp <- case kps of
    [] -> Left "Empty 諧聲部"
    kp:_ -> Right kp
  return $ Parts
    { p_諧聲部 = kp
    , p_諧聲位 = kin
    , p_variants = vps
    , p_parts = f_parts
    }

p_r_隋音 :: Text -> Either String Text
p_r_隋音 "" = Left "Missing 隋音"
p_r_隋音 t = Right t

p_r_反切本_item :: Text -> Either String Text
p_r_反切本_item pCBookRaw | (T.take 1 pCBookRaw == "《") && (T.takeEnd 1 pCBookRaw == "》") = return . T.drop 1 . T.dropEnd 1 $ pCBookRaw
p_r_反切本_item pCBookRaw | otherwise = Left $ printf "Malformatted 反切本: %s " pCBookRaw

p_r_反切本 :: Text -> Either String (NonEmpty Text)
p_r_反切本 pCBooksRaw = do
  books <- mapM p_r_反切本_item $ T.split (== '、') pCBooksRaw
  case books of
    [] -> Left $ "Missing 反切本"
    (x:xs) -> return $ x NEL.:| xs

keyword_反切 :: [Text]
keyword_反切 = ["未收", "脱字", "無本"]

-- | Parse a 反切
--
-- Examples
-- >>> p_r_反切 "反" (NEL.singleton "切韻") "なし"
-- Right (Pronunciation反切 {pr_反切 = Nothing, pr_反切_comment = Nothing, pr_反切_books = "\20999\38907" :| []})
--
-- >>> p_r_反切 "反" (NEL.singleton "切韻") "（）"
-- Right (Pronunciation反切 {pr_反切 = Nothing, pr_反切_comment = Just "", pr_反切_books = "\20999\38907" :| []})
--
-- >>> p_r_反切 "反" (NEL.singleton "切韻") "（コメント）"
-- Right (Pronunciation反切 {pr_反切 = Nothing, pr_反切_comment = Just "\12467\12513\12531\12488", pr_反切_books = "\20999\38907" :| []})
--
-- >>> p_r_反切 "反" (NEL.singleton "切韻") "而鋭反（コメント）"
-- Right (Pronunciation反切 {pr_反切 = Just ("\32780\37613","\21453"), pr_反切_comment = Just "\12467\12513\12531\12488", pr_反切_books = "\20999\38907" :| []})
--
-- >>> p_r_反切 "反" (NEL.singleton "切韻") "〓〓反（《王三》未收）"
-- Right (Pronunciation反切 {pr_反切 = Just ("\12307\12307","\21453"), pr_反切_comment = Just "\12298\29579\19977\12299\26410\25910", pr_反切_books = "\20999\38907" :| []})
--
-- # >>> p_r_反切 "反" (NEL.singleton "王韻") "《王一》户恢反，《王三》未收"
-- # >>> p_r_反切 "反" (NEL.singleton "王韻") "《王一》户孟反，《王三》胡孟反"
-- # >>> p_r_反切 "反" (NEL.singleton "王韻") "《王三》脱字（脱反語），《王一》無本"

p_r_反切 :: Text -> NonEmpty Text -> Text -> Either String (Pronunciation反切)
p_r_反切 field books pc = do
  rs <- p_r_反切s field books pc
  case rs of
    [] -> Right $ Pronunciation反切
        { pr_反切 = Nothing
        , pr_反切_comment = Nothing
        , pr_反切_books = books
        }
    [r] -> Right r
    _ -> Left "too many 反切s"

-- | Parse a 反切s
--
-- Examples
-- >>> p_r_反切s "反" (NEL.singleton "切韻") "なし"
-- Right []
--
-- >>> p_r_反切s "反" (NEL.singleton "切韻") "（）"
-- Right [Pronunciation反切 {pr_反切 = Nothing, pr_反切_comment = Just "", pr_反切_books = "\20999\38907" :| []}]
--
-- >>> p_r_反切s "反" (NEL.singleton "切韻") "（コメント）"
-- Right [Pronunciation反切 {pr_反切 = Nothing, pr_反切_comment = Just "\12467\12513\12531\12488", pr_反切_books = "\20999\38907" :| []}]
--
-- >>> p_r_反切s "反" (NEL.singleton "切韻") "而鋭反（コメント）"
-- Right [Pronunciation反切 {pr_反切 = Just ("\32780\37613","\21453"), pr_反切_comment = Just "\12467\12513\12531\12488", pr_反切_books = "\20999\38907" :| []}]
--
-- >>> p_r_反切s "反" (NEL.singleton "切韻") "〓〓反（《王三》未收）"
-- Right [Pronunciation反切 {pr_反切 = Just ("\12307\12307","\21453"), pr_反切_comment = Just "\12298\29579\19977\12299\26410\25910", pr_反切_books = "\20999\38907" :| []}]
--
-- >>> p_r_反切s "反" (NEL.singleton "王韻") "《王一》户恢反，《王三》未收"
-- Right [Pronunciation反切 {pr_反切 = Just ("\25143\24674","\21453"), pr_反切_comment = Nothing, pr_反切_books = "\29579\19968" :| []},Pronunciation反切 {pr_反切 = Nothing, pr_反切_comment = Just "\26410\25910", pr_反切_books = "\29579\19977" :| []}]
-- >>> p_r_反切s "反" (NEL.singleton "王韻") "《王一》户孟反，《王三》胡孟反"
-- Right [Pronunciation反切 {pr_反切 = Just ("\25143\23391","\21453"), pr_反切_comment = Nothing, pr_反切_books = "\29579\19968" :| []},Pronunciation反切 {pr_反切 = Just ("\32993\23391","\21453"), pr_反切_comment = Nothing, pr_反切_books = "\29579\19977" :| []}]
-- >>> p_r_反切s "反" (NEL.singleton "王韻") "《王三》脱字（脱反語），《王一》無本"
-- Right [Pronunciation反切 {pr_反切 = Nothing, pr_反切_comment = Just "\33073\23383\65288\33073\21453\35486\65289", pr_反切_books = "\29579\19977" :| []},Pronunciation反切 {pr_反切 = Nothing, pr_反切_comment = Just "\28961\26412", pr_反切_books = "\29579\19968" :| []}]
p_r_反切s :: Text -> NonEmpty Text -> Text -> Either String [Pronunciation反切]
p_r_反切s field books pc = do
  fq <- left (printf "反切: %s (%s): %s" field pc) $ parseOnly p_fanqieField pc
  let conv Nothing (Nothing, _, _) = Left $ printf "Cannot determine 反切本 for %s" field
      conv (Just books) (b, fq, c) = Right $ Pronunciation反切
        { pr_反切 = fq
        , pr_反切_comment = c
        , pr_反切_books = fromMaybe books . fmap NEL.singleton $ b
        }
      conv Nothing ((Just b), fq, c) = Right $ Pronunciation反切
        { pr_反切 = fq
        , pr_反切_comment = c
        , pr_反切_books = NEL.singleton b
        }
  case fq of
    [] -> Right []
    fq:fqs -> do
      pr1 <- conv (Just books) fq
      prs <- mapM (conv Nothing) fqs
      return $ [pr1] <> prs

textToMaybe :: Text -> Maybe Text
textToMaybe "" = Nothing
textToMaybe x = Just x

p_r_反切集 :: Text -> Text -> Text -> Text -> Text -> Either String Pronunciation反切集
p_r_反切集 pC pCBooksRaw pU pK pDz = do
  f_pr_切韵反切 <- case (pCBooksRaw, pC) of
    ("なし", "なし") -> Right []
    _ -> do
      pCBooks <- p_r_反切本 pCBooksRaw
      p_r_反切s "切韵" pCBooks pC
  f_pr_王韵反切 <- p_r_反切s "王韵" (NEL.singleton "王韵") pU
  f_pr_廣韵反切 <- p_r_反切s "廣韵" (NEL.singleton "廣韵") pK
  f_pr_集韵反切 <- p_r_反切s "集韵" (NEL.singleton "集韵") pDz
  Right $ Pronunciation反切集
    { pr_切韵反切 = f_pr_切韵反切
    , pr_王韵反切 = f_pr_王韵反切
    , pr_廣韵反切 = f_pr_廣韵反切
    , pr_集韵反切 = f_pr_集韵反切
    }

p_r_漢辭海 :: Text -> Text -> Text -> Either String (Maybe Pronunciation漢辭海)
p_r_漢辭海 "n" "n" "n" = Right Nothing
p_r_漢辭海 c v t = Right . Just $ Pronunciation漢辭海
  { pr_漢辭海_聲 = c
  , pr_漢辭海_韵 = v
  , pr_漢辭海_調 = t
  }

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither x Nothing = Left x
maybeToEither _ (Just x) = Right x

p_r_辭源韵 :: Text -> Either String (Maybe Pronunciation辭源韵)
p_r_辭源韵 "" = Right Nothing
p_r_辭源韵 i = do
  let (imRaw, com) = T.breakOn "：" i
  (f_調, f_韵目, f_韵字) <- maybeToEither ("Unknown 平水韻韻目: " <> T.unpack imRaw) $ M.lookup imRaw list_平水
  let parseCom "" = Just Nothing
      parseCom c = do
        c' <- T.stripPrefix "：“" c
        c'' <- T.stripSuffix "”" c'
        return $ Just c''
  com' <- maybeToEither ("Ill-formatted comment at 辭源韵: " <> T.unpack com) $ parseCom com
  Right . Just $ Pronunciation辭源韵
    { pr_辭源韵_調 = f_調
    , pr_辭源韵_韵目 = f_韵目
    , pr_辭源韵_韵字 = f_韵字
    , pr_辭源韵_用例 = com'
    }

p_r_略韵 :: Text -> Either String (Maybe Text)
p_r_略韵 "" = Right Nothing
p_r_略韵 t = Right $ Just t

p_r_字音補注 :: Text -> Either String (Maybe Text)
p_r_字音補注 "" = Right Nothing
p_r_字音補注 t = Right $ Just t

p_r_義 :: Text -> Either String (Maybe Text)
p_r_義 "" = Right Nothing
p_r_義 t = Right $ Just t

maybeToRight :: a -> Maybe b -> Either a b
maybeToRight _ (Just x) = Right x
maybeToRight y Nothing  = Left y

parseValidRow :: (HasCallStack) => Int -> Text -> Map Text Text -> Either String Row
parseValidRow row version m = do
  let lookupField f = maybeToRight (printf "missing field: %s" f) $ M.lookup (fromString f) m

  f_字 <- p_r_字 =<< lookupField "字"
  f_親_pl <- left ("親: " <>) . join $ p_r_shapeVariant <$> lookupField "字" <*> lookupField "四角"
  f_親 <- case f_親_pl of
            [] -> Left "Lacks 四角 for the head character"
            [f_親] -> Right f_親
            _ -> Left "Multiple 四角 for the head character"

  f_選 <- left ("選: " <>) . join $ p_r_shapeVariant <$> lookupField "選" <*> lookupField "四角選"
  f_簡 <- left ("簡: " <>) . join $ p_r_shapeVariant <$> lookupField "簡" <*> lookupField "四簡"
  f_日 <- left ("日: " <>) . join $ p_r_shapeVariant <$> lookupField "日" <*> lookupField "四日"
  f_韓 <- left ("韓: " <>) . join $ p_r_shapeVariant <$> lookupField "韓" <*> lookupField "四韓"
  f_异 <- left ("异: " <>) . join $ p_r_shapeVariant <$> lookupField "异" <*> lookupField "四异"
  let f_shapeVariants = ShapeVariants
        { s_親 = f_親
        , s_選 = f_選
        , s_簡 = f_簡
        , s_日 = f_日
        , s_韓 = f_韓
        , s_异 = f_异
        }

  f_部畫 <- left ("部畫: " <>) . join $ p_r_部畫 <$> lookupField "部" <*> lookupField "畫"

  f_隋音 <- join $ p_r_隋音 <$> lookupField "隋音"
  f_義 <- join $ p_r_義 <$> lookupField "義"

  f_parts <- do
    let partsFields =
          mapM (\(i, c) -> (,) <$> lookupField i <*> lookupField c)
            [ ("符外位1", "聲外1")
            , ("符外位2", "聲外2")
            , ("符外位3", "聲外3")
            ]
    join $ p_r_parts
      <$> lookupField "諧聲位"
      <*> lookupField "諧聲"
      <*> partsFields
      <*> lookupField "分"
      <*> lookupField "聲外分"


  f_r_漢辭海 <- join $ p_r_漢辭海 <$> lookupField "漢辭海聲" <*> lookupField "漢辭海韵" <*> lookupField "漢辭海調"
  -- f_r_辭源韵 <- join $ p_r_辭源韵 <$> lookupField "辭源韵"
  f_r_略韵 <- join $ p_r_略韵 <$> lookupField "略韵"
  f_r_字音補注 <- join $ p_r_字音補注 <$> lookupField "字音補注"

  f_r_反切集 <- join $ p_r_反切集 <$> lookupField "切韵反切" <*> lookupField "切韵本" <*> lookupField "王韵反切" <*> lookupField "廣韵反切" <*> lookupField "集韵反切"
  let f_pronunciation = Pronunciation
        { pr_漢辭海 = f_r_漢辭海
        , pr_反切集 = f_r_反切集
        , pr_略韵 = f_r_略韵
        , pr_字音補注 = f_r_字音補注
        }
  -- { pr_韵部 :: !Text
  return $ Row
    { r_position = Position { pos_row = row , pos_ver = version }
    , r_字 = f_字
    , r_shapeVariants = f_shapeVariants
    , r_部畫 = f_部畫
    , r_隋音 = f_隋音
    , r_義 = f_義
    , r_parts = f_parts
    , r_pronunciation = f_pronunciation
    }

parseRow :: (HasCallStack) => Int -> Map Text Text -> Either String (Maybe Row)
parseRow row m = do
  let lookupField f = maybeToRight f $ M.lookup (fromString f) m
  version <- lookupField "状態"
  f_字 <- p_r_字 =<< lookupField "字"
  if version `elem` ["5"]
    then left (printf "%s: %s" f_字) . fmap Just $ parseValidRow row version m
    else return Nothing
