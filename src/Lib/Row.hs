{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Lib.Row where

import Control.Arrow (left)
import Control.Monad (join)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NEL
import Data.Map (Map)
import Data.Map qualified as M
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read as TR
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
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
  { p_諧符部 :: !Text
  , p_諧符位 :: !Int
  , p_variant :: !Bool
  , p_parts :: ![Part]
  }
  deriving (Read, Show, Eq, Ord, Generic)

data Pronunciation反切 = Pronunciation反切
  { pr_反切 :: !(Maybe Text)
  , pr_反切_suffix :: !Text
  , pr_反切_comment :: !(Maybe Text)
  , pr_反切_books :: !(NonEmpty Text)
  }
  deriving (Read, Show, Eq, Ord, Generic)

data Pronunciation反切集 = Pronunciation反切集
  { pr_切韵反切 :: !(Maybe Pronunciation反切)
  , pr_王韵反切 :: !(Maybe Pronunciation反切)
  , pr_廣韵反切 :: !(Maybe Pronunciation反切)
  , pr_集韵反切 :: !(Maybe Pronunciation反切)
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
  , pr_辭源韵 :: !(Maybe Pronunciation辭源韵)
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
-- >>> p_r_shapeVariant "吶㕯" "64027/40227"
-- Right [ShapeVariant {s_字 = "\21558", s_四角 = ("6402","7") :| [], s_comment = ""},ShapeVariant {s_字 = "\13679", s_四角 = ("4022","7") :| [], s_comment = ""}]
-- >>> p_r_shapeVariant "大" "40030|40800"
-- Right [ShapeVariant {s_字 = "\22823", s_四角 = ("4003","0") :| [("4080","0")], s_comment = ""}]
-- >>> p_r_shapeVariant "皐臯" "26409/26409"
-- Right [ShapeVariant {s_字 = "\30352", s_四角 = ("2640","9") :| [], s_comment = ""},ShapeVariant {s_字 = "\33263", s_四角 = ("2640","9") :| [], s_comment = ""}]
-- >>> p_r_shapeVariant "一二三" "10000/10100"
-- Left "Inconsistent length of \23383 (\19968\20108\19977) and \22235\35282 ([[(\"1000\",\"0\")],[(\"1010\",\"0\")]])"
-- >>> p_r_shapeVariant "一二三" "10000|10100"
-- Left "Inconsistent length of \23383 (\19968\20108\19977) and \22235\35282 ([[(\"1000\",\"0\"),(\"1010\",\"0\")]])"


p_r_shapeVariant :: Text -> Text -> Either String [ShapeVariant]
p_r_shapeVariant cs ssRaw = do
  let p_sk "" = Right []
      p_sk str = mapM p_r_四角 $ T.split (`T.elem` "/") str
  sss <- mapM p_sk $ T.split (`T.elem` "|") ssRaw
  case (cs, sss) of
    ("", []) -> Right $ []
    ("", [[]]) -> Right $ []
    (_, []) -> Left "missing 四角"
    ("", _) -> Left "missing 字 for 四角"
    (_, _) ->
      if T.length cs == length sss
        then mapM f $ zip (T.unpack cs) sss
        else Left $ printf "Inconsistent length of 字 (%s) and 四角 (%s)" cs (show sss)
        where
          f (_, []) = Left "missing 四角"
          f (c, (s:ss)) = Right $ ShapeVariant
            { s_字 = T.singleton c
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

p_r_part :: Text -> Text -> Either String Part
p_r_part pIRaw pE = do
  (n, r) <- TR.decimal pIRaw :: Either String (Int, Text)
  pI <- case r of
    "" -> Right (n, 0)
    ".999" -> Right (n, 1)
    _ -> Left $ printf "trailing garbage: %s" r
  Right $ Part
    { p_玉篇部首位 = pI
    , p_部外 = pE
    }

-- | Parse parts info
--
-- Examples
--
-- >>> p_r_parts "26" "充" []
-- Right (Parts {p_諧符部 = "\20805", p_諧符位 = 26, p_variant = False, p_parts = []})
--
-- >>> p_r_parts "26" "充" [("0", "0")]
-- Right (Parts {p_諧符部 = "\20805", p_諧符位 = 26, p_variant = False, p_parts = []})
--
-- >>> p_r_parts "26" "充" [("0", "⌥")]
-- Right (Parts {p_諧符部 = "\20805", p_諧符位 = 26, p_variant = True, p_parts = []})
--
-- >>> p_r_parts "26" "充" [("269", "金")]
-- Right (Parts {p_諧符部 = "\20805", p_諧符位 = 26, p_variant = False, p_parts = [Part {p_玉篇部首位 = (269,0), p_部外 = "\37329"}]})
--
-- >>> p_r_parts "32" "竹筑" [("73.999", "巩")]
-- Right (Parts {p_諧符部 = "\31481", p_諧符位 = 32, p_variant = False, p_parts = [Part {p_玉篇部首位 = (73,1), p_部外 = "\24041"}]})

p_r_parts :: Text -> Text -> [(Text, Text)] -> Either String Parts
p_r_parts ki kp psRaw = do
  (kin, r) <- TR.decimal ki :: Either String (Int, Text)
  () <- if T.null r
    then Right ()
    else Left $ printf "trailing garbage: %s" r
  let (f_variant, ps) = case psRaw of
        ("0", "⌥"):psT -> (True, psT)
        psT -> (False, psT)
  f_parts <- mapM (uncurry p_r_part) $ filter (/= ("0", "0")) ps
  return $ Parts
    { p_諧符部 = T.take 1 kp
    , p_諧符位 = kin
    , p_variant = f_variant
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

-- | Parse a 反切
--
-- Examples
-- >>> p_r_反切 "反" (NEL.singleton "切韻") "なし"
-- Right (Just (Pronunciation反切 {pr_反切 = Nothing, pr_反切_suffix = "\21453", pr_反切_comment = Nothing, pr_反切_books = "\20999\38907" :| []}))
--
-- >>> p_r_反切 "反" (NEL.singleton "切韻") "（）"
-- Right (Just (Pronunciation反切 {pr_反切 = Nothing, pr_反切_suffix = "\21453", pr_反切_comment = Just "", pr_反切_books = "\20999\38907" :| []}))
--
-- >>> p_r_反切 "反" (NEL.singleton "切韻") "（コメント）"
-- Right (Just (Pronunciation反切 {pr_反切 = Nothing, pr_反切_suffix = "\21453", pr_反切_comment = Just "\12467\12513\12531\12488", pr_反切_books = "\20999\38907" :| []}))
--
-- >>> p_r_反切 "反" (NEL.singleton "切韻") "而鋭（コメント）"
-- Right (Just (Pronunciation反切 {pr_反切 = Just "\32780\37613", pr_反切_suffix = "\21453", pr_反切_comment = Just "\12467\12513\12531\12488", pr_反切_books = "\20999\38907" :| []}))
--
-- >>> p_r_反切 "反" (NEL.singleton "切韻") "〓〓（《王三》未收）"
-- Right (Just (Pronunciation反切 {pr_反切 = Just "\12307\12307", pr_反切_suffix = "\21453", pr_反切_comment = Just "\12298\29579\19977\12299\26410\25910", pr_反切_books = "\20999\38907" :| []}))
--
p_r_反切 :: Text -> NonEmpty Text -> Text -> Either String (Maybe Pronunciation反切)
p_r_反切 _ _ "" = Right Nothing
p_r_反切 suf books "なし" = Right . Just $ Pronunciation反切
    { pr_反切 = Nothing
    , pr_反切_suffix = suf
    , pr_反切_comment = Nothing
    , pr_反切_books = books
    }
p_r_反切 suf books pc | T.take 1 pc == "（" && T.takeEnd 1 pc == "）" = Right . Just $ Pronunciation反切
    { pr_反切 = Nothing
    , pr_反切_suffix = suf
    , pr_反切_comment = Just . T.drop 1 . T.dropEnd 1 $ pc
    , pr_反切_books = books
    }
p_r_反切 _ books pc | T.length pc == 3 = Right . Just $ Pronunciation反切
    { pr_反切 = Just $ T.take 2 pc
    , pr_反切_suffix = T.drop 2 pc
    , pr_反切_comment = Nothing
    , pr_反切_books = books
    }
p_r_反切 suf books pc | T.length pc == 2 = Right . Just $ Pronunciation反切
    { pr_反切 = Just pc
    , pr_反切_suffix = suf
    , pr_反切_comment = Nothing
    , pr_反切_books = books
    }
p_r_反切 _ books pc | T.take 1 (T.drop 3 pc) == "（" && T.takeEnd 1 pc == "）" = Right . Just $ Pronunciation反切
    { pr_反切 = Just $ T.take 2 pc
    , pr_反切_suffix = T.take 1 . T.drop 2 $ pc
    , pr_反切_comment = Just . T.drop 3 . T.dropEnd 1 $ pc
    , pr_反切_books = books
    }
p_r_反切 suf books pc | T.take 1 (T.drop 2 pc) == "（" && T.takeEnd 1 pc == "）" = Right . Just $ Pronunciation反切
    { pr_反切 = Just $ T.take 2 pc
    , pr_反切_suffix = suf
    , pr_反切_comment = Just . T.drop 3 . T.dropEnd 1 $ pc
    , pr_反切_books = books
    }
p_r_反切 _ _ pc = Left $ "Unknown 反切 format: " <> T.unpack pc

p_r_反切集 :: Text -> Text -> Text -> Text -> Text -> Either String Pronunciation反切集
p_r_反切集 pC pCBooksRaw pU pK pDz = do
  pCBooks <- p_r_反切本 pCBooksRaw
  f_pr_切韵反切 <- p_r_反切 "反" pCBooks pC
  f_pr_王韵反切 <- p_r_反切 "反" (NEL.singleton "王韵") pU
  f_pr_廣韵反切 <- p_r_反切 "切" (NEL.singleton "廣韵") pK
  f_pr_集韵反切 <- p_r_反切 "切" (NEL.singleton "集韵") pDz
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

p_r_義 :: Text -> Either String (Maybe Text)
p_r_義 "" = Right Nothing
p_r_義 t = Right $ Just t

maybeToRight :: a -> Maybe b -> Either a b
maybeToRight _ (Just x) = Right x
maybeToRight y Nothing  = Left y

parseValidRow :: (HasCallStack) => Int -> Text -> Map Text Text -> Either String Row
parseValidRow row version m = do
  let lookupField f = maybeToRight f $ M.lookup (fromString f) m

  f_字 <- p_r_字 =<< lookupField "字"
  f_親_pl <- left ("親: " <>) . join $ p_r_shapeVariant <$> lookupField "字" <*> lookupField "四角"
  f_親 <- case f_親_pl of
            [] -> Left "Lacks 四角 for the head character"
            [f_親] -> Right f_親
            _ -> Left "Multiple 四角 for the head character"

  f_選 <- left ("選: " <>) . join $ p_r_shapeVariant <$> lookupField "選" <*> lookupField "四角選"
  f_簡 <- left ("簡: " <>) . join $ p_r_shapeVariant <$> lookupField "簡" <*> lookupField "四簡"
  f_日 <- left ("日: " <>) . join $ p_r_shapeVariant <$> lookupField "日" <*> lookupField "四日"
  f_异 <- left ("异: " <>) . join $ p_r_shapeVariant <$> lookupField "异" <*> lookupField "四异"
  let f_shapeVariants = ShapeVariants
        { s_親 = f_親
        , s_選 = f_選
        , s_簡 = f_簡
        , s_日 = f_日
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
    join $ p_r_parts <$> lookupField "諧聲位" <*> lookupField "諧聲" <*> partsFields


  f_r_漢辭海 <- join $ p_r_漢辭海 <$> lookupField "漢辭海聲" <*> lookupField "漢辭海韵" <*> lookupField "漢辭海調"
  f_r_辭源韵 <- join $ p_r_辭源韵 <$> lookupField "辭源韵"
  f_r_反切集 <- join $ p_r_反切集 <$> lookupField "切韵反切" <*> lookupField "切韵本" <*> lookupField "王韵反切" <*> lookupField "廣韵反切" <*> lookupField "集韵反切"
  let f_pronunciation = Pronunciation
        { pr_漢辭海 = f_r_漢辭海
        , pr_辭源韵 = f_r_辭源韵
        , pr_反切集 = f_r_反切集
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
  if version `elem` ["3", "4", "5"]
    then fmap Just $ parseValidRow row version m
    else return Nothing
