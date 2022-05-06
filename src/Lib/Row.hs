{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Lib.Row where

import Control.Arrow (left)
import Data.ByteString.Lazy qualified as BL
import Data.Csv qualified as Csv
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Vector (Vector)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read as TR
import Text.Printf (printf)
import GHC.Generics (Generic)


data ShapeVariant = ShapeVariant
  { s_字 :: !Text
  , s_四角 :: !(Text, Text)
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

data Row = Row
  { r_字 :: !Text
  , r_shapeVariants :: !ShapeVariants
  , r_部畫 :: !(Maybe Shape部畫)
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
-- Right (Just ("5000","6"))
--
-- >>> p_r_四角 "50006/50006"
-- Left "trailing garbage: /50006"
--
-- >>> p_r_四角 "123456"
-- Left "123456 is too large"

p_r_四角 :: Text -> Either String (Maybe (Text, Text))
p_r_四角 "" = Right Nothing
p_r_四角 t = do
  (n, r) <- TR.decimal t :: Either String (Int, Text)
  () <- if T.null r
    then Right ()
    else Left $ printf "trailing garbage: %s" r
  let (dm, ds) = divMod n 10
  if dm > 9999
    then Left $ printf "%s is too large" t
    else Right $ Just (T.pack $ printf "%04d" dm, T.pack $ printf "%01d" ds)

-- | Parse shape variant information
--
-- Examples
--
-- >>> p_r_shape "中" "50006"
-- Right [ShapeVariant {s_字 = "\20013", s_四角 = ("5000","6"), s_comment = ""}]
-- >>> p_r_shape "吶㕯" "64027/40227"
-- Right [ShapeVariant {s_字 = "\21558", s_四角 = ("6402","7"), s_comment = ""},ShapeVariant {s_字 = "\13679", s_四角 = ("4022","7"), s_comment = ""}]
-- >>> p_r_shape "皐臯" "26409|26409"
-- Right [ShapeVariant {s_字 = "\30352", s_四角 = ("2640","9"), s_comment = ""},ShapeVariant {s_字 = "\33263", s_四角 = ("2640","9"), s_comment = ""}]
-- >>> p_r_shape "一二三" "10000|10100"
-- Left "Inconsistent length of \23383 (\19968\20108\19977) and \22235\35282 ([(\"1000\",\"0\"),(\"1010\",\"0\")])"

p_r_shapeVariant :: Text -> Text -> Either String [ShapeVariant]
p_r_shapeVariant c s = do
  s <- fmap catMaybes . mapM p_r_四角 $ T.split (`T.elem` "/|") s
  case (c, s) of
    ("", []) -> Right $ []
    (_, []) -> Left "missing 四角"
    ("", _) -> Left "missing 字 for 四角"
    (c, ss) ->
      if T.length c == length ss
        then Right $ map f $ zip (T.unpack c) ss
        else Left $ printf "Inconsistent length of 字 (%s) and 四角 (%s)" c (show s)
        where
          f (c, s) = ShapeVariant
            { s_字 = T.singleton c
            , s_四角 = s
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

parseRow :: Map Text Text -> Either String Row
parseRow m = do
  f_字 <- p_r_字 $ m M.! "字"
  f_親_pl <- left ("親: " <>) $ p_r_shapeVariant (m M.! "字") (m M.! "四角")
  f_親 <- case f_親_pl of
            [] -> Left "Lacks 四角 for the head character"
            [f_親] -> Right f_親
            _ -> Left "Multiple 四角 for the head character"

  f_選 <- left ("選: " <>) $ p_r_shapeVariant (m M.! "選") (m M.! "四角選")
  f_簡 <- left ("簡: " <>) $ p_r_shapeVariant (m M.! "簡") (m M.! "四簡")
  f_日 <- left ("日: " <>) $ p_r_shapeVariant (m M.! "日") (m M.! "四日")
  f_异 <- left ("异: " <>) $ p_r_shapeVariant (m M.! "异") (m M.! "四异")
  let f_shapeVariants = ShapeVariants
        { s_親 = f_親
        , s_選 = f_選
        , s_簡 = f_簡
        , s_日 = f_日
        , s_异 = f_异
        }

  f_部畫 <- left ("部畫: " <>) $ p_r_部畫 (m M.! "部") (m M.! "畫")

  return $ Row
    { r_字 = f_字
    , r_shapeVariants = f_shapeVariants
    , r_部畫 = f_部畫
    }
