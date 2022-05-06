{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Lib.Entry where

import Control.Arrow (left)
import Data.ByteString.Lazy qualified as BL
import Data.Csv qualified as Csv
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Maybe (fromMaybe, catMaybes)
import Data.Vector (Vector)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read as TR
import Text.Printf (printf)
import GHC.Generics (Generic)

import Lib.Row

data Entry音義 = Entry音義
  { e_隋音 :: !Text
  , e_義 :: !(Maybe Text)
  }
  deriving (Read, Show, Eq, Ord, Generic)

data Entry音 = Entry音
  deriving (Read, Show, Eq, Ord, Generic)

-- Entry
data Entry = Entry
  { e_字 :: !Text
  , e_shapeVariants :: !ShapeVariants
  , e_部畫 :: !(Maybe Shape部畫)
  , e_音義 :: ![Entry音義]
  }
  deriving (Read, Show, Eq, Ord, Generic)

entriesFromRows :: (Functor f) => f Row -> f Entry
entriesFromRows = fmap entryFromRow
  where
    entryFromRow r = Entry
                     { e_字 = r_字 r
                     , e_shapeVariants = r_shapeVariants r
                     , e_部畫 = r_部畫 r
                     , e_音義 = [Entry音義
                                 { e_隋音 = r_隋音 r
                                 , e_義 = r_義 r
                                 }]
                     }

variantToTex label [] pre = Nothing
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

entryToTex :: Entry -> Text
entryToTex e = mconcat
    [ "\\begin{Entry}{", e_字 e, "}{", e_字 e, "}\n"
    -- , "  + 0（夊部0畫）\n"
    , fromMaybe "" . fmap shape部畫ToTex $ e_部畫 e
    , "  \\\\\n"
    , "  ", shapeVariantsToTex $ e_shapeVariants e, "\n"
    , "  \\begin{Sound}\n"
    , T.intercalate "\n" tex音Items
    -- , "    \\SoundItem{toŋ}《P3798》都宗反，端冬平，上平聲二冬\n"
    , "  \\end{Sound}\n"
    , "\\end{Entry}\n"
    , if null tex義Items
      then ""
      else "\\begin{Sense}" <> T.intercalate "\n" tex義Items <> "\\end{Sense}"
    ]
  where
    tex音Items = map (\sp -> "\\SoundItem{" <> e_隋音 sp <> "}") $ e_音義 e
    tex義Items = catMaybes . map render義Item $ e_音義 e
    render義Item sp = do
      s <- e_義 sp
      return $ "\\SenseItem{" <> e_隋音 sp <> "}" <> s
