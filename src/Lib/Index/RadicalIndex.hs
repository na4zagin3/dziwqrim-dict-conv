{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Lib.Index.RadicalIndex where

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
import Lib.Entry (renderSikrokToTex, Entry(..), EntrySortKey(..), Section(..), entryToLabel, entrySortKey)
import Lib.Row (ShapeVariant(..), ShapeVariants(..), Part(..), Shape部畫(..))
import Text.Printf (printf)

data RadicalEntry = RadicalEntry
  { sk_r_radical :: !Shape部畫
  , sk_r_entry :: !Text
  , sk_r_label :: !Text
  , sk_r_sortKey :: !EntrySortKey
  }
  deriving (Read, Show, Eq, Ord, Generic)

data RadicalSection = RadicalSection
  { sk_r_header :: !Text
  , sk_r_entries :: ![RadicalEntry]
  }
  deriving (Read, Show, Eq, Ord, Generic)

radicalEntryToTex :: RadicalEntry -> Text
radicalEntryToTex RadicalEntry
  { sk_r_radical = r
  , sk_r_entry = e
  , sk_r_label = l
  } = mconcat
      [ "\\RadicalEntry"
      , "{"
      , l
      , "}"
      , "{"
      , s_部 r
      , "}"
      , "{"
      , T.pack . show $ s_畫 r
      , "}"
      , "{"
      , e
      , "}"
      ]

radicalSectionToTex :: RadicalSection -> Text
radicalSectionToTex RadicalSection
              { sk_r_header = h
              , sk_r_entries = es
              } = T.intercalate "\n" $ mconcat [[header (MS.lookup h radicalMap)], contents, [footer]]
  where
    header Nothing = mconcat
      [ "\\begin{RadicalSection}{"
      , "---"
      , "}{"
      , h
      , "}"
      ]
    header (Just (i, r)) = mconcat
      [ "\\begin{RadicalSection}{"
      , i
      , "}{"
      , r
      , "}"
      ]
    contents = map radicalEntryToTex es
    footer = mconcat
      [ "\\end{RadicalSection}"
      ]

radicalSectionsToTex :: [RadicalSection] -> Text
radicalSectionsToTex sss = T.intercalate "\n" $ concat [[header], contents, [footer]]
  where
    header = mconcat
      [ "\\begin{RadicalIndex}"
      , "{"
      , "\\begin{RadicalIndexRadicalIndex}"
      , T.intercalate "\n" radicals
      , "\\end{RadicalIndexRadicalIndex}"
      , "}"
      ]
    radicals = concatMap (\(str, rs) ->
                      [T.pack $ printf "\\RadicalIndexRadicalIndexLabel{%d}" str]
                   <> map rsToTex rs
                      ) radicalTable
    rsToTex (_, i, r) = T.pack $ printf "\\RadicalIndexRadicalIndexElem{%s}{%s}" i r
    contents = map radicalSectionToTex sss
    footer = mconcat
      [ "\\end{RadicalIndex}"
      ]

entryToRadicalEntries :: [Part] -> Entry -> [RadicalEntry]
entryToRadicalEntries p e = indexRadicals svParent (e_部畫 e)
  where
    indexRadicals ShapeVariant{s_字 = z, s_四角 = _} Nothing = []
    indexRadicals ShapeVariant{s_字 = z, s_四角 = _} (Just r) =
      [ RadicalEntry
        { sk_r_radical = r
        , sk_r_entry = z
        , sk_r_label = label
        , sk_r_sortKey = entrySortKey e
        }
      ]
    label = entryToLabel e
    svParent = s_親 sv
    sv = e_shapeVariants $ e

sectionsToRadicalSections :: [Section] -> [RadicalSection]
sectionsToRadicalSections ss = sections
  where
    ses = M.fromListWith (++) . map (\e -> (s_部 $ sk_r_radical e, [e])) $ concatMap (uncurry entryToRadicalEntries) es
    es :: [([Part], Entry)]
    es = concatMap (concatMap (\(p, vs) -> map (\v -> (p, v)) vs) . PT.toList . sec_entries) ss
    sections =
      map (\(p, es) -> RadicalSection
            { sk_r_header = p
            , sk_r_entries = L.sortOn (\e -> (s_畫 $ sk_r_radical e, sk_r_sortKey e)) es
            }) $ M.toList ses

radicalMap :: MS.Map Text (Text, Text)
radicalMap = M.fromList $ concatMap (\(k, i, r) -> map (\c -> (T.singleton c, (i, k))) $ T.unpack k) $ concatMap (\(_, rs) -> rs) radicalTable

radicalTable :: [(Int, [(Text, Text, Text)])]
radicalTable =
  [(1,
    [ ("一", "1", "一")
    , ("丨", "2", "丨")
    , ("丶", "3", "丶")
    , ("丿", "4", "丿")
    , ("乙", "5", "乙")
    , ("亅", "6", "亅")
    ]
   ),
   (2,
    [ ("二", "7", "二")
    , ("亠", "8", "亠")
    , ("人", "9", "人")
    , ("儿", "10", "儿")
    , ("入", "11", "入")
    , ("八", "12", "八")
    , ("冂", "13", "冂")
    , ("冖", "14", "冖")
    , ("冫", "15", "冫")
    , ("几", "16", "几")
    , ("凵", "17", "凵")
    , ("刀", "18", "刀")
    , ("力", "19", "力")
    , ("勹", "20", "勹")
    , ("匕", "21", "匕")
    , ("匚匸", "22/23", "匚\\KanjiCdot 匸")
    , ("十", "24", "十")
    , ("卜", "25", "卜")
    , ("卩", "26", "卩")
    , ("厂", "27", "厂")
    , ("厶", "28", "厶")
    , ("又", "29", "又")
    ]
   )
  , (3,
      [ ("口", "30", "口")
      , ("囗", "31", "囗")
      , ("土", "32", "土")
      , ("士", "33", "士")
      , ("夂夊", "34/35", "夂\\KanjiCdot 夊")
      , ("夕", "36", "夕")
      , ("大", "37", "大")
      , ("女", "38", "女")
      , ("子", "39", "子")
      , ("宀", "40", "宀")
      , ("寸", "41", "寸")
      , ("小", "42", "小")
      , ("尢", "43", "尢")
      , ("尸", "44", "尸")
      , ("屮", "45", "屮")
      , ("山", "46", "山")
      , ("巛", "47", "巛")
      , ("工", "48", "工")
      , ("己", "49", "己")
      , ("巾", "50", "巾")
      , ("干", "51", "干")
      , ("幺", "52", "幺")
      , ("广", "53", "广")
      , ("廴", "54", "廴")
      , ("廾", "55", "廾")
      , ("弋", "56", "弋")
      , ("弓", "57", "弓")
      , ("彐", "58", "彐")
      , ("彡", "59", "彡")
      , ("彳", "60", "彳")
      ]
    )
  , (4, [ ("心", "61", "心")
        , ("戈", "62", "戈")
        , ("户", "63", "户")
        , ("手", "64", "手")
        , ("攴攴", "65/66", "攴\\KanjiCdot 攴")
        , ("文", "67", "文")
        , ("斗", "68", "斗")
        , ("斤", "69", "斤")
        , ("方", "70", "方")
        , ("无", "71", "无")
        , ("日", "72", "日")
        , ("曰", "73", "曰")
        , ("月", "74", "月")
        , ("木", "75", "木")
        , ("欠", "76", "欠")
        , ("止", "77", "止")
        , ("歹", "78", "歹")
        , ("殳", "79", "殳")
        , ("毋", "80", "毋")
        , ("比", "81", "比")
        , ("毛", "82", "毛")
        , ("氏", "83", "氏")
        , ("气", "84", "气")
        , ("水", "85", "水")
        , ("火", "86", "火")
        , ("爪", "87", "爪")
        , ("父", "88", "父")
        , ("爻", "89", "爻")
        , ("爿", "90", "爿")
        , ("片", "91", "片")
        , ("牙", "92", "牙")
        , ("牛", "93", "牛")
        , ("犬", "94", "犬")
        ]
    )
  , (5, [("玄", "95", "玄")
        , ("瓜", "96", "瓜")
        , ("玉", "97", "玉")
        , ("瓦", "98", "瓦")
        , ("甘", "99", "甘")
        , ("生", "100", "生")
        , ("用", "101", "用")
        , ("田", "102", "田")
        , ("疋", "103", "疋")
        , ("疒", "104", "疒")
        , ("癶", "105", "癶")
        , ("白", "106", "白")
        , ("皮", "107", "皮")
        , ("皿", "108", "皿")
        , ("目", "109", "目")
        , ("矛", "110", "矛")
        , ("矢", "111", "矢")
        , ("石", "112", "石")
        , ("示", "113", "示")
        , ("禸", "114", "禸")
        , ("禾", "115", "禾")
        , ("穴", "116", "穴")
        , ("立", "117", "立")
        ]
    )
  , (6, [("竹", "118", "竹")
        , ("米", "119", "米")
        , ("糸", "120", "糸")
        , ("缶", "121", "缶")
        , ("网", "122", "网")
        , ("羊", "123", "羊")
        , ("羽", "124", "羽")
        , ("老", "125", "老")
        , ("而", "126", "而")
        , ("耒", "127", "耒")
        , ("耳", "128", "耳")
        , ("聿", "129", "聿")
        , ("肉", "130", "肉")
        , ("臣", "131", "臣")
        , ("自", "132", "自")
        , ("至", "133", "至")
        , ("臼", "134", "臼")
        , ("舌", "135", "舌")
        , ("舛", "136", "舛")
        , ("舟", "137", "舟")
        , ("艮", "138", "艮")
        , ("色", "139", "色")
        , ("艸", "140", "艸")
        , ("虍", "141", "虍")
        , ("虫", "142", "虫")
        , ("血", "143", "血")
        , ("行", "144", "行")
        , ("衣", "145", "衣")
        , ("襾", "146", "襾")
        ]
    )
  , (7, [("見", "147", "見")
        , ("角", "148", "角")
        , ("言", "149", "言")
        , ("谷", "150", "谷")
        , ("豆", "151", "豆")
        , ("豕", "152", "豕")
        , ("豸", "153", "豸")
        , ("貝", "154", "貝")
        , ("赤", "155", "赤")
        , ("走", "156", "走")
        , ("足", "157", "足")
        , ("身", "158", "身")
        , ("車", "159", "車")
        , ("辛", "160", "辛")
        , ("辰", "161", "辰")
        , ("辵", "162", "辵")
        , ("邑", "163", "邑")
        , ("酉", "164", "酉")
        , ("釆", "165", "釆")
        , ("里", "166", "里")
        ]
    )
  , (8, [("金", "167", "金")
        , ("長", "168", "長")
        , ("門", "169", "門")
        , ("阜", "170", "阜")
        , ("隶", "171", "隶")
        , ("隹", "172", "隹")
        , ("雨", "173", "雨")
        , ("青", "174", "青")
        , ("非", "175", "非")
        ]
    )
  , (9, [("面", "176", "面")
        , ("革", "177", "革")
        , ("韋", "178", "韋")
        , ("韭", "179", "韭")
        , ("音", "180", "音")
        , ("頁", "181", "頁")
        , ("風", "182", "風")
        , ("飛", "183", "飛")
        , ("食", "184", "食")
        , ("首", "185", "首")
        , ("香", "186", "香")
        ]
    )
  , (10, [("馬", "187", "馬")
         , ("骨", "188", "骨")
         , ("高", "189", "高")
         , ("髟", "190", "髟")
         , ("鬥", "191", "鬥")
         , ("鬯", "192", "鬯")
         , ("鬲", "193", "鬲")
         , ("鬼", "194", "鬼")
         ]
    )
  , (11, [("魚", "195", "魚")
         , ("鳥", "196", "鳥")
         , ("鹵", "197", "鹵")
         , ("鹿", "198", "鹿")
         , ("麥", "199", "麥")
         , ("麻", "200", "麻")
         ]
    )
  , (12, [ --("黃", "201", "黃")
           ("黄", "201", "黄")
         , ("黍", "202", "黍")
         , ("黑", "203", "黑")
         , ("黹", "204", "黹")
         ]
    )
  , (13, [("黽", "205", "黽")
         , ("鼎", "206", "鼎")
         , ("鼓", "207", "鼓")
         , ("鼠", "208", "鼠")
         ]
    )
  , (14, [("鼻", "209", "鼻")
         , ("齊", "210", "齊")
         ]
    )
  , (15, [("齒", "211", "齒")
         ]
    )
  , (16, [("龍", "212", "龍")
         , ("龜", "213", "龜")
         ]
    )
  , (17, [("龠", "214", "龠")])
  ]
