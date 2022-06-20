{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Lib.Kanji where

import Data.Attoparsec.Text
import Data.Attoparsec.Text qualified as AP
import Data.Text (Text)
import Data.Text qualified as T
import Text.Printf (printf)

ids2, ids3 :: String
ids2 = "⿰⿱⿴⿵⿶⿷⿸⿹⿺⿻"
ids3 = "⿲⿳"

p_ids2 :: Parser Char
p_ids2 = satisfy (`elem` ids2)

p_ids3 :: Parser Char
p_ids3 = satisfy (`elem` ids3)

p_simpleKanji :: Parser Char
p_simpleKanji = satisfy notIds
  where
    notIds = not . isIds
    isIds = (`elem` (ids2 ++ ids3))

p_kanji :: Parser Text
p_kanji = choice [p_simpleKanji >>= return . T.singleton, p_seq2]
  where
    p_seq2 = do
      op <- p_ids2
      k1 <- p_kanji
      k2 <- p_kanji
      return $ mconcat [T.singleton op, k1, k2]

explodeKanji :: Text -> Either String [Text]
explodeKanji = parseOnly p
  where
    p = do
      ks <- many' p_kanji
      endOfInput
      return ks

p_book :: Parser Text
p_book = do
  string "《"
  cs <- manyTill' anyChar (string "》")
  return $ T.pack cs

-- | Parse a fanqie
--
-- >>> parseTest p_fanqie "胡山反"
-- Done "" ("\32993\23665","\21453")
-- >>> parseTest p_fanqie "胡山切"
-- Done "" ("\32993\23665","\20999")
p_fanqie = do
  f1 <- p_kanji
  f2 <- p_kanji
  fc <- choice [string "反", string "切"]
  return (f1 <> f2, fc)

p_paren s c e = do
  string s
  cs <- option "" c
  string e <?> printf "end paren '%s'" e
  return $ s <> cs <> e

-- | Parse a balanced text
--
-- >>> parseOnly p_balancedText "abc"
-- Right "abc"
p_balancedText = T.concat <$> many' f
  where
    f = choice $
        map (\(s, e) -> p_paren s p_balancedText e) parens
        <> [AP.takeWhile1 (notInClass "（《“）》”")]
    parens =
      [ ("（", "）")
      , ("《", "》")
      , ("“", "”")
      ]

-- | Parse a fanqie with an optional comment
--
-- >>> parseOnly p_fanqieCharacterNote "誤作“苦角反”"
-- Right "\35492\20316\8220\33510\35282\21453\8221"
-- >>> parseOnly p_fanqieCharacterNote "字作“墅”"
-- Right "\23383\20316\8220\22661\8221"
p_fanqieCharacterNote = do
  k <- choice . map string $ ["誤作", "字作"]
  cp <- option "" $ do
    string "“"
    c <- p_balancedText
    string "”"
    return $ "“" <> c <> "”"
  return $ k <> cp

p_fanqieNote = do
  k <- choice . map string $ ["未收", "脱字", "無本"]
  cp <- option "" $ do
    string "（"
    c <- p_balancedText
    string "）"
    return $ "（" <> c <> "）"
  return $ k <> cp

p_fanqieComment = do
  string "（"
  c <- p_balancedText
  string "）"
  return c

-- | Parse a fanqie with an optional comment
--
-- >>> parseOnly p_fanqieWithComment "而鋭反（コメント）"
-- Right (("\32780\37613","\21453"),Just "\12467\12513\12531\12488")
-- >>> parseOnly p_fanqieWithComment "〓〓反（《王三》未收）"
-- Right (("\12307\12307","\21453"),Just "\12298\29579\19977\12299\26410\25910")
p_fanqieWithComment = do
  fq <- p_fanqie
  c <- option Nothing (Just <$> p_fanqieComment)
  return $ (fq, c)

p_fanqieRhymeGroup = do
  rg <- p_kanji
  c <- string "韵"
  return rg

p_fanqieItem = do
  b <- option Nothing (Just <$> p_book)
  let parseFanqie rg = do
        (f, c) <- choice
          [ p_fanqieComment >>= \c -> return (Nothing, Just c)
          , p_fanqieCharacterNote  >>= \c -> return (Nothing, Just c)
          , p_fanqieNote >>= \c -> return (Nothing, Just c)
          , p_fanqieWithComment >>= \(fq, c) -> return (Just fq, c)
          ]
        return (b, rg, f, c)
  choice
    [ p_fanqieRhymeGroup >>= (parseFanqie . Just)
    , parseFanqie Nothing
    ]

-- | Parse a fanqie field
--
-- >>> parseOnly p_fanqieField ""
-- Right []
-- >>> parseOnly p_fanqieField "なし"
-- Right []
-- >>> parseOnly p_fanqieField "（）"
-- Right [(Nothing,Nothing,Just "")]
-- >>> parseOnly p_fanqieField "（コメント）"
-- Right [(Nothing,Nothing,Just "\12467\12513\12531\12488")]
-- >>> parseOnly p_fanqieField "而鋭反（コメント）"
-- Right [(Nothing,Just ("\32780\37613","\21453"),Just "\12467\12513\12531\12488")]
-- >>> parseOnly p_fanqieField "〓〓反（《王三》未收）"
-- Right [(Nothing,Just ("\12307\12307","\21453"),Just "\12298\29579\19977\12299\26410\25910")]
-- >>> parseOnly p_fanqieField "《王三》未收"
-- Right [(Just "\29579\19977",Nothing,Just "\26410\25910")]
-- >>> parseOnly p_fanqieField "《王三》胡孟反"
-- Right [(Just "\29579\19977",Just ("\32993\23391","\21453"),Nothing)]
-- >>> parseOnly p_fanqieField "《王三》脱字（脱反語）"
-- Right [(Just "\29579\19977",Nothing,Just "\33073\23383\65288\33073\21453\35486\65289")]
-- >>> parseOnly p_fanqieField "《王一》户恢反，《王三》未收"
-- Right [(Just "\29579\19968",Just ("\25143\24674","\21453"),Nothing),(Just "\29579\19977",Nothing,Just "\26410\25910")]
-- >>> parseOnly p_fanqieField "《王一》户孟反，《王三》胡孟反"
-- Right [(Just "\29579\19968",Just ("\25143\23391","\21453"),Nothing),(Just "\29579\19977",Just ("\32993\23391","\21453"),Nothing)]
-- >>> parseOnly p_fanqieField "《王三》脱字（脱反語），《王一》無本"
-- Right [(Just "\29579\19977",Nothing,Just "\33073\23383\65288\33073\21453\35486\65289"),(Just "\29579\19968",Nothing,Just "\28961\26412")]

p_fanqieField
  :: Parser [(Maybe Text, Maybe Text, Maybe (Text, Text), Maybe Text)]
p_fanqieField = choice
  [ string "なし" *> return []
  , p_fanqieItem `sepBy1` (char '，')
  , string "" *> return []
  ] <* endOfInput
