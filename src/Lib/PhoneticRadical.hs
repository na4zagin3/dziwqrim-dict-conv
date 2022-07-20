{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Lib.PhoneticRadical where

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
import Lib.Row (p_r_phoneticNumber, p_r_number, p_r_numberPair, maybeToRight, PhoneticPartNumber(..))
import Text.Printf (printf)

data MainRhyme = MainRhyme
  { mr_index :: ![Int]
  , mr_text :: !Text
  , mr_subIndex :: !Int
  }
  deriving (Eq, Ord, Show, Read)

data PhoneticRadical = PhoneticRadical
  { pr_radical :: !Text
  , pr_pronunciation :: ![Text]
  , pr_index :: !PhoneticPartNumber
  , pr_comment :: !Text
  , pr_mainRhyme :: !MainRhyme
  }
  deriving (Eq, Ord, Show, Read)

p_pr_pronunciation :: Text -> [Text]
p_pr_pronunciation "" = []
p_pr_pronunciation "—" = []
p_pr_pronunciation s = T.split (`T.elem` "\n") s

decodeComment :: Text -> Text
decodeComment = T.replace "->" "→"

parsePhoneticRadical :: (HasCallStack) => Int -> Map Text Text -> Either String PhoneticRadical
parsePhoneticRadical row m = do
  let lookupField f = maybeToRight f $ M.lookup (fromString f) m
  f_pr_radical <- lookupField "聲首"
  f_pr_index <- p_r_phoneticNumber =<< lookupField "聲位"
  f_pr_comment1 <- decodeComment <$> lookupField "コメント1"
  f_pr_comment2 <- decodeComment <$> lookupField "コメント2"
  f_pr_mainRhyme_index <- left ("主韵部序: " <> ) $ fmap (\(a, b) -> [a, b]) .  p_r_numberPair =<< lookupField "主韵部序"
  f_pr_mainRhyme_text <- left ("主韵部1: " <> ) $ lookupField "主韵部1"
  f_pr_mainRhyme_subIndex <- left ("主韵部2: " <> ) $ p_r_number =<< lookupField "主韵部2"

  f_pr_pronunciation <- p_pr_pronunciation <$> lookupField "諧聲域"
  return $ PhoneticRadical
    { pr_radical = f_pr_radical
    , pr_pronunciation = f_pr_pronunciation
    , pr_index = f_pr_index
    -- , pr_comment = f_pr_comment1
    , pr_comment = f_pr_comment2
    , pr_mainRhyme = MainRhyme
      { mr_index = f_pr_mainRhyme_index
      , mr_text = f_pr_mainRhyme_text
      , mr_subIndex = f_pr_mainRhyme_subIndex
      }
    }

rhymeNoteMap :: Map Text Text
rhymeNoteMap = M.fromList
  [ ("職部", "之職蒸")
  , ("覺部", "幽覺冬")
  , ("藥部", "宵藥")
  , ("屋部", "侯屋東")
  , ("鐸部", "魚鐸陽")
  , ("錫部", "支錫耕")
  , ("質部", "脂質真")
  , ("物部", "微物文")
  , ("月部", "歌月元")
  , ("緝部", "緝侵")
  , ("葉部", "葉談")
  ]
