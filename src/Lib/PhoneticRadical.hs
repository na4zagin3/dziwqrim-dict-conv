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
import Lib.Row (p_r_phoneticNumber, maybeToRight, PhoneticPartNumber(..))
import Text.Printf (printf)

data PhoneticRadical = PhoneticRadical
  { pr_radical :: !Text
  , pr_pronunciation :: ![Text]
  , pr_index :: !PhoneticPartNumber
  , pr_comment :: !Text
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
  f_pr_comment <- decodeComment <$> lookupField "コメント"
  f_pr_pronunciation <- p_pr_pronunciation <$> lookupField "諧聲域"
  return $ PhoneticRadical
    { pr_radical = f_pr_radical
    , pr_pronunciation = f_pr_pronunciation
    , pr_index = f_pr_index
    , pr_comment = f_pr_comment
    }
