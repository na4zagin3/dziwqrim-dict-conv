{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib.PathTree where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)

type KeyPath k = [k]

newtype PathTree k v = PathTree (Map (KeyPath k) v)
  deriving (Read, Show, Eq, Functor, Foldable, Traversable, Generic)

empty :: PathTree k v
empty = PathTree Map.empty

singleton :: KeyPath k -> v -> PathTree k v
singleton k v = PathTree (Map.singleton k v)

insertWith :: Ord k => (a -> a -> a) -> KeyPath k -> a -> PathTree k a -> PathTree k a
insertWith f k v (PathTree m) = PathTree $ Map.insertWith f k v m

union :: Ord k => PathTree k a -> PathTree k a -> PathTree k a
union (PathTree a) (PathTree b) = PathTree $ Map.union a b

lookup :: Ord k => KeyPath k -> PathTree k a -> Maybe a
lookup k (PathTree m) = Map.lookup k m

toList :: PathTree k v -> [([k], v)]
toList (PathTree ns) = Map.toList ns

fromList :: Ord k => [([k], v)] -> PathTree k v
fromList l = PathTree $ Map.fromList l

root :: (Ord k) => PathTree k v -> Maybe v
root (PathTree m) = Map.lookup [] m

children :: (Ord k) => PathTree k v -> Map k (PathTree k v)
children m = Map.unionsWith union . catMaybes . map f $ toList m
  where
    f ([], _) = Nothing
    f (k:ks, v) = Just $ Map.singleton k (singleton ks v)
