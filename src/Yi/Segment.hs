{-# language TypeFamilies #-}
{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language StandaloneDeriving #-}
{-# language UndecidableInstances #-}
{-# language GADTs #-}
module Yi.Segment where

import qualified Data.Text as TX
import Data.Typeable
import qualified Data.FingerTree as T

class MSeg t where
  type Meas t

class (Monoid t) => Segmented t where
  type Segment t
  length :: t -> Int
  null :: t -> Bool
  head :: t -> Segment t
  last :: t -> Segment t
  init :: t -> t
  tail :: t -> t
  take :: Int -> t -> t
  drop :: Int -> t -> t
  singleton :: Segment t -> t
  splitAt :: Int -> t -> (t, t)
  cons :: Segment t -> t -> t
  filter :: (Segment t -> Bool) -> t -> t
  any :: (Segment t -> Bool) -> t -> Bool
  all :: (Segment t -> Bool) -> t -> Bool
  reverse :: t -> t
  takeWhile :: (Segment t -> Bool) -> t -> t
  dropWhile :: (Segment t -> Bool) -> t -> t
  dropWhileEnd :: (Segment t -> Bool) -> t -> t
  concat :: [t] -> t
  split :: (Segment t -> Bool) -> t -> [t]
  chunksOf :: Int -> t -> [t]
  snoc :: t -> Segment t -> t
  foldl :: (a -> Segment t -> a) -> a -> t -> a
  map :: (Segment t -> Segment t) -> t -> t
  replicate :: Int -> t -> t

instance Segmented TX.Text where
  type Segment TX.Text = Char
  length = TX.length
  null = TX.null
  head = TX.head
  last = TX.last
  init = TX.init
  tail = TX.tail
  take = TX.take
  drop = TX.drop
  cons = TX.cons
  filter = TX.filter
  any = TX.any
  all = TX.all
  reverse = TX.reverse
  takeWhile = TX.takeWhile
  dropWhile = TX.dropWhile
  dropWhileEnd = TX.dropWhileEnd
  concat = TX.concat
  split = TX.split
  chunksOf = TX.chunksOf
  snoc = TX.snoc
  foldl = TX.foldl'
  map = TX.map
  replicate = TX.replicate
