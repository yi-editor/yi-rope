{-# language BangPatterns #-}
{-# language DeriveDataTypeable #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language ViewPatterns #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language ConstraintKinds #-}
{-# options_haddock show-extensions #-}

-- |
-- Module      :  Yi.Braid
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines a @rope@ data structure for use in Yi. This
-- specific implementation uses a fingertree over Text.
--
-- In contrast to our old implementation, we can now reap all the
-- benefits of Text: automatic unicode handling and blazing fast
-- implementation on underlying strings. This frees us from a lot of
-- book-keeping. We don't lose out on not using ByteString directly
-- because the old implementation encoded it into UTF8 anyway, making
-- it unsuitable for storing anything but text.

module Yi.Braid
  ( Braid(..)
  , Chunk(..)
  , HasSize(..)
  , Yi.Braid.mkChunk
  , Yi.Braid.overChunk
  , (Yi.Braid.-|)
  , (Yi.Braid.|-)
  , Yi.Braid.reverse
  , Yi.Braid.toReversed
  , Yi.Braid.toBraid
  , Yi.Braid.toBraid'
  , Yi.Braid.extractBraid
  , Yi.Braid.null
  , Yi.Braid.empty
  , Yi.Braid.length
  , Yi.Braid.append
  , Yi.Braid.concat
  , Yi.Braid.head
  , Yi.Braid.last
  , Yi.Braid.init
  , Yi.Braid.tail
  , Yi.Braid.splitAt
  , Yi.Braid.take
  , Yi.Braid.drop
  , Yi.Braid.dropWhile
  , Yi.Braid.dropWhileEnd
  , Yi.Braid.takeWhile
  , Yi.Braid.takeWhileEnd
  , Yi.Braid.span
  , Yi.Braid.break
  , Yi.Braid.intercalate
  , Yi.Braid.intersperse
  , Yi.Braid.cons
  , Yi.Braid.snoc
  , Yi.Braid.singleton
  , Yi.Braid.any
  , Yi.Braid.all
  , Yi.Braid.filter
  , Yi.Braid.map
  , Yi.Braid.split
  , Yi.Braid.withChunk
  , Yi.Braid.unsafeWithChunk
  , Yi.Braid.foldl'
  , Yi.Braid.replicate
  , Yi.Braid.replicateSegment
  ) where

import           Control.DeepSeq
import qualified Data.FingerTree as T
import           Data.FingerTree hiding (null, empty, reverse, split)
import qualified Data.List as L (foldl')
import           Data.Maybe
import           Data.Monoid
import           Data.Typeable

import qualified Yi.Segment as S

type ValidBraid v a = (T.Measured v (Chunk a), S.Segmented a, HasSize v)

class HasSize a where
  getSize :: a -> Int


-- | Makes a chunk from a given string. We allow for an arbitrary
-- length function here to allow us to bypass the calculation with
-- 'const' in case the length is known ahead of time. In most cases,
-- the use of this is
--
-- > mkChunk 'TX.Text.length' someText
mkChunk :: (a -> Int) -- ^ The length function to use.
        -> a
        -> Chunk a
mkChunk l t = Chunk (l t) t

-- | Transform the chunk content. It's vital that the transformation
-- preserves the length of the content.
overChunk :: (a -> a) -- ^ Length-preserving content transformation.
          -> Chunk a -> Chunk a
overChunk f (Chunk l t) = Chunk l (f t)

newtype Braid v a = Braid { fromBraid :: T.FingerTree v (Chunk a) }
  deriving (Show, Typeable)

instance (ValidBraid v a) => Monoid (Braid v a) where
  mempty = Yi.Braid.empty
  mappend = Yi.Braid.append
  mconcat = Yi.Braid.concat

-- | Two 'YiString's are equal if their underlying text is.
--
-- Implementation note: This just uses 'TX.Text' equality as there is
-- no real opportunity for optimisation here except for a cached
-- length check first. We could unroll the trees and mess around with
-- matching prefixes but the overhead would be higher than a simple
-- conversion and relying on GHC optimisation.
--
-- The derived Eq implementation for the underlying tree only passes
-- the equality check if the chunks are the same too which is not what
-- we want.
instance (Eq a, Ord a, ValidBraid v a) => Ord (Braid v a) where
  compare x y = extractBraid x `compare` extractBraid y

data Chunk a = Chunk { chunkSize :: {-# UNPACK #-} !Int
                     , _fromChunk :: {-# UNPACK #-} !a
                     } deriving (Show, Eq, Typeable)

-- | Two 'YiString's are equal if their underlying text is.
--
-- Implementation note: This just uses 'TX.Text' equality as there is
-- no real opportunity for optimisation here except for a cached
-- length check first. We could unroll the trees and mess around with
-- matching prefixes but the overhead would be higher than a simple
-- conversion and relying on GHC optimisation.
--
-- The derived Eq implementation for the underlying tree only passes
-- the equality check if the chunks are the same too which is not what
-- we want.
instance (ValidBraid v a, Eq a) => Eq (Braid v a) where
  t == t' = Yi.Braid.length t == Yi.Braid.length t' && extractBraid t == extractBraid t'

instance (NFData a) => NFData (Chunk a) where
  rnf (Chunk !i !t) = i `seq` rnf t

instance (NFData a, ValidBraid v a) => NFData (Braid v a) where
  rnf = rnf . extractBraid

(-|) :: (ValidBraid v a) => (Chunk a) -> FingerTree v (Chunk a) -> FingerTree v (Chunk a)
b -| t | chunkSize b == 0 = t
       | otherwise        = b <| t

(|-) :: (ValidBraid v a) => FingerTree v (Chunk a) -> (Chunk a) -> FingerTree v (Chunk a)
t |- b | chunkSize b == 0 = t
       | otherwise        = t |> b

-- | Default size chunk to use. Currently @1200@ as this is what
-- benchmarks suggest.
--
-- This makes the biggest difference with 'lines'-like and
-- 'concat'-like functions. Bigger chunks make 'concat' (much) faster
-- but 'lines' slower. In general it seems that we benefit more from
-- larger chunks and 1200 seems to be the sweet spot.
defaultChunkSize :: Int
defaultChunkSize = 1200

-- | Reverse the whole underlying string.
--
-- This involves reversing the order of the chunks as well as content
-- of the chunks. We use a little optimisation here that re-uses the
-- content of each chunk but this exposes a potential problem: after
-- many transformations, our chunks size might become quite varied
-- (but never more than the default size), perhaps we should
-- periodically rechunk the tree to recover nice sizes?
reverse :: (ValidBraid v a) => Braid v a -> Braid v a
reverse = Braid . fmap' (overChunk S.reverse) . T.reverse . fromBraid

-- | This is like 'fromText' but it allows the user to specify the
-- chunk size to be used. Uses 'defaultChunkSize' if the given
-- size is <= 0.
toBraid' :: forall v a. (ValidBraid v a) => Int -> a -> Braid v a
toBraid' n | n <= 0 = toBraid' defaultChunkSize
            | otherwise = Braid . r T.empty . f
  where
    f = S.chunksOf n

    -- Convert the given string into chunks in the tree. We have a
    -- special case for a single element case: because we split on
    -- predetermined chunk size, we know that all chunks but the last
    -- one will be the specified size so we can optimise here instead
    -- of having to recompute chunk size at creation.
    r :: FingerTree v (Chunk a) -> [a] -> FingerTree v (Chunk a)
    r !tr []     = tr
    r !tr (t:[]) = tr |- mkChunk S.length t
    r !tr (t:ts) = let r' = tr |- mkChunk (const n) t
                   in r r' ts

-- | Converts a 'TX.Text' into a 'YiString' using
-- 'defaultChunkSize'-sized chunks for the underlying tree.
toBraid :: (ValidBraid v a) => a -> Braid v a
toBraid = toBraid' defaultChunkSize

-- | Consider whether you really need to use this!
extractBraid :: forall v a. (ValidBraid v a) => Braid v a -> a
extractBraid = S.concat . go . fromBraid
  where
    go :: FingerTree v (Chunk a) -> [a]
    go t = case viewl t of
      Chunk _ !c :< cs -> c : go cs
      EmptyL -> []

-- | Spits out the underlying string, reversed.
--
-- Note that this is actually slightly faster than manually unrolling
-- the tree from the end, 'TX.reverse'ing each chunk and
-- 'TX.concat'ing, at least with -O2 which you really need to be using
-- with 'TX.Text' anyway.
toReversed :: (ValidBraid v a) => Braid v a -> a
toReversed = S.reverse . extractBraid

-- | Checks if the given 'YiString' is actually empty.
null :: (ValidBraid v a) => Braid v a -> Bool
null = T.null . fromBraid

-- | Creates an empty 'YiString'.
empty :: (ValidBraid v a) => Braid v a
empty = Braid T.empty

-- | Length of the whole underlying string.
--
-- Amortized constant time.
length :: (ValidBraid v a) => Braid v a -> Int
length = getSize . measure . fromBraid

-- | Append two 'YiString's.
--
-- We take the extra time to optimise this append for many small
-- insertions. With naive append of the inner fingertree with 'T.><',
-- it is often the case that we end up with a large collection of tiny
-- chunks. This function instead tries to join the underlying trees at
-- outermost chunks up to 'defaultChunkSize' which while slower,
-- should improve memory usage.
--
-- I suspect that this pays for itself as we'd spend more time
-- computing over all the little chunks than few large ones anyway.
append :: (ValidBraid v a) => Braid v a -> Braid v a -> Braid v a
append (Braid t) (Braid t') = case (viewr t, viewl t') of
  (EmptyR, _) -> Braid t'
  (_, EmptyL) -> Braid t
  (ts :> Chunk l x, Chunk l' x' :< ts') ->
    let len = l + l' in case compare len defaultChunkSize of
      GT -> Braid (t <> t')
      _ -> Braid (ts |- Chunk len (x <> x') <> ts')

-- | Concat a list of 'YiString's.
concat :: (ValidBraid v a) => [Braid v a] -> Braid v a
concat = L.foldl' append empty

-- | Take the first character of the underlying string if possible.
head :: (ValidBraid v a) => Braid v a -> Maybe (S.Segment a)
head (Braid t) = case viewl t of
  EmptyL -> Nothing
  Chunk _ x :< _ -> if S.null x then Nothing else Just (S.head x)

-- | Take the last character of the underlying string if possible.
last :: (ValidBraid v a) => Braid v a -> Maybe (S.Segment a)
last (Braid t) = case viewr t of
  EmptyR -> Nothing
  _ :> Chunk _ x -> if S.null x then Nothing else Just (S.last x)

-- | Takes every character but the last one: returns Nothing on empty
-- string.
init :: (ValidBraid v a) => Braid v a -> Maybe (Braid v a)
init (Braid t) = case viewr t of
  EmptyR -> Nothing
  ts :> Chunk 0 _ -> Yi.Braid.init (Braid ts)
  ts :> Chunk l x -> Just . Braid $ ts |- Chunk (l - 1) (S.init x)

-- | Takes the tail of the underlying string. If the string is empty
-- to begin with, returns Nothing.
tail :: (ValidBraid v a) => Braid v a -> Maybe (Braid v a)
tail (Braid t) = case viewl t of
  EmptyL -> Nothing
  Chunk 0 _ :< ts -> Yi.Braid.tail (Braid ts)
  Chunk l x :< ts -> Just . Braid $ Chunk (l - 1) (S.tail x) -| ts

-- | Splits the string at given character position.
--
-- If @position <= 0@ then the left string is empty and the right string
-- contains everything else.
--
-- If @position >= length of the string@ then the left string contains
-- everything and the right string is empty.
--
-- Implementation note: the way this works is by splitting the
-- underlying finger at a closest chunk that goes *over* the given
-- position (see 'T.split'). This either results in a perfect split at
-- which point we're done or more commonly, it leaves as few
-- characters short and we need to take few characters from the first
-- chunk of the right side of the split. We do precisely that.
--
-- All together, this split is only as expensive as underlying
-- 'T.split', the cost of splitting a chunk into two, the cost of one
-- cons and one cons of a chunk and lastly the cost of 'T.splitAt' of
-- the underlying 'TX.Text'. It turns out to be fairly fast all
-- together.
splitAt :: (ValidBraid v a) => Int -> Braid v a -> (Braid v a, Braid v a)
splitAt n (Braid t)
  | n <= 0 = (mempty, Braid t)
  | otherwise = case viewl s of
    Chunk l x :< ts | n' /= 0 ->
      let (lx, rx) = S.splitAt n' x
      in (Braid $ f |> Chunk n' lx,
          Braid $ Chunk (l - n') rx -| ts)
    _ -> (Braid f, Braid s)
  where
    (f, s) = T.split ((> n) . getSize) t
    n' = n - getSize (measure f)

-- | Takes the first n given characters.
take :: (ValidBraid v a) => Int -> Braid v a -> Braid v a
take 1 = maybe mempty Yi.Braid.singleton . Yi.Braid.head
take n = fst . Yi.Braid.splitAt n

-- | Drops the first n characters.
drop :: (ValidBraid v a) => Int -> Braid v a -> Braid v a
drop 1 = fromMaybe mempty . Yi.Braid.tail
drop n = snd . Yi.Braid.splitAt n

-- | The usual 'Prelude.dropWhile' optimised for 'YiString's.
dropWhile :: (ValidBraid v a) => (S.Segment a -> Bool) -> Braid v a -> Braid v a
dropWhile p = Braid . go . fromBraid
  where
    go t = case viewl t of
      EmptyL -> T.empty
      Chunk 0 _ :< ts -> go ts
      Chunk l x :< ts ->
        let r = S.dropWhile p x
            l' = S.length r
        in case compare l' l of
          -- We dropped nothing so we must be done.
          EQ -> t
          -- We dropped something, if it was everything then drop from
          -- next chunk.
          LT | S.null r -> go ts
          -- It wasn't everything and we have left-overs, we must be done.
             | otherwise -> Chunk l' r <| ts
          -- We shouldn't really get here or it would mean that
          -- dropping stuff resulted in more content than we had. This
          -- can only happen if unsafe functions don't preserve the
          -- chunk size and it goes out of sync with the text length.
          -- Preserve this abomination, it may be useful for
          -- debugging.
          _ -> Chunk l' r -| ts

-- | As 'Yi.Braid.dropWhile' but drops from the end instead.
dropWhileEnd :: (ValidBraid v a) => (S.Segment a -> Bool) -> Braid v a -> Braid v a
dropWhileEnd p = Braid . go . fromBraid
  where
    go t = case viewr t of
      EmptyR -> T.empty
      ts :> Chunk 0 _ -> go ts
      ts :> Chunk l x ->
        let r = S.dropWhileEnd p x
            l' = S.length r
        in case compare l' l of
          EQ -> t
          LT | S.null r -> go ts
             | otherwise -> ts |> Chunk l' r
          _ -> ts |- Chunk l' r

-- | The usual 'Prelude.takeWhile' optimised for 'YiString's.
takeWhile :: (ValidBraid v a) => (S.Segment a -> Bool) -> Braid v a -> Braid v a
takeWhile p = Braid . go . fromBraid
  where
    go t = case viewl t of
      EmptyL -> T.empty
      Chunk 0 _ :< ts -> go ts
      Chunk l x :< ts ->
        let r = S.takeWhile p x
            l' = S.length r
        in case compare l' l of
          -- We took the whole chunk, keep taking more.
          EQ -> Chunk l x -| go ts
          -- We took some stuff but not everything so we're done.
          -- Alternatively, we took more than the size chunk so
          -- preserve this wonder. This should only ever happen if you
          -- use unsafe functions and Chunk size goes out of sync with
          -- actual text length.
          _ -> T.singleton $ Chunk l' r

-- | Like 'Yi.Braid.takeWhile' but takes from the end instead.
takeWhileEnd :: (ValidBraid v a) => (S.Segment a -> Bool) -> Braid v a -> Braid v a
takeWhileEnd p = Braid . go . fromBraid
  where
    go t = case viewr t of
      EmptyR -> T.empty
      ts :> Chunk 0 _ -> go ts
      ts :> Chunk l x -> case compare l' l of
        EQ -> go ts |> Chunk l x
        _ -> T.singleton $ Chunk l' r
        where
          -- no TX.takeWhileEnd – https://github.com/bos/text/issues/89
          r = S.reverse . S.takeWhile p . S.reverse $ x
          l' = S.length r


-- | Returns a pair whose first element is the longest prefix
-- (possibly empty) of t of elements that satisfy p, and whose second
-- is the remainder of the string. See also 'TX.span'.
--
-- This implementation uses 'Yi.Braid.splitAt' which actually is just
-- as fast as hand-unrolling the tree. GHC sure is great!
span :: (ValidBraid v a) => (S.Segment a -> Bool) -> Braid v a -> (Braid v a, Braid v a)
span p y = let x = Yi.Braid.takeWhile p y
           in case Yi.Braid.splitAt (Yi.Braid.length x) y of
             -- Re-using ‘x’ seems to gain us a minor performance
             -- boost.
             (_, y') -> (x, y')

-- | Just like 'Yi.Braid.span' but with the predicate negated.
break :: (ValidBraid v a) => (S.Segment a -> Bool) -> Braid v a -> (Braid v a, Braid v a)
break p = Yi.Braid.span (not . p)

-- | Concatenates the list of 'YiString's after inserting the
-- user-provided 'YiString' between the elements.
--
-- Empty 'YiString's are not ignored and will end up as strings of
-- length 1. If you don't want this, it's up to you to pre-process the
-- list. Just as with 'Yi.Braid.intersperse', it is up to the user to
-- pre-process the list.
intercalate :: (ValidBraid v a) => Braid v a -> [Braid v a] -> Braid v a
intercalate _ [] = mempty
intercalate (Braid t') (Braid s:ss) = Braid $ go s ss
  where
    go !acc []                = acc
    go acc (Braid t : ts') = go (acc >< t' >< t) ts'

-- | Intersperses the given character between the 'YiString's. This is
-- useful when you have a bunch of strings you just want to separate
-- something with, comma or a dash. Note that it only inserts the
-- character between the elements.
--
-- What's more, the result is a single 'YiString'. You can easily
-- achieve a version that blindly inserts elements to the back by
-- mapping over the list instead of using this function.
--
-- You can think of it as a specialised version of
-- 'Yi.Braid.intercalate'. Note that what this does __not__ do is
-- intersperse characters into the underlying text, you should convert
-- and use 'TX.intersperse' for that instead.
intersperse :: (ValidBraid v a) => S.Segment a -> [Braid v a] -> Braid v a
intersperse _ [] = mempty
intersperse c (t:ts) = go t ts
  where
    go !acc [] = acc
    go acc (t':ts') = go (acc <> (c `cons` t')) ts'

-- | Add a 'Char' in front of a 'YiString'.
cons :: (ValidBraid v a) => S.Segment a -> Braid v a -> Braid v a
cons c (Braid t) = case viewl t of
  EmptyL -> Yi.Braid.singleton c
  Chunk l x :< ts | l < defaultChunkSize -> Braid $ Chunk (l + 1) (c `S.cons` x) <| ts
  _ -> Braid $ Chunk 1 (S.singleton c) <| t

-- | Add a 'Char' in the back of a 'YiString'.
snoc :: (ValidBraid v a) => Braid v a -> S.Segment a -> Braid v a
snoc (Braid t) c = case viewr t of
  EmptyR -> Yi.Braid.singleton c
  ts :> Chunk l x | l < defaultChunkSize -> Braid $ ts |> Chunk (l + 1) (x `S.snoc` c)
  _ -> Braid $ t |> Chunk 1 (S.singleton c)

-- | Single character 'YiString'. Consider whether it's worth creating
-- this, maybe you can use 'cons' or 'snoc' instead?
singleton :: (ValidBraid v a) => S.Segment a -> Braid v a
singleton c = Braid . T.singleton $ Chunk 1 (S.singleton c)

-- | 'YiString' specialised @any@.
--
-- Implementation note: this currently just does any by doing ‘TX.Text’
-- conversions upon consecutive chunks. We should be able to speed it
-- up by running it in parallel over multiple chunks.
any :: (ValidBraid v a) => (S.Segment a -> Bool) -> Braid v a -> Bool
any p = go . fromBraid
  where
    go x = case viewl x of
      EmptyL -> False
      Chunk _ t :< ts -> S.any p t || go ts

-- | 'YiString' specialised @all@.
--
-- See the implementation note for 'Yi.Braid.any'.
all :: (ValidBraid v a) => (S.Segment a -> Bool) -> Braid v a -> Bool
all p = go . fromBraid
  where
    go x = case viewl x of
      EmptyL -> True
      Chunk _ t :< ts -> S.all p t && go ts

-- | Filters the characters from the underlying string.
--
-- >>> filter (/= 'a') "bac"
-- "bc"
filter :: (ValidBraid v a) => (S.Segment a -> Bool) -> Braid v a -> Braid v a
filter p = Braid . go . fromBraid
  where
    go t = case viewl t of
      EmptyL -> T.empty
      Chunk _ x :< ts -> mkChunk S.length (S.filter p x) -| go ts

-- | Maps the characters over the underlying string.
map :: (ValidBraid v a) => (S.Segment a -> S.Segment a) -> Braid v a -> Braid v a
map f = Braid . go . fromBraid
  where
    go t = case viewl t of
      EmptyL -> T.empty
      Chunk l x :< ts -> Chunk l (S.map f x) <| go ts

-- | Splits the 'YiString' on characters matching the predicate, like
-- 'TX.split'.
--
-- For splitting on newlines use 'Yi.Braid.lines' or 'Yi.Braid.lines''
-- instead.
--
-- Implementation note: GHC actually makes this naive implementation
-- about as fast and in cases with lots of splits, faster, as a
-- hand-rolled version on chunks with appends which is quite amazing
-- in itself.
split :: (ValidBraid v a) => (S.Segment a -> Bool) -> Braid v a -> [Braid v a]
split p = fmap toBraid . S.split p . extractBraid

-- | Left fold.
--
-- Benchmarks show that folding is actually Pretty Damn Slow™: consider
-- whether folding is really the best thing to use in your scenario.
foldl' :: (ValidBraid v a) => (b -> S.Segment a -> b) -> b -> Braid v a -> b
foldl' f a = go a . fromBraid
  where
    go acc t = case viewl t of
      EmptyL -> acc
      Chunk _ x :< ts -> let r = S.foldl f acc x
                            in r `seq` go r ts

-- | Replicate the given YiString set number of times, concatenating
-- the results. Also see 'Yi.Braid.replicateChar'.
replicate :: (ValidBraid v a) => Int -> Braid v a -> Braid v a
replicate n t | n <= 0 = mempty
              | otherwise = t <> Yi.Braid.replicate (n - 1) t

-- | Replicate the given character set number of times and pack the
-- result into a 'YiString'.
--
-- >>> replicateChar 4 ' '
-- "    "
replicateSegment :: (ValidBraid v a) => Int -> S.Segment a -> Braid v a
replicateSegment n = toBraid . S.replicate n . S.singleton

-- | Helper function doing conversions of to and from underlying
-- 'TX.Text'. You should aim to implement everything in terms of
-- 'YiString' instead.
--
-- Please note that this maps over each __chunk__ so this can only be
-- used with layout-agnostic functions. For example
--
-- >>> let t = 'fromString' "abc" <> 'fromString' "def"
-- >>> 'toString' $ 'withText' 'TX.reverse' t
-- "cbafed"
--
-- Probably doesn't do what you wanted, but 'TX.toUpper' would.
-- Specifically, for any @f : 'TX.Text' → 'TX.Text'@, 'withText' will
-- only do the ‘expected’ thing iff
--
-- @f x <> f y ≡ f (x <> y)@
--
-- which should look very familiar.
withChunk :: (ValidBraid v a) => (a -> a) -> Braid v a -> Braid v a
withChunk f = Braid . T.fmap' (mkChunk S.length . f . _fromChunk) . fromBraid

-- | Maps over each __chunk__ which means this function is UNSAFE! If
-- you use this with functions which don't preserve 'Size', that is
-- the chunk length and number of newlines, things will break really,
-- really badly. You should not need to use this.
--
-- Also see 'T.unsafeFmap'
unsafeWithChunk :: (ValidBraid v a) => (a -> a) -> Braid v a -> Braid v a
unsafeWithChunk f = Braid . T.unsafeFmap g . fromBraid
  where
    g (Chunk l t) = Chunk l (f t)
