{-# language BangPatterns #-}
{-# language DeriveDataTypeable #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language ViewPatterns #-}
{-# language FlexibleContexts #-}
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
-- This module defines a @Braid@ data structure. This is distinct from a 
-- traditional @Rope@ in that it abstracts over the underlying segmentable
-- type. It uses FingerTree's for efficiency and depends on the underlying
-- type's own definitions of common segmentation operations.

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
  , Yi.Braid.fmap'
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

import qualified Data.ListLike as LL
import qualified Yi.Rope.Internal.ListLikeHelpers as LL

-- | A @'Braid' v a s@ is a 'FingerTree' over some underlying type @a@ with
-- a 'measure' of @v@. The underlying type @a@ is referred to a @chain@ which
-- can be split into smaller segments of type @s@.
newtype Braid v a = Braid { fromBraid :: T.FingerTree v (Chunk a) }
  deriving (Show, Typeable)

-- | @ValidBraid :: * -> * -> Constraint@
-- 
-- ValidBraid is a constraint which ensures that the values used in the Braid
-- are valid for use with FingerTrees and can be segmented.
type ValidBraid v s a = (T.Measured v (Chunk a), HasSize v, LL.ListLike a s)

instance (ValidBraid v s a) => Monoid (Braid v a) where
  mempty = Yi.Braid.empty
  mappend = Yi.Braid.append
  mconcat = Yi.Braid.concat

class HasSize a where
  getSize :: a -> Int


-- | Makes a chunk from a given chain. We allow for an arbitrary
-- length function here to allow us to bypass calculating it manually.
-- for cases where the length is known ahead of time. An example would be:
--
-- > mkChunk 'Data.Text.length' someText
mkChunk :: (a -> Int) -- ^ The length function to use.
        -> a
        -> Chunk a
mkChunk l t = Chunk (l t) t

-- | Transform the chain inside the chunk. It's vital that the transformation
-- preserves the length of the content.
overChunk :: (a -> a) -- ^ Length-preserving content transformation.
          -> Chunk a -> Chunk a
overChunk f (Chunk l t) = Chunk l (f t)

-- | Two 'Braid's are equal if their underlying content is.
--
-- Implementation note: This uses 'extractBraid' and relies on the chain's 
-- underlying equality check, thus it's relatively inefficient and should
-- be avoided if possible. We could unroll the trees and mess around with
-- matching prefixes but the overhead would be higher than a simple
-- conversion and relying on GHC optimisation.
--
-- The derived Eq implementation for the underlying tree only passes
-- the equality check if the chunks are the same too which is not what
-- we want.
instance (ValidBraid v s a, Eq a) => Eq (Braid v a) where
  t == t' = Yi.Braid.length t == Yi.Braid.length t' && extractBraid t == extractBraid t'

-- | Ord is checked by using 'extractBraid' and using the underlying chain's
-- Ord instance.
instance (Eq a, Ord a, ValidBraid v s a) => Ord (Braid v a) where
  compare x y = extractBraid x `compare` extractBraid y

-- | A chunk caches the length of the underlying chain since computing the
-- length of the underlying chain type is not guaranteed to be efficient.
data Chunk a = Chunk { chunkSize :: {-# UNPACK #-} !Int
                     , _fromChunk :: !a
                     } deriving (Show, Eq, Typeable)

instance (NFData a) => NFData (Chunk a) where
  rnf (Chunk !i !t) = i `seq` rnf t

instance (NFData a, ValidBraid v s a) => NFData (Braid v a) where
  rnf = rnf . extractBraid

-- | Prepend a 'Chunk' onto a 'FingerTree'
(-|) :: (ValidBraid v s a) => Chunk a -> FingerTree v (Chunk a) -> FingerTree v (Chunk a)
b -| t | chunkSize b == 0 = t
       | otherwise        = b <| t

-- | Append a 'Chunk' onto a 'FingerTree'
(|-) :: (ValidBraid v s a) => FingerTree v (Chunk a) -> Chunk a -> FingerTree v (Chunk a)
t |- b | chunkSize b == 0 = t
       | otherwise        = t |> b

-- | Default size chunk to use. Currently @1200@ as this is what
-- benchmarks suggest.
--
-- For 'YiString' and similar chain types
-- this makes the biggest difference with 'lines'-like and
-- 'concat'-like functions. Bigger chunks make 'concat' (much) faster
-- but 'lines' slower. In general it seems that we benefit more from
-- larger chunks and 1200 seems to be the sweet spot.
defaultChunkSize :: Int
defaultChunkSize = 1200

-- | Reverse the whole underlying chain
--
-- This involves reversing the order of the chunks as well as content
-- of the chunks. We use a little optimisation here that re-uses the
-- content of each chunk but this exposes a potential problem: after
-- many transformations, our chunks size might become quite varied
-- (but never more than the default size), perhaps we should
-- periodically rechunk the tree to recover nice sizes?
reverse :: (ValidBraid v s a) => Braid v a -> Braid v a
reverse = Braid . T.fmap' (overChunk LL.reverse) . T.reverse . fromBraid

-- | This is like 'toBraid' but it allows the user to specify the
-- chunk size to be used. Uses 'defaultChunkSize' if the given
-- size is <= 0.
toBraid' :: forall v a s. (ValidBraid v s a) => Int -> a -> Braid v a
toBraid' n | n <= 0 = toBraid' defaultChunkSize
           | otherwise = Braid . r T.empty . f
  where
    f = LL.chunksOf n

    -- Convert the given chain into chunks in the tree. We have a
    -- special case for a single element case: because we split on
    -- predetermined chunk size, we know that all chunks but the last
    -- one will be the specified size so we can optimise here instead
    -- of having to recompute chunk size at creation.
    r :: FingerTree v (Chunk a) -> [a] -> FingerTree v (Chunk a)
    r !tr []     = tr
    r !tr (t:[]) = tr |- mkChunk LL.length t
    r !tr (t:ts) = let r' = tr |- mkChunk (const n) t
                   in r r' ts

-- | Converts a chain of arbitrary type into a 'Braid' using
-- 'defaultChunkSize'-sized chunks for the underlying tree.
toBraid :: (ValidBraid v s a) => a -> Braid v a
toBraid = toBraid' defaultChunkSize

-- | Extracts the entire underlying chain by deconstructing the tree.
-- Consider whether you really need to use this, it's very inefficient.
extractBraid :: forall v a s. (ValidBraid v s a) => Braid v a -> a
extractBraid = LL.concat . go . fromBraid
  where
    go :: FingerTree v (Chunk a) -> [a]
    go t = case viewl t of
      Chunk _ !c :< cs -> c : go cs
      EmptyL -> []

-- | Spits out the underlying chain, reversed.
--
-- Note that this is actually slightly faster than manually unrolling
-- the tree from the end, reversing each chunk and
-- concating, at least with -O2 which you really should be using anyway.
toReversed :: (ValidBraid v s a) => Braid v a -> a
toReversed = LL.reverse . extractBraid

-- | Checks if the given 'Braid' is empty.
null :: (ValidBraid v s a) => Braid v a -> Bool
null = T.null . fromBraid

-- | Creates an empty 'Braid'
empty :: (ValidBraid v s a) => Braid v a
empty = Braid T.empty

-- | Length of the whole underlying chain.
--
-- Amortized constant time.
length :: (ValidBraid v s a) => Braid v a -> Int
length = getSize . measure . fromBraid

-- | Append two 'Braid's.
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
append :: (ValidBraid v s a) => Braid v a -> Braid v a -> Braid v a
append (Braid t) (Braid t') = case (viewr t, viewl t') of
  (EmptyR, _) -> Braid t'
  (_, EmptyL) -> Braid t
  (ts :> Chunk l x, Chunk l' x' :< ts') ->
    let len = l + l' in case compare len defaultChunkSize of
      GT -> Braid (t <> t')
      _ -> Braid (ts |- Chunk len (x <> x') <> ts')

-- | Concat a list of 'Braid's.
concat :: (ValidBraid v s a) => [Braid v a] -> Braid v a
concat = L.foldl' append empty

-- | Take the first segment of the underlying chain if possible.
head :: (ValidBraid v s a) => Braid v a -> Maybe s
head (Braid t) = case viewl t of
  EmptyL -> Nothing
  Chunk _ x :< _ -> if LL.null x then Nothing else Just (LL.head x)

-- | Take the last segment of the underlying chain if possible.
last :: (ValidBraid v s a) => Braid v a -> Maybe s
last (Braid t) = case viewr t of
  EmptyR -> Nothing
  _ :> Chunk _ x -> if LL.null x then Nothing else Just (LL.last x)

-- | Takes every segment but the last one: returns Nothing on empty
-- string.
init :: (ValidBraid v s a) => Braid v a -> Maybe (Braid v a)
init (Braid t) = case viewr t of
  EmptyR -> Nothing
  ts :> Chunk 0 _ -> Yi.Braid.init (Braid ts)
  ts :> Chunk l x -> Just . Braid $ ts |- Chunk (l - 1) (LL.init x)

-- | Takes the tail of the underlying chain. If the string is empty
-- to begin with, returns Nothing.
tail :: (ValidBraid v s a) => Braid v a -> Maybe (Braid v a)
tail (Braid t) = case viewl t of
  EmptyL -> Nothing
  Chunk 0 _ :< ts -> Yi.Braid.tail (Braid ts)
  Chunk l x :< ts -> Just . Braid $ Chunk (l - 1) (LL.tail x) -| ts

-- | Splits the 'Braid' at given number of segments.
--
-- If @position <= 0@ then the left 'Braid' is empty and the right string
-- contains everything else.
--
-- If @position >= length of the 'Braid'@ then the left 'Braid' contains
-- everything and the right 'Braid' is empty.
--
-- Implementation note: the way this works is by splitting the
-- underlying finger at a closest chunk that goes *over* the given
-- position (see 'T.split'). This either results in a perfect split at
-- which point we're done or more commonly, it leaves as few
-- segments short and we need to take few segments from the first
-- chunk of the right side of the split. We do precisely that.
--
-- All together, this split is only as expensive as underlying
-- 'T.split', the cost of splitting a chunk into two, the cost of one
-- cons and one cons of a chunk and lastly the cost of 'T.splitAt' of
-- the underlying chain type. It turns out to be fairly fast all
-- together.
splitAt :: (ValidBraid v s a) => Int -> Braid v a -> (Braid v a, Braid v a)
splitAt n (Braid t)
  | n <= 0 = (mempty, Braid t)
  | otherwise = case viewl s of
    Chunk l x :< ts | n' /= 0 ->
      let (lx, rx) = LL.splitAt n' x
      in (Braid $ f |> Chunk n' lx,
          Braid $ Chunk (l - n') rx -| ts)
    _ -> (Braid f, Braid s)
  where
    (f, s) = T.split ((> n) . getSize) t
    n' = n - getSize (measure f)

-- | Takes the first n given segments
take :: (ValidBraid v s a) => Int -> Braid v a -> Braid v a
take 1 = maybe mempty Yi.Braid.singleton . Yi.Braid.head
take n = fst . Yi.Braid.splitAt n

-- | Drops the first n segments.
drop :: (ValidBraid v s a) => Int -> Braid v a -> Braid v a
drop 1 = fromMaybe mempty . Yi.Braid.tail
drop n = snd . Yi.Braid.splitAt n

-- | The usual 'Prelude.dropWhile' optimised for 'Braid's.
dropWhile :: (ValidBraid v s a) => (s -> Bool) -> Braid v a -> Braid v a
dropWhile p = Braid . go . fromBraid
  where
    go t = case viewl t of
      EmptyL -> T.empty
      Chunk 0 _ :< ts -> go ts
      Chunk l x :< ts ->
        let r = LL.dropWhile p x
            l' = LL.length r
        in case compare l' l of
          -- We dropped nothing so we must be done.
          EQ -> t
          -- We dropped something, if it was everything then drop from
          -- next chunk.
          LT | LL.null r -> go ts
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
dropWhileEnd :: (ValidBraid v s a) => (s -> Bool) -> Braid v a -> Braid v a
dropWhileEnd p = Braid . go . fromBraid
  where
    go t = case viewr t of
      EmptyR -> T.empty
      ts :> Chunk 0 _ -> go ts
      ts :> Chunk l x ->
        let r = LL.dropWhileEnd p x
            l' = LL.length r
        in case compare l' l of
          EQ -> t
          LT | LL.null r -> go ts
             | otherwise -> ts |> Chunk l' r
          _ -> ts |- Chunk l' r

-- | The usual 'Prelude.takeWhile' optimised for 'Braid's.
takeWhile :: (ValidBraid v s a) => (s -> Bool) -> Braid v a -> Braid v a
takeWhile p = Braid . go . fromBraid
  where
    go t = case viewl t of
      EmptyL -> T.empty
      Chunk 0 _ :< ts -> go ts
      Chunk l x :< ts ->
        let r = LL.takeWhile p x
            l' = LL.length r
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
takeWhileEnd :: (ValidBraid v s a) => (s -> Bool) -> Braid v a -> Braid v a
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
          r = LL.reverse . LL.takeWhile p . LL.reverse $ x
          l' = LL.length r


-- | Returns a pair whose first element is the longest prefix
-- (possibly empty) of t of elements that satisfy p, and whose second
-- is the remainder of the string. See also 'Data.Text.span'.
--
-- This implementation uses 'Yi.Braid.splitAt' which actually is just
-- as fast as hand-unrolling the tree. GHC sure is great!
span :: (ValidBraid v s a) => (s -> Bool) -> Braid v a -> (Braid v a, Braid v a)
span p y = let x = Yi.Braid.takeWhile p y
           in case Yi.Braid.splitAt (Yi.Braid.length x) y of
             -- Re-using ‘x’ seems to gain us a minor performance
             -- boost.
             (_, y') -> (x, y')

-- | Just like 'Yi.Braid.span' but with the predicate negated.
break :: (ValidBraid v s a) => (s -> Bool) -> Braid v a -> (Braid v a, Braid v a)
break p = Yi.Braid.span (not . p)

-- | Concatenates the list of 'Braid's after inserting the
-- user-provided 'Braid' between the elements.
--
-- Empty 'Braid's are not ignored and will end up as 'Braid's of
-- length 1. If you don't want this, it's up to you to pre-process the
-- list. Just as with 'Yi.Braid.intersperse', it is up to the user to
-- pre-process the list.
intercalate :: (ValidBraid v s a) => Braid v a -> [Braid v a] -> Braid v a
intercalate _ [] = mempty
intercalate (Braid t') (Braid s:ss) = Braid $ go s ss
  where
    go !acc []                = acc
    go acc (Braid t : ts') = go (acc >< t' >< t) ts'

-- | Intersperses the given segment between the 'Braid's. This is
-- useful when you have a bunch of 'Braid's you just want to separate
-- with something.
--
-- What's more, the result is a single 'Braid'. You can easily
-- achieve a version that blindly inserts elements to the back by
-- mapping over the list instead of using this function.
--
-- You can think of it as a specialised version of
-- 'Yi.Braid.intercalate'. Note that what this does __not__ do is
-- intersperse segments into the underlying chain, you should convert
-- and use your type's underlying intersperse for that instead.
intersperse :: (ValidBraid v s a) => s -> [Braid v a] -> Braid v a
intersperse _ [] = mempty
intersperse c (t:ts) = go t ts
  where
    go !acc [] = acc
    go acc (t':ts') = go (acc <> (c `cons` t')) ts'

-- | Add a segment in front of a 'Braid'.
cons :: (ValidBraid v s a) => s -> Braid v a -> Braid v a
cons c (Braid t) = case viewl t of
  EmptyL -> Yi.Braid.singleton c
  Chunk l x :< ts | l < defaultChunkSize -> Braid $ Chunk (l + 1) (c `LL.cons` x) <| ts
  _ -> Braid $ Chunk 1 (LL.singleton c) <| t

-- | Add a segment in the back of a 'Braid'.
snoc :: (ValidBraid v s a) => Braid v a -> s -> Braid v a
snoc (Braid t) c = case viewr t of
  EmptyR -> Yi.Braid.singleton c
  ts :> Chunk l x | l < defaultChunkSize -> Braid $ ts |> Chunk (l + 1) (x `LL.snoc` c)
  _ -> Braid $ t |> Chunk 1 (LL.singleton c)

-- | Turn a single segment into a 'Braid'. 
-- Consider whether it's worth creating
-- this, maybe you can use 'cons' or 'snoc' instead?
singleton :: (ValidBraid v s a) => s -> Braid v a
singleton c = Braid . T.singleton $ Chunk 1 (LL.singleton c)

-- | @any@ specialised to 'Braid'
--
-- Implementation note: this currently just does any by doing 'S.any'
-- on underlying chunks. We should be able to speed it
-- up by running it in parallel over multiple chunks.
any :: (ValidBraid v s a) => (s -> Bool) -> Braid v a -> Bool
any p = go . fromBraid
  where
    go x = case viewl x of
      EmptyL -> False
      Chunk _ t :< ts -> LL.any p t || go ts

-- | @all@ specialised to 'Braid'
--
-- See the implementation note for 'Yi.Braid.any'.
all :: (ValidBraid v s a) => (s -> Bool) -> Braid v a -> Bool
all p = go . fromBraid
  where
    go x = case viewl x of
      EmptyL -> True
      Chunk _ t :< ts -> LL.all p t && go ts

-- | Filters the segments from the underlying chain
--
-- >>> filter (/= 'a') "bac"
-- "bc"
filter :: (ValidBraid v s a) => (s -> Bool) -> Braid v a -> Braid v a
filter p = Braid . go . fromBraid
  where
    go t = case viewl t of
      EmptyL -> T.empty
      Chunk _ x :< ts -> mkChunk LL.length (LL.filter p x) -| go ts

-- | Maps the segments of the underlying chain.
map :: (ValidBraid v s a, ValidBraid q t b) => (s -> t) -> Braid v a -> Braid q b
map f = Braid . go . fromBraid
  where
    go t = case viewl t of
      EmptyL -> T.empty
      Chunk l x :< ts -> Chunk l (LL.map f x) <| go ts

-- | Splits the 'Braid' on characters matching the predicate
--
-- Implementation note: GHC actually makes this naive implementation
-- about as fast and in cases with lots of splits, faster, as a
-- hand-rolled version on chunks with appends which is quite amazing
-- in itself.
split :: (ValidBraid v s a) => (s -> Bool) -> Braid v a -> [Braid v a]
split p = fmap toBraid . LL.split p . extractBraid

-- | Left fold.
--
-- Benchmarks show that folding is actually Pretty Damn Slow™: consider
-- whether folding is really the best thing to use in your scenario.
foldl' :: (ValidBraid v s a) => (b -> s -> b) -> b -> Braid v a -> b
foldl' f a = go a . fromBraid
  where
    go acc t = case viewl t of
      EmptyL -> acc
      Chunk _ x :< ts -> let r = LL.foldl f acc x
                            in r `seq` go r ts

-- | Replicate the given 'Braid' a set number of times, concatenating
-- the results. Also see 'Yi.Braid.replicateChar'.
replicate :: (ValidBraid v s a) => Int -> Braid v a -> Braid v a
replicate n t | n <= 0 = mempty
              | otherwise = t <> Yi.Braid.replicate (n - 1) t

-- | Replicate the given segment a set number of times and pack the
-- result into a 'Braid'.
replicateSegment :: (ValidBraid v s a) => Int -> s -> Braid v a
replicateSegment n = toBraid . LL.replicate n

-- Please note that this maps over each __chunk__ so this can only be
-- used with layout-agnostic functions. For example
--
-- >>> let t = 'toBraid' "abc" <> 'toBraid' "def"
-- >>> 'extractBraid' $ 'fmap'' 'Data.Text.reverse' t
-- "cbafed"
--
-- If however your function is unaffected by this 'chunking' behaviour
-- you can tag or transform your underlying sequences or convert between
-- `Braid` types.
fmap' :: (ValidBraid v s a, ValidBraid q t b) => (a -> b) -> Braid v a -> Braid q b
fmap' f = Braid . T.fmap' (mkChunk LL.length . f . _fromChunk) . fromBraid

-- | Maps over each __chunk__ which means this function is UNSAFE! If
-- you use this with functions which don't preserve a Chunk's measure
-- things will break really, really badly. You should not need to use this.
--
-- Also see 'T.unsafeFmap'
unsafeWithChunk :: (ValidBraid v s a) => (a -> a) -> Braid v a -> Braid v a
unsafeWithChunk f = Braid . T.unsafeFmap g . fromBraid
  where
    g (Chunk l t) = Chunk l (f t)
