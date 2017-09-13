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
{-# language GADTs #-}
{-# language UndecidableInstances #-}
{-# options_haddock show-extensions #-}

-- |
-- Module      :  Yi.Rope
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

module Yi.Rope (
   Yi.Rope.YiString,

   -- * Conversions to YiString
   Yi.Rope.fromString, Yi.Rope.fromText,
   Yi.Rope.fromString', Yi.Rope.fromText',

   -- * Conversions from YiString
   Yi.Rope.toString, Yi.Rope.toReverseString,
   Yi.Rope.toText, Yi.Rope.toReverseText,

   -- * Functions over content
   Yi.Rope.null, Yi.Rope.empty, Yi.Rope.take, Yi.Rope.drop,
   Yi.Rope.length, Yi.Rope.reverse, Yi.Rope.countNewLines,
   Yi.Rope.lines, Yi.Rope.lines', Yi.Rope.unlines,
   Yi.Rope.splitAt, Yi.Rope.splitAtLine,
   Yi.Rope.cons, Yi.Rope.snoc, Yi.Rope.singleton,
   Yi.Rope.head, Yi.Rope.last,
   Yi.Rope.append, Yi.Rope.concat,
   Yi.Rope.any, Yi.Rope.all,
   Yi.Rope.dropWhile, Yi.Rope.takeWhile,
   Yi.Rope.dropWhileEnd, Yi.Rope.takeWhileEnd,
   Yi.Rope.intercalate, Yi.Rope.intersperse,
   Yi.Rope.filter, Yi.Rope.map,
   Yi.Rope.words, Yi.Rope.unwords,
   Yi.Rope.split, Yi.Rope.init, Yi.Rope.tail,
   Yi.Rope.span, Yi.Rope.break, Yi.Rope.foldl',
   Yi.Rope.replicate, Yi.Rope.replicateChar,

   -- * IO
   Yi.Rope.readFile, Yi.Rope.writeFile,

   -- * Escape latches to underlying content. Note that these are safe
   -- to use but it does not mean they should.
   Yi.Rope.fromRope, Yi.Rope.withText, Yi.Rope.unsafeWithText

  ) where

import           Control.DeepSeq
import           Control.Exception (try)
import           Data.Binary
import qualified Data.ByteString.Lazy as BSL
import           Data.Char (isSpace)
import qualified Data.FingerTree as T
import           Data.FingerTree hiding (null, empty, reverse, split)
import qualified Data.List as L (foldl')
import           Data.Maybe
import           Data.Monoid
import           Data.String (IsString(..))
import qualified Data.Text as TX
import qualified Data.Text.Encoding.Error as TXEE
import qualified Data.Text.Lazy as TXL
import qualified Data.Text.Lazy.Encoding as TXLE
import qualified Data.Text.IO as TXIO (writeFile)
import           Data.Typeable
import           Prelude hiding (drop)

import qualified Yi.Segment as S
import Yi.Segment (_fromChunk, chunkSize)

class MSeg t where
  type Meas t

instance MSeg TX.Text where
  type Meas TX.Text = Size


-- | Used to cache the size of the strings.
data Size = Indices { charIndex :: {-# UNPACK #-} !Int
                      -- ^ How many characters under here?
                    , lineIndex :: Int
                      -- ^ How many lines under here?
                    } deriving (Eq, Show, Typeable)

-- | Makes a chunk from a given string. We allow for an arbitrary
-- length function here to allow us to bypass the calculation with
-- 'const' in case the length is known ahead of time. In most cases,
-- the use of this is
--
-- > mkChunk 'TX.Text.length' someText
mkChunk :: (TX.Text -> Int) -- ^ The length function to use.
        -> TX.Text
        -> S.Chunk TX.Text
mkChunk l t = S.Chunk (l t) t

-- | Transform the chunk content. It's vital that the transformation
-- preserves the length of the content.
overChunk :: (TX.Text -> TX.Text) -- ^ Length-preserving content transformation.
          -> S.Chunk TX.Text -> S.Chunk TX.Text
overChunk f (S.Chunk l t) = S.Chunk l (f t)

-- | Counts number of newlines in the given 'TX.Text'.
countNl :: TX.Text -> Int
countNl = TX.count "\n"

instance Monoid Size where
  mempty = Indices 0 0
  Indices c l `mappend` Indices c' l' = Indices (c + c') (l + l')

instance Measured Size (S.Chunk TX.Text) where
  measure (S.Chunk l t) = Indices l (countNl t)



data Rope a where
  Rope :: (T.Measured (Meas a) (S.Chunk a)) => T.FingerTree (Meas a) (S.Chunk a) -> Rope a
  deriving (Typeable)

instance (Show a) => Show (Rope a) where
  show (Rope a) = "Rope " ++ show a

fromRope :: Rope a -> T.FingerTree (Meas a) (S.Chunk a)
fromRope (Rope a) = a

instance (T.Measured (Meas a) (S.Chunk a)) => Monoid (Rope a) where
  mempty = Rope mempty
  Rope a `mappend` Rope b = Rope (a `mappend` b)

instance Ord (Rope TX.Text) where
  compare x y = toText x `compare` toText y



-- | A 'YiString' is a 'FingerTree' with cached char and line counts
-- over chunks of 'TX.Text'.
newtype YiString = YiString { fromRope' :: FingerTree Size (S.Chunk TX.Text) }
                 deriving (Show, Typeable)

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
instance (Eq TX.Text) => Eq (Rope TX.Text) where
  t == t' = Yi.Rope.length t == Yi.Rope.length t' && toText t == toText t'

instance NFData Size where
  rnf (Indices !c !l) = c `seq` l `seq` ()

instance NFData (S.Chunk TX.Text) where
  rnf (S.Chunk !i !t) = i `seq` rnf t

instance NFData (Rope TX.Text) where
  rnf = rnf . toText

instance IsString (Rope TX.Text) where
  fromString = Yi.Rope.fromString

(-|) :: (S.Chunk TX.Text) -> FingerTree (Meas TX.Text) (S.Chunk TX.Text) -> FingerTree (Meas TX.Text) (S.Chunk TX.Text)
b -| t | chunkSize b == 0 = t
       | otherwise        = b <| t

(|-) :: FingerTree (Meas TX.Text) (S.Chunk TX.Text) -> (S.Chunk TX.Text) -> FingerTree (Meas TX.Text) (S.Chunk TX.Text)
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
reverse :: Rope TX.Text -> Rope TX.Text
reverse = Rope . fmap' (overChunk TX.reverse) . T.reverse . fromRope

-- | See 'fromText'.
fromString :: String -> Rope TX.Text
fromString = fromText . TX.pack

-- | See 'fromText''.
fromString' :: Int -> String -> Rope TX.Text
fromString' n = fromText' n . TX.pack

-- | See 'toText'.
toString :: Rope TX.Text -> String
toString = TX.unpack . toText

-- | See 'toReverseText'.
--
-- Note that it is actually ~4.5 times faster to use 'toReverseText'
-- and unpack the result than to convert to 'String' and use
-- 'Prelude.reverse'.
toReverseString :: Rope TX.Text -> String
toReverseString = TX.unpack . toReverseText

-- | This is like 'fromText' but it allows the user to specify the
-- chunk size to be used. Uses 'defaultChunkSize' if the given
-- size is <= 0.
fromText' :: Int -> TX.Text -> Rope TX.Text
fromText' n | n <= 0 = fromText' defaultChunkSize
            | otherwise = Rope . r T.empty . f
  where
    f = TX.chunksOf n

    -- Convert the given string into chunks in the tree. We have a
    -- special case for a single element case: because we split on
    -- predetermined chunk size, we know that all chunks but the last
    -- one will be the specified size so we can optimise here instead
    -- of having to recompute chunk size at creation.
    r :: FingerTree Size (S.Chunk TX.Text) -> [TX.Text] -> FingerTree Size (S.Chunk TX.Text)
    r !tr []     = tr
    r !tr (t:[]) = tr |- mkChunk TX.length t
    r !tr (t:ts) = let r' = tr |- mkChunk (const n) t
                   in r r' ts

-- | Converts a 'TX.Text' into a 'YiString' using
-- 'defaultChunkSize'-sized chunks for the underlying tree.
fromText :: TX.Text -> Rope TX.Text
fromText = fromText' defaultChunkSize

fromLazyText :: TXL.Text -> Rope TX.Text
fromLazyText = Rope . T.fromList . fmap (mkChunk TX.length) . TXL.toChunks

-- | Consider whether you really need to use this!
toText :: Rope TX.Text -> TX.Text
toText = TX.concat . go . fromRope
  where
    go :: FingerTree Size (S.Chunk TX.Text) -> [TX.Text]
    go t = case viewl t of
      S.Chunk _ !c :< cs -> c : go cs
      EmptyL -> []

-- | Spits out the underlying string, reversed.
--
-- Note that this is actually slightly faster than manually unrolling
-- the tree from the end, 'TX.reverse'ing each chunk and
-- 'TX.concat'ing, at least with -O2 which you really need to be using
-- with 'TX.Text' anyway.
toReverseText :: Rope TX.Text -> TX.Text
toReverseText = TX.reverse . toText

-- | Checks if the given 'YiString' is actually empty.
null :: Rope TX.Text -> Bool
null = T.null . fromRope

-- | Creates an empty 'YiString'.
empty :: Rope TX.Text
empty = Rope T.empty

-- | Length of the whole underlying string.
--
-- Amortized constant time.
length :: Rope TX.Text -> Int
length = charIndex . measure . fromRope

-- | Count the number of newlines in the underlying string. This is
-- actually amortized constant time as we cache this information in
-- the underlying tree.
countNewLines :: Rope TX.Text -> Int
countNewLines = lineIndex . measure . fromRope

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
append :: Rope TX.Text -> Rope TX.Text -> Rope TX.Text
append (Rope t) (Rope t') = case (viewr t, viewl t') of
  (EmptyR, _) -> Rope t'
  (_, EmptyL) -> Rope t
  (ts :> S.Chunk l x, S.Chunk l' x' :< ts') ->
    let len = l + l' in case compare len defaultChunkSize of
      GT -> Rope (t <> t')
      _ -> Rope (ts |- S.Chunk len (x <> x') <> ts')

-- | Concat a list of 'YiString's.
concat :: [Rope TX.Text] -> Rope TX.Text
concat = L.foldl' append empty

-- | Take the first character of the underlying string if possible.
head :: (S.Segmented a) => Rope a -> Maybe (S.Segment a)
head (Rope t) = case viewl t of
  EmptyL -> Nothing
  S.Chunk _ x :< _ -> if S.null x then Nothing else Just (S.head x)

-- | Take the last character of the underlying string if possible.
last :: Rope TX.Text -> Maybe Char
last (Rope t) = case viewr t of
  EmptyR -> Nothing
  _ :> S.Chunk _ x -> if TX.null x then Nothing else Just (TX.last x)

-- | Takes every character but the last one: returns Nothing on empty
-- string.
init :: Rope TX.Text -> Maybe (Rope TX.Text)
init (Rope t) = case viewr t of
  EmptyR -> Nothing
  ts :> S.Chunk 0 _ -> Yi.Rope.init (Rope ts)
  ts :> S.Chunk l x -> Just . Rope $ ts |- S.Chunk (l - 1) (TX.init x)

-- | Takes the tail of the underlying string. If the string is empty
-- to begin with, returns Nothing.
tail :: Rope TX.Text -> Maybe (Rope TX.Text)
tail (Rope t) = case viewl t of
  EmptyL -> Nothing
  S.Chunk 0 _ :< ts -> Yi.Rope.tail (Rope ts)
  S.Chunk l x :< ts -> Just . Rope $ S.Chunk (l - 1) (TX.tail x) -| ts

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
splitAt :: Int -> Rope TX.Text -> (Rope TX.Text, Rope TX.Text)
splitAt n (Rope t)
  | n <= 0 = (mempty, Rope t)
  | otherwise = case viewl s of
    S.Chunk l x :< ts | n' /= 0 ->
      let (lx, rx) = S.splitAt n' x
      in (Rope $ f |> S.Chunk n' lx,
          Rope $ S.Chunk (l - n') rx -| ts)
    _ -> (Rope f, Rope s)
  where
    (f, s) = T.split ((> n) . charIndex) t
    n' = n - charIndex (measure f)

-- | Takes the first n given characters.
take :: Int -> Rope TX.Text -> Rope TX.Text
take 1 = maybe mempty Yi.Rope.singleton . Yi.Rope.head
take n = fst . Yi.Rope.splitAt n

-- | Drops the first n characters.
drop :: Int -> Rope TX.Text -> Rope TX.Text
drop 1 = fromMaybe mempty . Yi.Rope.tail
drop n = snd . Yi.Rope.splitAt n

-- | The usual 'Prelude.dropWhile' optimised for 'YiString's.
dropWhile :: (S.Segment TX.Text -> Bool) -> Rope TX.Text -> Rope TX.Text
dropWhile p = Rope . go . fromRope
  where
    go t = case viewl t of
      EmptyL -> T.empty
      S.Chunk 0 _ :< ts -> go ts
      S.Chunk l x :< ts ->
        let r = TX.dropWhile p x
            l' = TX.length r
        in case compare l' l of
          -- We dropped nothing so we must be done.
          EQ -> t
          -- We dropped something, if it was everything then drop from
          -- next chunk.
          LT | TX.null r -> go ts
          -- It wasn't everything and we have left-overs, we must be done.
             | otherwise -> S.Chunk l' r <| ts
          -- We shouldn't really get here or it would mean that
          -- dropping stuff resulted in more content than we had. This
          -- can only happen if unsafe functions don't preserve the
          -- chunk size and it goes out of sync with the text length.
          -- Preserve this abomination, it may be useful for
          -- debugging.
          _ -> S.Chunk l' r -| ts

-- | As 'Yi.Rope.dropWhile' but drops from the end instead.
dropWhileEnd :: (Char -> Bool) -> Rope TX.Text -> Rope TX.Text
dropWhileEnd p = Rope . go . fromRope
  where
    go t = case viewr t of
      EmptyR -> T.empty
      ts :> S.Chunk 0 _ -> go ts
      ts :> S.Chunk l x ->
        let r = TX.dropWhileEnd p x
            l' = TX.length r
        in case compare l' l of
          EQ -> t
          LT | TX.null r -> go ts
             | otherwise -> ts |> S.Chunk l' r
          _ -> ts |- S.Chunk l' r

-- | The usual 'Prelude.takeWhile' optimised for 'YiString's.
takeWhile :: (S.Segment TX.Text -> Bool) -> Rope TX.Text -> Rope TX.Text
takeWhile p = Rope . go . fromRope
  where
    go t = case viewl t of
      EmptyL -> T.empty
      S.Chunk 0 _ :< ts -> go ts
      S.Chunk l x :< ts ->
        let r = S.takeWhile p x
            l' = S.length r
        in case compare l' l of
          -- We took the whole chunk, keep taking more.
          EQ -> S.Chunk l x -| go ts
          -- We took some stuff but not everything so we're done.
          -- Alternatively, we took more than the size chunk so
          -- preserve this wonder. This should only ever happen if you
          -- use unsafe functions and S.Chunk size goes out of sync with
          -- actual text length.
          _ -> T.singleton $ S.Chunk l' r

-- | Like 'Yi.Rope.takeWhile' but takes from the end instead.
takeWhileEnd :: (S.Segment TX.Text -> Bool) -> Rope TX.Text -> Rope TX.Text
takeWhileEnd p = Rope . go . fromRope
  where
    go t = case viewr t of
      EmptyR -> T.empty
      ts :> S.Chunk 0 _ -> go ts
      ts :> S.Chunk l x -> case compare l' l of
        EQ -> go ts |> S.Chunk l x
        _ -> T.singleton $ S.Chunk l' r
        where
          -- no TX.takeWhileEnd – https://github.com/bos/text/issues/89
          r = TX.reverse . TX.takeWhile p . TX.reverse $ x
          l' = S.length r


-- | Returns a pair whose first element is the longest prefix
-- (possibly empty) of t of elements that satisfy p, and whose second
-- is the remainder of the string. See also 'TX.span'.
--
-- This implementation uses 'Yi.Rope.splitAt' which actually is just
-- as fast as hand-unrolling the tree. GHC sure is great!
span :: (S.Segment TX.Text -> Bool) -> Rope TX.Text -> (Rope TX.Text, Rope TX.Text)
span p y = let x = Yi.Rope.takeWhile p y
           in case Yi.Rope.splitAt (Yi.Rope.length x) y of
             -- Re-using ‘x’ seems to gain us a minor performance
             -- boost.
             (_, y') -> (x, y')

-- | Just like 'Yi.Rope.span' but with the predicate negated.
break :: (S.Segment TX.Text -> Bool) -> Rope TX.Text -> (Rope TX.Text, Rope TX.Text)
break p = Yi.Rope.span (not . p)

-- | Concatenates the list of 'YiString's after inserting the
-- user-provided 'YiString' between the elements.
--
-- Empty 'YiString's are not ignored and will end up as strings of
-- length 1. If you don't want this, it's up to you to pre-process the
-- list. Just as with 'Yi.Rope.intersperse', it is up to the user to
-- pre-process the list.
intercalate :: Rope TX.Text -> [Rope TX.Text] -> Rope TX.Text
intercalate _ [] = mempty
intercalate (Rope t') (Rope s:ss) = Rope $ go s ss
  where
    go !acc []                = acc
    go acc (Rope t : ts') = go (acc >< t' >< t) ts'

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
-- 'Yi.Rope.intercalate'. Note that what this does __not__ do is
-- intersperse characters into the underlying text, you should convert
-- and use 'TX.intersperse' for that instead.
intersperse :: S.Segment TX.Text -> [Rope TX.Text] -> Rope TX.Text
intersperse _ [] = mempty
intersperse c (t:ts) = go t ts
  where
    go !acc [] = acc
    go acc (t':ts') = go (acc <> (c `cons` t')) ts'

-- | Add a 'Char' in front of a 'YiString'.
cons :: S.Segment TX.Text -> Rope TX.Text -> Rope TX.Text
cons c (Rope t) = case viewl t of
  EmptyL -> Yi.Rope.singleton c
  S.Chunk l x :< ts | l < defaultChunkSize -> Rope $ S.Chunk (l + 1) (c `S.cons` x) <| ts
  _ -> Rope $ S.Chunk 1 (S.singleton c) <| t

-- | Add a 'Char' in the back of a 'YiString'.
snoc :: Rope TX.Text -> S.Segment TX.Text -> Rope TX.Text
snoc (Rope t) c = case viewr t of
  EmptyR -> Yi.Rope.singleton c
  ts :> S.Chunk l x | l < defaultChunkSize -> Rope $ ts |> S.Chunk (l + 1) (x `TX.snoc` c)
  _ -> Rope $ t |> S.Chunk 1 (S.singleton c)

-- | Single character 'YiString'. Consider whether it's worth creating
-- this, maybe you can use 'cons' or 'snoc' instead?
singleton :: (Measured (Meas a) (S.Chunk a), S.Segmented a) => S.Segment a -> Rope a
singleton c = Rope . T.singleton $ S.Chunk 1 (S.singleton c)

-- | Splits the underlying string before the given line number.
-- Zero-indexed lines.
--
-- Splitting at line <= 0 gives you an empty string. Splitting at
-- @n > 0@ gives you the first n lines.
--
-- Also see 'splitAtLine''.
splitAtLine :: Int -> Rope TX.Text -> (Rope TX.Text, Rope TX.Text)
splitAtLine n r | n <= 0    = (empty, r)
                | otherwise = splitAtLine' (n - 1) r

-- | Splits the underlying string after the given line number.
-- Zero-indexed lines.
--
-- Splitting at line <= 0 gives you the first line. Splitting at
-- @n > 0@ gives you the first n + 1 lines.
--
-- The implementation is similar to that of 'splitAt' except we are
-- now looking for extra newlines in the next chunk rather than extra
-- characters.
splitAtLine' :: Int -> Rope TX.Text -> (Rope TX.Text, Rope TX.Text)
splitAtLine' p (Rope tr) = case viewl s of
  ch@(S.Chunk _ x) :< r ->
    let excess = lineIndex (measure f) + lineIndex (measure ch) - p - 1
        (lx, rx) = cutExcess excess x
    in (Rope $ f |- mkChunk TX.length lx,
        Rope $ mkChunk S.length rx -| r)
  _ -> (Rope f, Rope s)
  where
    (f, s) = T.split ((p <) . lineIndex) tr

    cutExcess :: Int -> TX.Text -> (TX.Text, TX.Text)
    cutExcess n t = case TX.length t of
      0 -> (TX.empty, TX.empty)
      _ -> let ns = countNl t
               ls = TX.lines t
               front = TX.unlines $ Prelude.take (ns - n) ls
               back = TX.drop (TX.length front) t
           in if n >= ns
              then (t, TX.empty)
              else (front, back)

-- | This is like 'lines'' but it does *not* preserve newlines.
--
-- Specifically, we just strip the newlines from the result of
-- 'lines''.
--
-- This behaves slightly differently than the old split: the number of
-- resulting strings here is equal to the number of newline characters
-- in the underlying string. This is much more consistent than the old
-- behaviour which blindly used @ByteString@s split and stitched the
-- result back together which was inconsistent with the rest of the
-- interface which worked with number of newlines.
lines :: Rope TX.Text -> [Rope TX.Text]
lines = Prelude.map dropNl . lines'
  where
    dropNl (Rope t)  = case viewr t of
      EmptyR -> Yi.Rope.empty
      ts :> ch@(S.Chunk l tx) ->
        Rope $ ts |- if S.null tx
                         then ch
                         else case S.last tx of
                           '\n' -> S.Chunk (l - 1) (S.init tx)
                           _ -> ch

-- | Splits the 'YiString' into a list of 'YiString' each containing a
-- line.
--
-- Note that in old implementation this allowed an arbitrary character
-- to split on. If you want to do that, manually convert 'toText' and
-- use 'TX.splitOn' to suit your needs. This case is optimised for
-- newlines only which seems to have been the only use of the original
-- function.
--
-- The newlines are preserved so this should hold:
--
-- > 'toText' . 'concat' . 'lines'' ≡ 'toText'
--
-- but the underlying structure might change: notably, chunks will
-- most likely change sizes.
lines' :: Rope TX.Text -> [Rope TX.Text]
lines' t = let (Rope f, Rope s) = splitAtLine' 0 t
           in if T.null s
              then if T.null f then [] else [Rope f]
              else Rope f : lines' (Rope s)

-- | Joins up lines by a newline character. It does not leave a
-- newline after the last line. If you want a more classical
-- 'Prelude.unlines' behaviour, use 'Yi.Rope.map' along with
-- 'Yi.Rope.snoc'.
unlines :: [Rope TX.Text] -> Rope TX.Text
unlines = Yi.Rope.intersperse '\n'

-- | 'YiString' specialised @any@.
--
-- Implementation note: this currently just does any by doing ‘TX.Text’
-- conversions upon consecutive chunks. We should be able to speed it
-- up by running it in parallel over multiple chunks.
any :: (S.Segment TX.Text -> Bool) -> Rope TX.Text -> Bool
any p = go . fromRope
  where
    go x = case viewl x of
      EmptyL -> False
      S.Chunk _ t :< ts -> S.any p t || go ts

-- | 'YiString' specialised @all@.
--
-- See the implementation note for 'Yi.Rope.any'.
all :: (S.Segment TX.Text -> Bool) -> Rope TX.Text -> Bool
all p = go . fromRope
  where
    go x = case viewl x of
      EmptyL -> True
      S.Chunk _ t :< ts -> S.all p t && go ts

-- | To serialise a 'YiString', we turn it into a regular 'String'
-- first.
instance Binary (Rope TX.Text) where
  put = put . toString
  get = Yi.Rope.fromString <$> get

-- | Write a 'YiString' into the given file.
--
-- It's up to the user to handle exceptions.
writeFile :: FilePath -> Rope TX.Text -> IO ()
writeFile f = TXIO.writeFile f . toText

-- | Reads file into the rope, also returning the 'ConverterName' that
-- was used for decoding. You should resupply this to 'writeFile' if
-- you're aiming to preserve the original encoding.
--
-- If we fail to guess the encoding used, error message is given
-- instead.
--
-- It is up to the user to handle exceptions not directly related to
-- character decoding.
readFile :: FilePath -> IO (Either TX.Text (Rope TX.Text))
readFile fp = BSL.readFile fp >>= go decoders
  where
  go [] _ = pure (Left err)
  go (d : ds) bytes =
      try (pure (d bytes)) >>= \case
          Left (_ :: TXEE.UnicodeException) -> go ds bytes
          Right text -> pure (Right (fromLazyText text))
  err = "Could not guess the encoding of " <> TX.pack fp
  decoders =
      [ TXLE.decodeUtf8
      , TXLE.decodeUtf16LE
      , TXLE.decodeUtf16BE
      , TXLE.decodeUtf32LE
      , TXLE.decodeUtf32BE
      ]

-- | Filters the characters from the underlying string.
--
-- >>> filter (/= 'a') "bac"
-- "bc"
filter :: (S.Segment TX.Text -> Bool) -> Rope TX.Text -> Rope TX.Text
filter p = Rope . go . fromRope
  where
    go t = case viewl t of
      EmptyL -> T.empty
      S.Chunk _ x :< ts -> mkChunk S.length (S.filter p x) -| go ts

-- | Maps the characters over the underlying string.
map :: (S.Segment TX.Text -> S.Segment TX.Text) -> Rope TX.Text -> Rope TX.Text
map f = Rope . go . fromRope
  where
    go t = case viewl t of
      EmptyL -> T.empty
      S.Chunk l x :< ts -> S.Chunk l (TX.map f x) <| go ts

-- | Join given 'YiString's with a space. Empty lines will be filtered
-- out first.
unwords :: [Rope TX.Text] -> Rope TX.Text
unwords = Yi.Rope.intersperse ' '

-- | Splits the given 'YiString' into a list of words, where spaces
-- are determined by 'isSpace'. No empty strings are in the result
-- list.
words :: Rope TX.Text -> [Rope TX.Text]
words = Prelude.filter (not . Yi.Rope.null) . Yi.Rope.split isSpace

-- | Splits the 'YiString' on characters matching the predicate, like
-- 'TX.split'.
--
-- For splitting on newlines use 'Yi.Rope.lines' or 'Yi.Rope.lines''
-- instead.
--
-- Implementation note: GHC actually makes this naive implementation
-- about as fast and in cases with lots of splits, faster, as a
-- hand-rolled version on chunks with appends which is quite amazing
-- in itself.
split :: (Char -> Bool) -> Rope TX.Text -> [Rope TX.Text]
split p = fmap fromText . TX.split p . toText

-- | Left fold.
--
-- Benchmarks show that folding is actually Pretty Damn Slow™: consider
-- whether folding is really the best thing to use in your scenario.
foldl' :: (a -> S.Segment TX.Text -> a) -> a -> Rope TX.Text -> a
foldl' f a = go a . fromRope
  where
    go acc t = case viewl t of
      EmptyL -> acc
      S.Chunk _ x :< ts -> let r = TX.foldl' f acc x
                            in r `seq` go r ts

-- | Replicate the given YiString set number of times, concatenating
-- the results. Also see 'Yi.Rope.replicateChar'.
replicate :: Int -> Rope TX.Text -> Rope TX.Text
replicate n t | n <= 0 = mempty
              | otherwise = t <> Yi.Rope.replicate (n - 1) t

-- | Replicate the given character set number of times and pack the
-- result into a 'YiString'.
--
-- >>> replicateChar 4 ' '
-- "    "
replicateChar :: Int -> Char -> Rope TX.Text
replicateChar n = fromText . TX.replicate n . TX.singleton

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
withText :: (TX.Text -> TX.Text) -> Rope TX.Text -> Rope TX.Text
withText f = Rope . T.fmap' (mkChunk S.length . f . _fromChunk) . fromRope

-- | Maps over each __chunk__ which means this function is UNSAFE! If
-- you use this with functions which don't preserve 'Size', that is
-- the chunk length and number of newlines, things will break really,
-- really badly. You should not need to use this.
--
-- Also see 'T.unsafeFmap'
unsafeWithText :: (TX.Text -> TX.Text) -> Rope TX.Text -> Rope TX.Text
unsafeWithText f = Rope . T.unsafeFmap g . fromRope
  where
    g (S.Chunk l t) = S.Chunk l (f t)
