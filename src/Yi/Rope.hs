{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_HADDOCK show-extensions #-}

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

   -- * List-like functions
   Yi.Rope.null, Yi.Rope.empty, Yi.Rope.take, Yi.Rope.drop,
   Yi.Rope.length, Yi.Rope.reverse, Yi.Rope.countNewLines,

   -- * Text manipulations
   Yi.Rope.lines, Yi.Rope.lines',
   Yi.Rope.splitAt, Yi.Rope.splitAtLine,
   Yi.Rope.cons, Yi.Rope.snoc, Yi.Rope.singleton,

   Yi.Rope.head, Yi.Rope.last,

   Yi.Rope.append, Yi.Rope.concat,

   -- * IO
   Yi.Rope.readFile, Yi.Rope.writeFile

  ) where

import           Control.Applicative ((<$>))
import           Control.DeepSeq
import           Data.Binary
import qualified Data.FingerTree as T
import           Data.FingerTree hiding (null, empty, reverse, split)
import qualified Data.List as L (foldl')
import           Data.Monoid
import           Data.String (IsString(..))
import qualified Data.Text as TX
import qualified Data.Text.IO as TF (writeFile, readFile)
import           Prelude hiding (drop)

-- | Used to cache the size of the strings.
data Size = Indices { charIndex :: {-# UNPACK #-} !Int
                      -- ^ How many characters under here?
                    , lineIndex :: Int
                      -- ^ How many lines under here?
                    } deriving (Eq, Show)

-- | A chunk storing the string of the type it is indexed by. It
-- caches the length of stored string.
data YiChunk = Chunk { chunkSize :: {-# UNPACK #-} !Int
                     , _fromChunk :: {-# UNPACK #-} !TX.Text
                     } deriving (Show, Eq)

-- | Makes a chunk from a given string. We allow for an arbitrary
-- length function here to allow us to bypass the calculation with
-- 'const' in case the length is known ahead of time. In most cases,
-- the use of this is
--
-- > mkChunk 'TX.Text.length' someText
mkChunk :: (TX.Text -> Int) -- ^ The length function to use.
        -> TX.Text
        -> YiChunk
mkChunk l t = Chunk (l t) t

-- | Transform the chunk content. It's vital that the transformation
-- preserves the length of the content.
overChunk :: (TX.Text -> TX.Text) -- ^ Length-preserving content transformation.
          -> YiChunk -> YiChunk
overChunk f (Chunk l t) = Chunk l (f t)

instance Monoid Size where
  mempty = Indices 0 0
  Indices c l `mappend` Indices c' l' = Indices (c + c') (l + l')

instance Measured Size YiChunk where
  measure (Chunk l t) = Indices l (TX.count "\n" t)

-- | A 'YiString' is a 'FingerTree' with cached column and line counts
-- over chunks of 'TX.Text'.
newtype YiString = YiString { fromRope :: FingerTree Size YiChunk }
                 deriving (Show, Eq)

instance NFData Size where
  rnf (Indices !c !l) = c `seq` l `seq` ()

instance NFData YiChunk where
  rnf (Chunk !i !t) = i `seq` rnf t

instance NFData YiString where
  rnf = rnf . toText

instance IsString YiString where
  fromString = Yi.Rope.fromString

instance Monoid YiString where
  mempty = Yi.Rope.empty
  mappend = Yi.Rope.append
  mconcat = Yi.Rope.concat

(-|) :: YiChunk -> FingerTree Size YiChunk -> FingerTree Size YiChunk
b -| t | chunkSize b == 0 = t
       | otherwise        = b <| t

(|-) :: FingerTree Size YiChunk -> YiChunk -> FingerTree Size YiChunk
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
reverse :: YiString -> YiString
reverse = YiString . fmap' (overChunk TX.reverse) . T.reverse . fromRope

-- | See 'fromText'.
fromString :: String -> YiString
fromString = fromText . TX.pack

-- | See 'fromText''.
fromString' :: Int -> String -> YiString
fromString' n = fromText' n . TX.pack

-- | See 'toText'.
toString :: YiString -> String
toString = TX.unpack . toText

-- | See 'toReverseText'.
toReverseString :: YiString -> String
toReverseString = Prelude.reverse . toString

-- | This is like 'fromText' but it allows the user to specify the
-- chunk size to be used. Uses 'defaultChunkSize' if the given
-- size is <= 0.
fromText' :: Int -> TX.Text -> YiString
fromText' n | n <= 0 = fromText' defaultChunkSize
            | otherwise = YiString . r T.empty . f
  where
    f = TX.chunksOf n

    -- Convert the given string into chunks in the tree. We have a
    -- special case for a single element case: because we split on
    -- predetermined chunk size, we know that all chunks but the last
    -- one will be the specified size so we can optimise here instead
    -- of having to recompute chunk size at creation.
    r :: FingerTree Size YiChunk -> [TX.Text] -> FingerTree Size YiChunk
    r tr []      = tr
    r tr [!ts]   = tr |- mkChunk TX.length ts
    r tr (!t:ts) = let r' = tr |- mkChunk (const n) t
                   in r r' ts

-- | Converts a 'TX.Text' into a 'YiString' using
-- 'defaultChunkSize'-sized chunks for the underlying tree.
fromText :: TX.Text -> YiString
fromText = fromText' defaultChunkSize

-- | Consider whether you really need to use this!
toText :: YiString -> TX.Text
toText = TX.concat . go . fromRope
  where
    go :: FingerTree Size YiChunk -> [TX.Text]
    go t = case viewl t of
      Chunk _ !c :< cs -> c : go cs
      EmptyL -> []

-- | Spits out the underlying string, reversed.
--
-- Note that this is actually slightly faster than manually unrolling
-- the tree from the end, 'TX.reverse'ing each chunk and
-- 'TX.concat'ing, at least with -O2 which you really need to be using
-- with 'TX.Text' anyway.
toReverseText :: YiString -> TX.Text
toReverseText = TX.reverse . toText

-- | Checks if the given 'YiString' is actually empty.
null :: YiString -> Bool
null = T.null . fromRope

-- | Creates an empty 'YiString'.
empty :: YiString
empty = YiString T.empty

-- | Length of the whole underlying string.
--
-- Amortized constant time.
length :: YiString -> Int
length = charIndex . measure . fromRope

-- | Count the number of newlines in the underlying string. This is
-- actually amortized constant time as we cache this information in
-- the underlying tree.
countNewLines :: YiString -> Int
countNewLines = lineIndex . measure . fromRope

-- | Append two 'YiString's.
append :: YiString -> YiString -> YiString
append (YiString t) (YiString t') = YiString $ t T.>< t'

-- | Concat a list of 'YiString's.
concat :: [YiString] -> YiString
concat = L.foldl' append empty

-- | Take the first character of the underlying string if possible.
head :: YiString -> Maybe Char
head (YiString t) = case viewl t of
  Chunk _ x :< _ -> if TX.null x then Nothing else Just (TX.head x)
  EmptyL          -> Nothing

-- | Take the last character of the underlying string if possible.
last :: YiString -> Maybe Char
last (YiString t) = case viewr t of
  _ :> Chunk _ x -> if TX.null x then Nothing else Just (TX.last x)
  EmptyR          -> Nothing

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
splitAt :: Int -> YiString -> (YiString, YiString)
splitAt n i@(YiString t) = case viewl s of
  Chunk l x :< ts | n' /= 0 ->
    let (lx, rx) = TX.splitAt n' x
    in (YiString $ f |> Chunk n' lx,
        YiString $ Chunk (l - n') rx -| ts)
  _ -> (Yi.Rope.empty, i)
  where
    (f, s) = T.split ((> n) . charIndex) t
    n' = n - charIndex (measure f)

-- | Takes the first n given characters.
take :: Int -> YiString -> YiString
take n = fst . Yi.Rope.splitAt n

-- | Drops the first n characters.
drop :: Int -> YiString -> YiString
drop n = snd . Yi.Rope.splitAt n

-- | Add a 'Char' in front of a 'YiString'.
--
-- We add the character to the front of the first chunk. This does
-- mean that a lot of 'cons' might result in an abnormally large first
-- chunk so if you have to do that, consider using 'append' instead..
cons :: Char -> YiString -> YiString
cons c (YiString t) = YiString $ case viewl t of
  Chunk !l x :< ts -> Chunk (l + 1) (c `TX.cons` x) <| ts
  EmptyL -> T.singleton $ Chunk 1 (TX.singleton c)

-- | Add a 'Char' in the back of a 'YiString'.
--
-- We add the character to the end of the last chunk. This does mean
-- that a lot of 'snoc' might result in an abnormally large last chunk
-- so if you have to do that, consider using 'append' instead..
snoc :: YiString -> Char -> YiString
snoc (YiString t) c = YiString $ case viewr t of
  ts :> Chunk l x -> ts |> Chunk (l + 1) (x `TX.snoc` c)
  EmptyR -> T.singleton $ Chunk 1 (TX.singleton c)

-- | Single character 'YiString'. Consider whether it's worth creating
-- this, maybe you can use 'cons' or 'snoc' instead?
singleton :: Char -> YiString
singleton c = YiString . T.singleton $ Chunk 1 (TX.singleton c)

-- | Splits the underlying string before the given line number.
-- Zero-indexed lines.
--
-- Splitting at line <= 0 gives you an empty string. Splitting at
-- @n > 0@ gives you the first n lines.
--
-- Also see 'splitAtLine''.
splitAtLine :: Int -> YiString -> (YiString, YiString)
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
splitAtLine' :: Int -> YiString -> (YiString, YiString)
splitAtLine' p (YiString tr) = case viewl s of
  ch@(Chunk _ x) :< r ->
    let excess = lineIndex (measure f) + lineIndex (measure ch) - p - 1
        (lx, rx) = cutExcess excess x
    in (YiString $ f |- mkChunk TX.length lx,
        YiString $ mkChunk TX.length rx -| r)
  _ -> (YiString f, YiString s)
  where
    (f, s) = T.split ((p <) . lineIndex) tr

    cutExcess :: Int -> TX.Text -> (TX.Text, TX.Text)
    cutExcess n t = case TX.length t of
      0 -> (TX.empty, TX.empty)
      _ -> let ns = TX.count "\n" t
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
lines :: YiString -> [YiString]
lines = map dropNl . lines'
  where
    dropNl (YiString t) = case viewr t of
      ts :> ch@(Chunk l tx) ->
        YiString $ ts |- if TX.null tx
                         then ch
                         else case TX.last tx of
                           '\n' -> Chunk (l - 1) (TX.init tx)
                           _ -> ch
      EmptyR -> YiString T.empty

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
lines' :: YiString -> [YiString]
lines' t = let (YiString f, YiString s) = splitAtLine' 0 t
           in if T.null s
              then if T.null f then [] else [YiString f]
              else YiString f : lines' (YiString s)

-- | To serialise a 'YiString', we turn it into a regular 'String'
-- first.
instance Binary YiString where
  put = put . toString
  get = Yi.Rope.fromString <$> get

writeFile :: FilePath -> YiString -> IO ()
writeFile f = TF.writeFile f . toText

readFile :: FilePath -> IO YiString
readFile f = fromText <$> TF.readFile f
