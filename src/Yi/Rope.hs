{-# language BangPatterns #-}
{-# language DeriveDataTypeable #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language ViewPatterns #-}
{-# language FlexibleInstances #-}
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
import qualified Data.ListLike as LL
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

import qualified Yi.Braid as B

-- | A chunk caches the length of the underlying Text since computing the
-- length of Text is O(n)
data YiChunk = Chunk { _chunkSize :: {-# UNPACK #-} !Int
                     , _fromChunk :: {-# UNPACK #-} !TX.Text
                     } deriving (Show, Eq, Typeable)

instance NFData YiChunk where
  rnf (Chunk !i !t) = i `seq` rnf t

-- | Transform the chain inside the chunk. It's vital that the transformation
-- preserves the length of the content.
overChunk :: (TX.Text -> TX.Text) -- ^ Length-preserving content transformation.
          -> YiChunk -> YiChunk
overChunk f (Chunk l t) = Chunk l (f t)


-- | A 'YiString' is a 'FingerTree' with cached char and line counts
-- over chunks of 'TX.Text'.
type YiString = B.Braid Size YiChunk

fromRope :: YiString -> FingerTree Size YiChunk
fromRope = B.fromBraid

-- | Used to cache the size of the strings.
data Size = Indices { charIndex :: {-# UNPACK #-} !Int
                      -- ^ How many characters under here?
                    , lineIndex :: Int
                      -- ^ How many lines under here?
                    } deriving (Eq, Show, Typeable)

instance B.HasSize Size where
  getSize = charIndex

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


-- | Counts number of newlines in the given 'TX.Text'.
countNl :: TX.Text -> Int
countNl = TX.count "\n"

instance Monoid Size where
  mempty = Indices 0 0
  Indices c l `mappend` Indices c' l' = Indices (c + c') (l + l')

instance Measured Size YiChunk where
  measure (Chunk l t) = Indices l (countNl t)

instance NFData Size where
  rnf (Indices !c !l) = c `seq` l `seq` ()

instance IsString YiString where
  fromString = Yi.Rope.fromString

(-|) :: YiChunk -> FingerTree Size YiChunk -> FingerTree Size YiChunk
(-|) = (B.-|)

(|-) :: FingerTree Size YiChunk -> YiChunk -> FingerTree Size YiChunk
(|-) = (B.|-)

-- | Reverse the whole underlying string.
--
-- This involves reversing the order of the chunks as well as content
-- of the chunks. We use a little optimisation here that re-uses the
-- content of each chunk but this exposes a potential problem: after
-- many transformations, our chunks size might become quite varied
-- (but never more than the default size), perhaps we should
-- periodically rechunk the tree to recover nice sizes?
reverse :: YiString -> YiString
reverse = B.reverse

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
--
-- Note that it is actually ~4.5 times faster to use 'toReverseText'
-- and unpack the result than to convert to 'String' and use
-- 'Prelude.reverse'.
toReverseString :: YiString -> String
toReverseString = TX.unpack . toReverseText

-- | This is like 'fromText' but it allows the user to specify the
-- chunk size to be used. Uses 'defaultChunkSize' if the given
-- size is <= 0.
fromText' :: Int -> TX.Text -> YiString
fromText' n = B.toBraid' n . mkChunk TX.length

-- | Converts a 'TX.Text' into a 'YiString' using
-- 'defaultChunkSize'-sized chunks for the underlying tree.
fromText :: TX.Text -> YiString
fromText = B.toBraid . mkChunk TX.length

fromLazyText :: TXL.Text -> YiString
fromLazyText = B.Braid . T.fromList . fmap (mkChunk TX.length) . TXL.toChunks

-- | Consider whether you really need to use this!
toText :: YiString -> TX.Text
toText = _fromChunk . B.extractBraid

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
null = B.null

-- | Creates an empty 'YiString'.
empty :: YiString
empty = B.empty

-- | Length of the whole underlying string.
--
-- Amortized constant time.
length :: YiString -> Int
length = B.length

-- | Count the number of newlines in the underlying string. This is
-- actually amortized constant time as we cache this information in
-- the underlying tree.
countNewLines :: YiString -> Int
countNewLines = lineIndex . measure . B.fromBraid

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
append :: YiString -> YiString -> YiString
append = B.append

-- | Concat a list of 'YiString's.
concat :: [YiString] -> YiString
concat = B.concat

-- | Take the first character of the underlying string if possible.
head :: YiString -> Maybe Char
head = B.head

-- | Take the last character of the underlying string if possible.
last :: YiString -> Maybe Char
last = B.last

-- | Takes every character but the last one: returns Nothing on empty
-- string.
init :: YiString -> Maybe YiString
init = B.init

-- | Takes the tail of the underlying string. If the string is empty
-- to begin with, returns Nothing.
tail :: YiString -> Maybe YiString
tail = B.tail

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
splitAt = B.splitAt

-- | Takes the first n given characters.
take :: Int -> YiString -> YiString
take = B.take

-- | Drops the first n characters.
drop :: Int -> YiString -> YiString
drop = B.drop

-- | The usual 'Prelude.dropWhile' optimised for 'YiString's.
dropWhile :: (Char -> Bool) -> YiString -> YiString
dropWhile = B.dropWhile

-- | As 'Yi.Rope.dropWhile' but drops from the end instead.
dropWhileEnd :: (Char -> Bool) -> YiString -> YiString
dropWhileEnd = B.dropWhileEnd

-- | The usual 'Prelude.takeWhile' optimised for 'YiString's.
takeWhile :: (Char -> Bool) -> YiString -> YiString
takeWhile = B.takeWhile

-- | Like 'Yi.Rope.takeWhile' but takes from the end instead.
takeWhileEnd :: (Char -> Bool) -> YiString -> YiString
takeWhileEnd = B.takeWhileEnd

-- | Returns a pair whose first element is the longest prefix
-- (possibly empty) of t of elements that satisfy p, and whose second
-- is the remainder of the string. See also 'TX.span'.
--
-- This implementation uses 'Yi.Rope.splitAt' which actually is just
-- as fast as hand-unrolling the tree. GHC sure is great!
span :: (Char -> Bool) -> YiString -> (YiString, YiString)
span = B.span

-- | Just like 'Yi.Rope.span' but with the predicate negated.
break :: (Char -> Bool) -> YiString -> (YiString, YiString)
break = B.break

-- | Concatenates the list of 'YiString's after inserting the
-- user-provided 'YiString' between the elements.
--
-- Empty 'YiString's are not ignored and will end up as strings of
-- length 1. If you don't want this, it's up to you to pre-process the
-- list. Just as with 'Yi.Rope.intersperse', it is up to the user to
-- pre-process the list.
intercalate :: YiString -> [YiString] -> YiString
intercalate = B.intercalate

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
intersperse :: Char -> [YiString] -> YiString
intersperse = B.intersperse

-- | Add a 'Char' in front of a 'YiString'.
cons :: Char -> YiString -> YiString
cons = B.cons

-- | Add a 'Char' in the back of a 'YiString'.
snoc :: YiString -> Char -> YiString
snoc = B.snoc

-- | Single character 'YiString'. Consider whether it's worth creating
-- this, maybe you can use 'cons' or 'snoc' instead?
singleton :: Char -> YiString
singleton = B.singleton

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
splitAtLine' p (B.Braid tr) = case viewl s of
  ch@(Chunk _ x) :< r ->
    let excess = lineIndex (measure f) + lineIndex (measure ch) - p - 1
        (lx, rx) = cutExcess excess x
    in (B.Braid $ f |- mkChunk TX.length lx,
        B.Braid $ mkChunk TX.length rx -| r)
  _ -> (B.Braid f, B.Braid s)
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
lines :: YiString -> [YiString]
lines = Prelude.map dropNl . lines'
  where
    dropNl (B.Braid t)  = case viewr t of
      EmptyR -> Yi.Rope.empty
      ts :> ch@(Chunk l tx) ->
        B.Braid $ ts |- if TX.null tx
                         then ch
                         else case TX.last tx of
                           '\n' -> Chunk (l - 1) (TX.init tx)
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
lines' :: YiString -> [YiString]
lines' t = let (B.Braid f, B.Braid s) = splitAtLine' 0 t
           in if T.null s
              then if T.null f then [] else [B.Braid f]
              else B.Braid f : lines' (B.Braid s)

-- | Joins up lines by a newline character. It does not leave a
-- newline after the last line. If you want a more classical
-- 'Prelude.unlines' behaviour, use 'Yi.Rope.map' along with
-- 'Yi.Rope.snoc'.
unlines :: [YiString] -> YiString
unlines = Yi.Rope.intersperse '\n'

-- | 'YiString' specialised @any@.
--
-- Implementation note: this currently just does any by doing ‘TX.Text’
-- conversions upon consecutive chunks. We should be able to speed it
-- up by running it in parallel over multiple chunks.
any :: (Char -> Bool) -> YiString -> Bool
any = B.any

-- | 'YiString' specialised @all@.
--
-- See the implementation note for 'Yi.Rope.any'.
all :: (Char -> Bool) -> YiString -> Bool
all = B.all

-- | To serialise a 'YiString', we turn it into a regular 'String'
-- first.
instance Binary YiString where
  put = put . toString
  get = Yi.Rope.fromString <$> get

-- | Write a 'YiString' into the given file.
--
-- It's up to the user to handle exceptions.
writeFile :: FilePath -> YiString -> IO ()
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
readFile :: FilePath -> IO (Either TX.Text YiString)
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
filter :: (Char -> Bool) -> YiString -> YiString
filter = B.filter

-- | Maps the characters over the underlying string.
map :: (Char -> Char) -> YiString -> YiString
map = B.map

-- | Join given 'YiString's with a space. Empty lines will be filtered
-- out first.
unwords :: [YiString] -> YiString
unwords = Yi.Rope.intersperse ' '

-- | Splits the given 'YiString' into a list of words, where spaces
-- are determined by 'isSpace'. No empty strings are in the result
-- list.
words :: YiString -> [YiString]
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
split :: (Char -> Bool) -> YiString -> [YiString]
split = B.split

-- | Left fold.
--
-- Benchmarks show that folding is actually Pretty Damn Slow™: consider
-- whether folding is really the best thing to use in your scenario.
foldl' :: (a -> Char -> a) -> a -> YiString -> a
foldl' = B.foldl'

-- | Replicate the given YiString set number of times, concatenating
-- the results. Also see 'Yi.Braid.replicateChar'.
replicate :: Int -> YiString -> YiString
replicate = B.replicate

-- | Replicate the given character set number of times and pack the
-- result into a 'YiString'.
--
-- >>> replicateChar 4 ' '
-- "    "
replicateChar :: Int -> Char -> YiString
replicateChar = B.replicateSegment

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
withText :: (TX.Text -> TX.Text) -> YiString -> YiString
withText = B.fmap' . overChunk

-- | Maps over each __chunk__ which means this function is UNSAFE! If
-- you use this with functions which don't preserve 'Size', that is
-- the chunk length and number of newlines, things will break really,
-- really badly. You should not need to use this.
--
-- Also see 'T.unsafeFmap'
unsafeWithText :: (TX.Text -> TX.Text) -> YiString -> YiString
unsafeWithText = B.unsafeWithChunk . overChunk

instance Monoid YiChunk where
  mempty = Chunk 0 mempty
  Chunk n txt `mappend` Chunk n' txt' = Chunk (n + n') (txt `mappend` txt')

instance LL.FoldableLL YiChunk Char where
  foldl f d (Chunk _ x) = LL.foldl f d x
  foldl' f d (Chunk _ x)= LL.foldl' f d x
  foldl1 f (Chunk _ x)= LL.foldl1 f x
  foldr f d (Chunk _ x)= LL.foldr f d x
  foldr' f d (Chunk _ x) = LL.foldr' f d x
  foldr1 f (Chunk _ x) = LL.foldr1 f x

instance LL.ListLike YiChunk Char where
  empty = Chunk 0 TX.empty
  singleton = Chunk 1 . TX.singleton
  cons c (Chunk n x) = Chunk (n + 1) (TX.cons c x)
  snoc (Chunk n x) c = Chunk (n + 1) (TX.snoc x c)
  append (Chunk n x) (Chunk n' x') = Chunk (n + n') (x `TX.append` x')
  head (Chunk _ x) = TX.head x
  uncons (Chunk n x) = case TX.uncons x of
                         Just (c, rest) -> Just (c, Chunk (n - 1) rest)
                         Nothing -> Nothing
  last (Chunk _ x) = TX.last x
  tail (Chunk l x) = (Chunk (l - 1) (TX.tail x))
  init (Chunk l x) = (Chunk (l - 1) (TX.init x))

  null (Chunk 0 _) = True
  null _ = False
  length (Chunk l _) = l
  -- map f (Chunk n x) = Chunk n (TX.map f x)
  -- rigidMap = LL.map
  reverse (Chunk n x) = Chunk n (TX.reverse x)
  intersperse _ (Chunk 0 x) = Chunk 0 x
  intersperse _ (Chunk 1 x) = Chunk 1 x
  intersperse c (Chunk n x) = Chunk ((2 * n) - 1) (TX.intersperse c x)
  -- concat = fold
  -- concatMap = foldMap
  -- rigidConcatMap = concatMap
  any p (Chunk _ x) = TX.any p x
  all p (Chunk _ x) = TX.all p x
  -- maximum = foldr1 max
  -- minimum = foldr1 min
  replicate n c = Chunk n (TX.replicate n (TX.singleton c))
  take n c@(Chunk n' x) | n >= n' = c
                        | otherwise = Chunk n (TX.take n x)
  drop n (Chunk n' x) | n >= n' = LL.empty
                      | otherwise = Chunk (n' - n) (TX.drop n x)

  splitAt n c@(Chunk n' x)
    | n <= 0 = (LL.empty, c)
    | n >= n' = (c, LL.empty)
    | otherwise = let (pre, post) = TX.splitAt n x
                   in (Chunk n pre, Chunk (n' - n) post)

  takeWhile p (Chunk _ x) =
    let x' = TX.takeWhile p x
     in (Chunk (TX.length x') x)

  dropWhile p (Chunk _ x) =
    let x' = TX.dropWhile p x
     in (Chunk (TX.length x') x)

  dropWhileEnd p (Chunk _ x) =
    let x' = TX.dropWhileEnd p x
     in (Chunk (TX.length x') x)

  span p (Chunk l x) = let (pre, post) = TX.span p x
                           preLen = TX.length pre
                           postLen = l - preLen
                        in (Chunk preLen pre, Chunk postLen post)

  -- break p = LL.span (not . p)
  -- group :: (ListLike full' full, Eq item) => full -> full' Source #
  -- inits :: ListLike full' full => full -> full' Source #
  -- tails :: ListLike full' full => full -> full' Source #
  -- isPrefixOf :: Eq item => full -> full -> Bool Source #
  -- isSuffixOf :: Eq item => full -> full -> Bool Source #
  -- isInfixOf :: Eq item => full -> full -> Bool Source #
  -- stripPrefix :: Eq item => full -> full -> Maybe full Source #
  -- stripSuffix :: Eq item => full -> full -> Maybe full Source #
  -- elem :: Eq item => item -> full -> Bool Source #
  -- notElem :: Eq item => item -> full -> Bool Source #
  -- find :: (item -> Bool) -> full -> Maybe item Source #
  filter p (Chunk _ x) = mkChunk TX.length $ TX.filter p x
  -- index :: full -> Int -> item Source #
  -- elemIndex :: Eq item => item -> full -> Maybe Int Source #
  -- elemIndices :: (Eq item, ListLike result Int) => item -> full -> result Source #
  -- findIndex :: (item -> Bool) -> full -> Maybe Int Source #
  -- findIndices :: ListLike result Int => (item -> Bool) -> full -> result Source #
  -- sequence :: (Monad m, ListLike fullinp (m item)) => fullinp -> m full Source #
  -- mapM :: (Monad m, ListLike full' item') => (item -> m item') -> full -> m full' Source #
  -- rigidMapM :: Monad m => (item -> m item) -> full -> m full Source #
  -- nub :: Eq item => full -> full Source #
  -- delete :: Eq item => item -> full -> full Source #
  -- deleteFirsts :: Eq item => full -> full -> full Source #
  -- union :: Eq item => full -> full -> full Source #
  -- intersect :: Eq item => full -> full -> full Source #
  -- sort :: Ord item => full -> full Source #
  -- insert :: Ord item => item -> full -> full Source #
  -- toList :: full -> [item] Source #
  -- fromList :: [item] -> full Source #
  -- fromListLike :: ListLike full' item => full -> full' Source #
  -- nubBy :: (item -> item -> Bool) -> full -> full Source #
  -- deleteBy :: (item -> item -> Bool) -> item -> full -> full Source #
  -- deleteFirstsBy :: (item -> item -> Bool) -> full -> full -> full Source #
  -- unionBy :: (item -> item -> Bool) -> full -> full -> full Source #
  -- intersectBy :: (item -> item -> Bool) -> full -> full -> full Source #
  -- groupBy :: (ListLike full' full, Eq item) => (item -> item -> Bool) -> full -> full' Source #
  -- sortBy :: (item -> item -> Ordering) -> full -> full Source #
  -- insertBy :: (item -> item -> Ordering) -> item -> full -> full Source #
  -- genericLength :: Num a => full -> a Source #
  -- genericTake :: Integral a => a -> full -> full Source #
  -- genericDrop :: Integral a => a -> full -> full Source #
  -- genericSplitAt :: Integral a => a -> full -> (full, full) Source #
  -- genericReplicate :: Integral a => a -> item -> full
