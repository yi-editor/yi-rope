{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

   -- * IO
   Yi.Rope.readFile, Yi.Rope.readFile', Yi.Rope.writeFile,

   -- * Escape latches to underlying content. Note that these are safe
   -- to use but it does not mean they should.
   Yi.Rope.fromRope, Yi.Rope.withText, Yi.Rope.unsafeWithText

  ) where

import           Control.Applicative ((<$>))
import           Control.DeepSeq
import           Data.Binary
import           Data.Char (isSpace)
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

countNl :: TX.Text -> Int
countNl = TX.count (TX.pack "\n")

instance Monoid Size where
  mempty = Indices 0 0
  Indices c l `mappend` Indices c' l' = Indices (c + c') (l + l')

instance Measured Size YiChunk where
  measure (Chunk l t) = Indices l (countNl t)

-- | A 'YiString' is a 'FingerTree' with cached column and line counts
-- over chunks of 'TX.Text'.
newtype YiString = YiString { fromRope :: FingerTree Size YiChunk }
                 deriving (Show)

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
instance Eq YiString where
  t == t' = Yi.Rope.length t == Yi.Rope.length t' && toText t == toText t'

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

instance Ord YiString where
  compare x y = toText x `compare` toText y

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
  EmptyL -> Nothing
  Chunk _ x :< _ -> if TX.null x then Nothing else Just (TX.head x)

-- | Take the last character of the underlying string if possible.
last :: YiString -> Maybe Char
last (YiString t) = case viewr t of
  EmptyR -> Nothing
  _ :> Chunk _ x -> if TX.null x then Nothing else Just (TX.last x)

-- | Takes every character but the last one: returns Nothing on empty
-- string.
init :: YiString -> Maybe YiString
init (YiString t) = case viewr t of
  EmptyR -> Nothing
  ts :> Chunk 0 _ -> Yi.Rope.init (YiString ts)
  ts :> Chunk l x -> Just . YiString $ ts |- Chunk (l - 1) (TX.init x)

-- | Takes the tail of the underlying string. If the string is empty
-- to begin with, returns Nothing.
tail :: YiString -> Maybe YiString
tail (YiString t) = case viewr t of
  EmptyR -> Nothing
  ts :> Chunk 0 _ -> Yi.Rope.tail (YiString ts)
  ts :> Chunk l x -> Just . YiString $ ts |- Chunk (l - 1) (TX.tail x)

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
splitAt n (YiString t)
  | n <= 0 = (mempty, YiString t)
  | otherwise = case viewl s of
    Chunk l x :< ts | n' /= 0 ->
      let (lx, rx) = TX.splitAt n' x
      in (YiString $ f |> Chunk n' lx,
          YiString $ Chunk (l - n') rx -| ts)
    _ -> (YiString f, YiString s)
  where
    (f, s) = T.split ((> n) . charIndex) t
    n' = n - charIndex (measure f)

-- | Takes the first n given characters.
take :: Int -> YiString -> YiString
take n = fst . Yi.Rope.splitAt n

-- | Drops the first n characters.
drop :: Int -> YiString -> YiString
drop n = snd . Yi.Rope.splitAt n

-- | The usual 'Prelude.dropWhile' optimised for 'YiString's.
dropWhile :: (Char -> Bool) -> YiString -> YiString
dropWhile p = YiString . go . fromRope
  where
    go t = case viewl t of
      EmptyL -> T.empty
      Chunk 0 _ :< ts -> go ts
      Chunk l x :< ts ->
        let r = TX.dropWhile p x
            l' = TX.length r
        in case compare l' l of
          -- We dropped nothing so we must be done.
          EQ -> t
          -- We dropped something, if it was everything then drop from
          -- next chunk.
          LT | TX.null r -> go ts
          -- It wasn't everything and we have left-overs, we must be done.
             | otherwise -> Chunk l' r <| ts
          -- We shouldn't really get here or it would mean that
          -- dropping stuff resulted in more content than we had. This
          -- can only happen if unsafe functions don't preserve the
          -- chunk size and it goes out of sync with the text length.
          -- Preserve this abomination, it may be useful for
          -- debugging.
          _ -> Chunk l' r -| ts

-- | As 'Yi.Rope.dropWhile' but drops from the end instead.
dropWhileEnd :: (Char -> Bool) -> YiString -> YiString
dropWhileEnd p = YiString . go . fromRope
  where
    go t = case viewr t of
      EmptyR -> T.empty
      ts :> Chunk 0 _ -> go ts
      ts :> Chunk l x ->
        let r = TX.dropWhileEnd p x
            l' = TX.length r
        in case compare l' l of
          EQ -> t
          LT | TX.null r -> go ts
             | otherwise -> ts |> Chunk l' r
          _ -> ts |- Chunk l' r

-- | The usual 'Prelude.takeWhile' optimised for 'YiString's.
takeWhile :: (Char -> Bool) -> YiString -> YiString
takeWhile p = YiString . go . fromRope
  where
    go t = case viewl t of
      EmptyL -> T.empty
      Chunk 0 _ :< ts -> go ts
      Chunk l x :< ts ->
        let r = TX.takeWhile p x
            l' = TX.length r
        in case compare l' l of
          -- We took the whole chunk, keep taking more.
          EQ -> Chunk l x -| go ts
          -- We took some stuff but not everything so we're done.
          -- Alternatively, we took more than the size chunk so
          -- preserve this wonder. This should only ever happen if you
          -- use unsafe functions and Chunk size goes out of sync with
          -- actual text length.
          _ -> T.singleton $ Chunk l' r

-- | Like 'Yi.Rope.takeWhile' but takes from the end instead.
takeWhileEnd :: (Char -> Bool) -> YiString -> YiString
takeWhileEnd p = YiString . go . fromRope
  where
    go t = case viewr t of
      EmptyR -> T.empty
      ts :> Chunk 0 _ -> go ts
      ts :> Chunk l x -> case compare l' l of
        EQ -> go ts |> Chunk l x
        _ -> T.singleton $ Chunk l' r
        where
          -- no TX.takeWhileEnd – https://github.com/bos/text/issues/89
          r = TX.reverse . TX.takeWhile p . TX.reverse $ x
          l' = TX.length r


-- | Returns a pair whose first element is the longest prefix
-- (possibly empty) of t of elements that satisfy p, and whose second
-- is the remainder of the string. See also 'TX.span'.
--
-- This implementation uses 'Yi.Rope.splitAt' which actually is just
-- as fast as hand-unrolling the tree. GHC sure is great!
span :: (Char -> Bool) -> YiString -> (YiString, YiString)
span p y = let x = Yi.Rope.takeWhile p y
           in case Yi.Rope.splitAt (Yi.Rope.length x) y of
             -- Re-using ‘x’ seems to gain us a minor performance
             -- boost.
             (_, y') -> (x, y')

-- | Just like 'Yi.Rope.span' but with the predicate negated.
break :: (Char -> Bool) -> YiString -> (YiString, YiString)
break p = Yi.Rope.span (not . p)

-- | Concatenates the list of 'YiString's after inserting the
-- user-provided 'YiString' between the elements.
--
-- Empty 'YiString's are not ignored and will end up as strings of
-- length 1. If you don't want this, it's up to you to pre-process the
-- list. Just as with 'Yi.Rope.intersperse', it is up to the user to
-- pre-process the list.
intercalate :: YiString -> [YiString] -> YiString
intercalate _ [] = mempty
intercalate (YiString t') ts = YiString $ t' >< go ts
  where
    go []                = mempty
    go (YiString t : ts') = t >< t' >< go ts'

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
intersperse _ [] = mempty
intersperse c (t:ts) = t <> go ts
  where
    go [] = mempty
    go (t':ts') = (c `cons` t') <> go ts'

-- | Add a 'Char' in front of a 'YiString'.
--
-- We add the character to the front of the first chunk. This does
-- mean that a lot of 'cons' might result in an abnormally large first
-- chunk so if you have to do that, consider using 'append' instead..
cons :: Char -> YiString -> YiString
cons c (YiString t) = case viewl t of
  EmptyL -> Yi.Rope.singleton c
  Chunk !l x :< ts -> YiString $ Chunk (l + 1) (c `TX.cons` x) <| ts

-- | Add a 'Char' in the back of a 'YiString'.
--
-- We add the character to the end of the last chunk. This does mean
-- that a lot of 'snoc' might result in an abnormally large last chunk
-- so if you have to do that, consider using 'append' instead..
snoc :: YiString -> Char -> YiString
snoc (YiString t) c = case viewr t of
  EmptyR -> Yi.Rope.singleton c
  ts :> Chunk l x -> YiString $ ts |> Chunk (l + 1) (x `TX.snoc` c)

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
    dropNl (YiString t)  = case viewr t of
      EmptyR -> Yi.Rope.empty
      ts :> ch@(Chunk l tx) ->
        YiString $ ts |- if TX.null tx
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
lines' t = let (YiString f, YiString s) = splitAtLine' 0 t
           in if T.null s
              then if T.null f then [] else [YiString f]
              else YiString f : lines' (YiString s)

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
any p = go . fromRope
  where
    go x = case viewl x of
      EmptyL -> False
      Chunk _ t :< ts -> TX.any p t || go ts

-- | 'YiString' specialised @all@.
--
-- See the implementation note for 'Yi.Rope.any'.
all :: (Char -> Bool) -> YiString -> Bool
all p = go . fromRope
  where
    go x = case viewl x of
      EmptyL -> False
      Chunk _ t :< ts -> TX.all p t || go ts

-- | To serialise a 'YiString', we turn it into a regular 'String'
-- first.
instance Binary YiString where
  put = put . toString
  get = Yi.Rope.fromString <$> get

-- | Write a 'YiString' into the given file. It's up to the user to
-- handle exceptions.
writeFile :: FilePath -> YiString -> IO ()
writeFile f = TF.writeFile f . toText

-- | Reads file into the rope, using 'fromText'. It's up to the user
-- to handle exceptions.
readFile :: FilePath -> IO YiString
readFile f = fromText <$> TF.readFile f

-- | A version of 'readFile' which allows for arbitrary chunk size to
-- start with.
--
-- For example, @readFile' foo ((/ 2) . 'TX.length')@ would produce
-- chunks that are half the size of the read in text: whether that's a
-- good idea depends on situation.
--
-- Note that if this number ends up as @< 1@, 'defaultChunkSize' will
-- be used instead.
--
-- It's up to the user to handle exceptions.
readFile' :: FilePath -> (TX.Text -> Int) -> IO YiString
readFile' f l = do
  c <- TF.readFile f
  let l' = case l c of
        x | x < 1     -> defaultChunkSize
          | otherwise -> x
  return $ fromText' l' c

-- | Filters the characters from the underlying string.
--
-- >>> filter (/= 'a') "bac"
-- "bc"
filter :: (Char -> Bool) -> YiString -> YiString
filter p = YiString . go . fromRope
  where
    go t = case viewl t of
      EmptyL -> T.empty
      Chunk _ x :< ts -> mkChunk TX.length (TX.filter p x) -| go ts

-- | Maps the characters over the underlying string.
map :: (Char -> Char) -> YiString -> YiString
map f = YiString . go . fromRope
  where
    go t = case viewl t of
      EmptyL -> T.empty
      Chunk l x :< ts -> Chunk l (TX.map f x) <| go ts

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
split p = fmap fromText . TX.split p . toText

-- | Left fold.
--
-- Benchmarks show that folding is actually Pretty Damn Slow™: consider
-- whether folding is really the best thing to use in your scenario.
foldl' :: (a -> Char -> a) -> a -> YiString -> a
foldl' f a = go a . fromRope
  where
    go acc t = case viewl t of
      EmptyL -> acc
      Chunk _ x :< ts -> let r = TX.foldl' f acc x
                         in r `seq` go r ts

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
withText f = YiString . T.fmap' (mkChunk TX.length . f . _fromChunk) . fromRope

-- | Maps over each __chunk__ which means this function is UNSAFE! If
-- you use this with functions which don't preserve 'Size', that is
-- the chunk length and number of newlines, things will break really,
-- really badly. You should not need to use this.
--
-- Also see 'T.unsafeFmap'
unsafeWithText :: (TX.Text -> TX.Text) -> YiString -> YiString
unsafeWithText f = YiString . T.unsafeFmap g . fromRope
  where
    g (Chunk l t) = Chunk l (f t)
