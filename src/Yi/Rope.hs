module Yi.Rope (Rope, fromString, toString, toReverseString, null, empty,
                take, drop, Data.Rope.length, reverse, countNewLines,
                Yi.Rope.split, Yi.Rope.splitAt, Yi.Rope.splitAtLine,
                Yi.Rope.append, Yi.Rope.concat, Yi.Rope.readFile,
                Yi.Rope.writeFile, Yi.Rope.splitAtChunkBefore) where

import qualified Codec.Binary.UTF8.Generic as G
import           Data.Binary
import qualified Data.ByteString.Lazy as LB (readFile, split, count, reverse)
import           Data.Monoid
import           Data.Rope
import qualified Prelude as P
import           Prelude hiding (null, take, drop, reverse)
import           System.IO.Cautious (writeFileL)

toReverseString :: Rope -> String
toReverseString = P.reverse . toString

reverse :: Rope -> Rope
reverse = fromLazyByteString . LB.reverse . toLazyByteString

countNewLines :: Rope -> Int
countNewLines = fromIntegral . LB.count 10 . toLazyByteString

split :: Word8 -> Rope -> [Rope]
split c = map fromLazyByteString . LB.split c . toLazyByteString

splitAt :: Int -> Rope -> (Rope, Rope)
splitAt = G.splitAt

splitAtChunkBefore :: Int -> Rope -> (Rope, Rope)
splitAtChunkBefore = Yi.Rope.splitAt

-- | Split before the specified line. Lines are indexed from 0.
splitAtLine :: Int -> Rope -> (Rope, Rope)
splitAtLine n r | n <= 0     = (mempty, r)
                | otherwise = splitAtLine' (n - 1) r

-- | Split after the specified line. Lines are indexed from 0.
splitAtLine' :: Int -> Rope -> (Rope, Rope)
splitAtLine' n r = let ls = P.take (n + 1) (G.lines' r)
                   in G.splitAt (sum $ map G.length ls) r

append :: Rope -> Rope -> Rope
append = (<>)

concat :: [Rope] -> Rope
concat = mconcat

writeFile :: FilePath -> Rope -> IO ()
writeFile f = writeFileL f . toLazyByteString

readFile :: FilePath -> IO Rope
readFile f = fromLazyByteString `fmap` LB.readFile f
