{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.DeepSeq
import           Criterion.Main
import qualified Criterion.Main as C
import           Data.Text (unlines, Text, replicate, unpack)
import           Prelude hiding (unlines)
import qualified Yi.Rope as F


longText :: Text
longText = force . Data.Text.unlines
         $ Prelude.replicate 1000 "Lorem Спасибопожалусто dolor 中文測試 amet"
{-# NOINLINE longText #-}

longTextTree :: F.YiString
longTextTree = force . F.fromText . Data.Text.unlines
               $ Prelude.replicate 1000 "Lorem Спасибопожалусто dolor 中文測試 amet"
{-# NOINLINE longTextTree #-}

longFRope :: F.YiString
longFRope = force (F.fromText longText)
{-# NOINLINE longFRope #-}

wideText :: Text
wideText = force . unlines
         $ Prelude.replicate 10
         $ Data.Text.replicate 100 "Lorem Спасибопожалусто dolor 中文測試 amet "
{-# NOINLINE wideText #-}

shortText :: Text
shortText = force . unlines
         $ Prelude.replicate 3 "Lorem Спасибопожалусто dolor 中文測試 amet"
{-# NOINLINE shortText #-}

tinyText :: Text
tinyText = force $ "Lorem Спасибопожалусто dolor 中文測試 amet"
{-# NOINLINE tinyText #-}

wideFRope :: F.YiString
wideFRope = force (F.fromText wideText)
{-# NOINLINE wideFRope #-}

benchOnText :: NFData b => a -> String -> (a -> b) -> Benchmark
benchOnText text name f
  = C.bench name
  $ C.nf f text

benchSplitAt :: NFData a => a -> String
             -> (Int -> a -> (a, a))
             -> C.Benchmark
benchSplitAt text name f
    = C.bench name
    $ C.nf (\x -> Prelude.foldr ((fst .) . f) x [1000, 999 .. 1]) text

benchTakeDrop :: NFData a => a -> String -> (Int -> a -> a) -> C.Benchmark
benchTakeDrop text name f
    = C.bench name
    $ C.nf (\x -> foldr f x [1000, 999 .. 1]) text

-- | Chunk sizes to test with.
chunkSizes :: [Int]
chunkSizes = [1200]

wideTexts :: (Int -> String, [(Int, F.YiString)])
wideTexts = (\x -> "wide " ++ show x, mkTextSample wideText)

longTexts :: (Int -> String, [(Int, F.YiString)])
longTexts = (\x -> "long " ++ show x, mkTextSample longText)

shortTexts :: (Int -> [Char], [(Int, F.YiString)])
shortTexts = (\x -> "short " ++ show x, mkTextSample shortText)

tinyTexts :: (Int -> String, [(Int, F.YiString)])
tinyTexts = (\x -> "tiny " ++ show x, mkTextSample tinyText)

mkTextSample :: Text -> [(Int, F.YiString)]
mkTextSample s = force $ zipWith mkTexts chunkSizes (Prelude.repeat s)
  where
    mkTexts :: Int -> Text -> (Int, F.YiString)
    mkTexts x t = (x, F.fromText' x t)

allTexts :: [(Int -> String, [(Int, F.YiString)])]
allTexts = [longTexts {-, wideTexts, shortTexts, tinyTexts -}]

allChars :: [(Int -> String, [(Int, Char)])]
allChars = map mkChar "λa"
  where
    mkChar c = (\x -> unwords [ "char", [c], show x ], [(1, c)])

-- | Sample usage:
--
-- > mkGroup "drop" F.drop allTexts benchOnText
mkGroup :: String -- ^ Group name
        -> f -- ^ Function being benchmarked
        -> [(chsize -> String, [(chsize, input)])]
        -> (input -> String -> f -> Benchmark)
        -> Benchmark
mkGroup n f subs r = bgroup n tests
  where
    mkTest s (l, t) = r t (s l) f
    tests = Prelude.concat $ map (\(s, t) -> map (mkTest s) t) subs

onTextGroup :: NFData a => String -> (F.YiString -> a) -> Benchmark
onTextGroup n f = mkGroup n f allTexts benchOnText

onCharGroup :: NFData a => String -> (Char -> a) -> Benchmark
onCharGroup n f = mkGroup n f allChars benchOnText

onIntGroup :: String -> (Int -> F.YiString -> F.YiString) -> Benchmark
onIntGroup n f = mkGroup n f allTexts benchTakeDrop

onSplitGroup :: String
             -> (Int -> F.YiString -> (F.YiString, F.YiString))
             -> Benchmark
onSplitGroup n f = mkGroup n f allTexts benchSplitAt

main :: IO ()
main = defaultMain
  [ onIntGroup "drop" F.drop
  , onIntGroup "take" F.take
  , onTextGroup "cons" (F.cons 'λ')
  , onTextGroup "snoc" (`F.snoc` 'λ')
  , onCharGroup "singleton" F.singleton
  , onTextGroup "countNewLines" F.countNewLines
  , onTextGroup "lines" F.lines
  , onSplitGroup "splitAt" F.splitAt
  , onSplitGroup "splitAtLine" F.splitAtLine
  , onTextGroup "toReverseString" F.toReverseString
  , onTextGroup "toReverseText" F.toReverseText
  , onTextGroup "toText" F.toText
  , onTextGroup "length" F.length
  , onTextGroup "reverse" F.reverse
  , onTextGroup "null" F.null
  , onTextGroup "empty" $ const F.empty
  , onTextGroup "append" (\x -> F.append x x)
  , onTextGroup "concat x100" $ F.concat . Prelude.replicate 100
  , onTextGroup "any OK, (== '中')" $ F.any (== '中')
  , onTextGroup "any bad, (== '×')" $ F.any (== '×')
  , onTextGroup "all OK (/= '×')" $ F.all (== '×')
  , onTextGroup "all bad, (== '中')" $ F.all (== '中')
  ]
