{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.DeepSeq
import           Criterion.Main
import qualified Criterion.Main as C
import           Data.Text (unlines, length, pack, unpack,
                            filter, count, lines, Text,
                            concat, replicate)
import qualified Yi.FastRope as F
import qualified Yi.OldRope as O
import Prelude hiding (unlines)

type Bench a = Input -> Name -> a -> C.Benchmark
type Input = String
type Name = String

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

longORope :: O.Rope
longORope = force (O.fromString $ Data.Text.unpack longText)
{-# NOINLINE longORope #-}

wideText :: Text
wideText = force . unlines
         $ Prelude.replicate 10
         $ Data.Text.replicate 100 "Lorem Спасибопожалусто dolor 中文測試 amet "
{-# NOINLINE wideText #-}

wideORope :: O.Rope
wideORope = force (O.fromString $ Data.Text.unpack wideText)
{-# NOINLINE wideORope #-}


wideFRope :: F.YiString
wideFRope = force (F.fromText wideText)
{-# NOINLINE wideFRope #-}

instance NFData O.Rope where
  rnf r = O.toString r `deepseq` ()


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

main :: IO ()
main = defaultMain
  [ benchOnText longORope "long O.countNewLines" O.countNewLines
  , benchOnText longFRope "long F.countNewLines" F.countNewLines
  , benchOnText wideORope "wide O.countNewLines" O.countNewLines
  , benchOnText wideFRope "wide F.countNewLines" F.countNewLines
  , benchOnText longORope "long O.split \\n" (O.split 10)
  , benchOnText longFRope "long F.lines" F.lines
  , benchOnText wideORope "wide O.split \\n" (O.split 10)
  , benchOnText wideFRope "wide F.lines" F.lines
  , benchSplitAt longORope "varied long O.splitAt" O.splitAt
  , benchSplitAt longFRope "varied long F.splitAt" F.splitAt
  , benchSplitAt wideORope "varied wide O.splitAt" O.splitAt
  , benchSplitAt wideFRope "varied wide F.splitAt" F.splitAt
  , benchSplitAt longORope "varied long O.splitAtLine" O.splitAtLine
  , benchSplitAt longFRope "varied long F.splitAtLine" F.splitAtLine
  , benchSplitAt wideORope "varied wide O.splitAtLine" O.splitAtLine
  , benchSplitAt wideFRope "varied wide F.splitAtLine" F.splitAtLine
  , benchTakeDrop longORope "long O.drop" O.drop
  , benchTakeDrop longFRope "long F.drop" F.drop
  , benchTakeDrop wideORope "wide O.drop" O.drop
  , benchTakeDrop wideFRope "wide F.drop" F.drop
  , benchTakeDrop longORope "long O.take" O.take
  , benchTakeDrop longFRope "long F.take" F.take
  , benchTakeDrop wideORope "wide O.take" O.take
  , benchTakeDrop wideFRope "wide F.take" F.take
  , benchOnText longORope "long O.toReverseString" O.toReverseString
  , benchOnText longFRope "long F.toReverseText" F.toReverseText
  , benchOnText wideORope "wide O.toReverseString" O.toReverseString
  , benchOnText wideFRope "wide F.toReverseText" F.toReverseText
  , benchOnText longORope "long O.toString" O.toString
  , benchOnText longFRope "long F.toText" F.toText
  , benchOnText wideORope "wide O.toString" O.toString
  , benchOnText wideFRope "wide F.toText" F.toText
  , benchOnText longORope "long O.null" O.null
  , benchOnText longFRope "long F.null" F.null
  , benchOnText wideORope "wide O.null" O.null
  , benchOnText wideFRope "wide F.null" F.null
  , benchOnText longORope "long O.empty" (const O.empty)
  , benchOnText longFRope "long F.empty" (const F.empty)
  , benchOnText wideORope "wide O.empty" (const O.empty)
  , benchOnText wideFRope "wide F.empty" (const F.empty)
  , benchOnText longORope "long O.length" O.length
  , benchOnText longFRope "long F.length" F.length
  , benchOnText wideORope "wide O.length" O.length
  , benchOnText wideFRope "wide F.length" F.length
  , benchOnText longORope "long O.reverse" O.reverse
  , benchOnText longFRope "long F.reverse" F.reverse
  , benchOnText wideORope "wide O.reverse" O.reverse
  , benchOnText wideFRope "wide F.reverse" F.reverse
  , benchOnText longORope "long O.append" (\x -> O.append x x)
  , benchOnText longFRope "long F.append" (\x -> F.append x x)
  , benchOnText wideORope "wide O.append" (\x -> O.append x x)
  , benchOnText wideFRope "wide F.append" (\x -> F.append x x)
  , benchOnText longORope "long O.concat 10" (\x -> O.concat (Prelude.replicate 10 x))
  , benchOnText longFRope "long F.concat 10" (\x -> F.concat (Prelude.replicate 10 x))
  , benchOnText wideORope "wide O.concat 10" (\x -> O.concat (Prelude.replicate 10 x))
  , benchOnText wideFRope "wide F.concat 10" (\x -> F.concat (Prelude.replicate 10 x))
  , benchOnText longORope "long O.concat 100" (\x -> O.concat (Prelude.replicate 100 x))
  , benchOnText longFRope "long F.concat 100" (\x -> F.concat (Prelude.replicate 100 x))
  , benchOnText wideORope "wide O.concat 100" (\x -> O.concat (Prelude.replicate 100 x))
  , benchOnText wideFRope "wide F.concat 100" (\x -> F.concat (Prelude.replicate 100 x))
  ]
