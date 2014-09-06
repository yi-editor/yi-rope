module Main where

import qualified Yi.Rope as R
import qualified Yi.OldRope as O
import Criterion.Main
import qualified Criterion.Main as C
import Control.DeepSeq
import Data.List (foldl')

type Bench a = Input -> Name -> a -> C.Benchmark
type Input = String
type Name = String

benchOnText :: NFData b => a -> String -> (a -> b) -> Benchmark
benchOnText text name f
  = C.bench name
  $ C.nf f text

linesplitbench :: IO ()
linesplitbench = defaultMain
  [
  --   benchOnText longORope "long O.splitAtLine 10" (O.splitAtLine 10)
  -- , benchOnText longRRope "long R.splitAtLine 10" (R.splitAtLine 10)
  -- , benchOnText wideORope "wide O.splitAtLine 10" (O.splitAtLine 10)
  -- , benchOnText wideRRope "wide R.splitAtLine 10" (R.splitAtLine 10)
  -- , benchOnText longORope "long O.splitAtLine 100" (O.splitAtLine 100)
  -- , benchOnText longRRope "long R.splitAtLine 100" (R.splitAtLine 100)
  -- , benchOnText wideORope "wide O.splitAtLine 100" (O.splitAtLine 100)
  -- ,
    benchOnText wideRRope "wide R.splitAtLine 100" (R.splitAtLine 100)
  ]

lengthbench :: IO ()
lengthbench = defaultMain
  [ benchOnText longORope "long O.length" O.length
  , benchOnText longRRope "long R.length" R.length
  , benchOnText wideORope "wide O.length" O.length
  , benchOnText wideRRope "wide R.length" R.length
  ]

main :: IO ()
main = defaultMain
  [ benchOnText longORope "long O.countNewLines" O.countNewLines
  , benchOnText longRRope "long R.countNewLines" R.countNewLines
  , benchOnText wideORope "wide O.countNewLines" O.countNewLines
  , benchOnText wideRRope "wide R.countNewLines" R.countNewLines
  , benchOnText longORope "long O.split \\n" (O.split 10)
  , benchOnText longRRope "long R.split \\n" (R.split 10)
  , benchOnText wideORope "wide O.split \\n" (O.split 10)
  , benchOnText wideRRope "wide R.split \\n" (R.split 10)
  , benchOnText longORope "long O.splitAt 5" (O.splitAt 5)
  , benchOnText longRRope "long R.splitAt 5" (R.splitAt 5)
  , benchOnText wideORope "wide O.splitAt 5" (O.splitAt 5)
  , benchOnText wideRRope "wide R.splitAt 5" (R.splitAt 5)
  , benchOnText longORope "long O.splitAt 700" (O.splitAt 700)
  , benchOnText longRRope "long R.splitAt 700" (R.splitAt 700)
  , benchOnText wideORope "wide O.splitAt 700" (O.splitAt 700)
  , benchOnText wideRRope "wide R.splitAt 700" (R.splitAt 700)
  , benchOnText longORope "long O.splitAtLine 10" (O.splitAtLine 10)
  , benchOnText longRRope "long R.splitAtLine 10" (R.splitAtLine 10)
  , benchOnText wideORope "wide O.splitAtLine 10" (O.splitAtLine 10)
  , benchOnText wideRRope "wide R.splitAtLine 10" (R.splitAtLine 10)
  , benchOnText longORope "long O.splitAtLine 100" (O.splitAtLine 100)
  , benchOnText longRRope "long R.splitAtLine 100" (R.splitAtLine 100)
  , benchOnText wideORope "wide O.splitAtLine 100" (O.splitAtLine 100)
  , benchOnText wideRRope "wide R.splitAtLine 100" (R.splitAtLine 100)
  , benchOnText longORope "long O.drop 5" (O.drop 5)
  , benchOnText longRRope "long R.drop 5" (R.drop 5)
  , benchOnText wideORope "wide O.drop 5" (O.drop 5)
  , benchOnText wideRRope "wide R.drop 5" (R.drop 5)
  , benchOnText longORope "long O.drop 150" (O.drop 150)
  , benchOnText longRRope "long R.drop 150" (R.drop 150)
  , benchOnText wideORope "wide O.drop 150" (O.drop 150)
  , benchOnText wideRRope "wide R.drop 150" (R.drop 150)
  , benchOnText longORope "long O.take 5" (O.take 5)
  , benchOnText longRRope "long R.take 5" (R.take 5)
  , benchOnText wideORope "wide O.take 5" (O.take 5)
  , benchOnText wideRRope "wide R.take 5" (R.take 5)
  , benchOnText longORope "long O.take 150" (O.take 150)
  , benchOnText longRRope "long R.take 150" (R.take 150)
  , benchOnText wideORope "wide O.take 150" (O.take 150)
  , benchOnText wideRRope "wide R.take 150" (R.take 150)
  , benchOnText longORope "long O.toReverseString" O.toReverseString
  , benchOnText longRRope "long R.toReverseString" R.toReverseString
  , benchOnText wideORope "wide O.toReverseString" O.toReverseString
  , benchOnText wideRRope "wide R.toReverseString" R.toReverseString
  , benchOnText longORope "long O.toString" O.toString
  , benchOnText longRRope "long R.toString" R.toString
  , benchOnText wideORope "wide O.toString" O.toString
  , benchOnText wideRRope "wide R.toString" R.toString
  , benchOnText longORope "long O.null" O.null
  , benchOnText longRRope "long R.null" R.null
  , benchOnText wideORope "wide O.null" O.null
  , benchOnText wideRRope "wide R.null" R.null
  , benchOnText longORope "long O.empty" (const O.empty)
  , benchOnText longRRope "long R.empty" (const R.empty)
  , benchOnText wideORope "wide O.empty" (const O.empty)
  , benchOnText wideRRope "wide R.empty" (const R.empty)
  , benchOnText longORope "long O.length" O.length
  , benchOnText longRRope "long R.length" R.length
  , benchOnText wideORope "wide O.length" O.length
  , benchOnText wideRRope "wide R.length" R.length
  , benchOnText longORope "long O.reverse" O.reverse
  , benchOnText longRRope "long R.reverse" R.reverse
  , benchOnText wideORope "wide O.reverse" O.reverse
  , benchOnText wideRRope "wide R.reverse" R.reverse
  , benchOnText longORope "long O.append" (\x -> O.append x x)
  , benchOnText longRRope "long R.append" (\x -> R.append x x)
  , benchOnText wideORope "wide O.append" (\x -> O.append x x)
  , benchOnText wideRRope "wide R.append" (\x -> R.append x x)
  , benchOnText longORope "long O.concat 10" (\x -> O.concat (replicate 10 x))
  , benchOnText longRRope "long R.concat 10" (\x -> R.concat (replicate 10 x))
  , benchOnText wideORope "wide O.concat 10" (\x -> O.concat (replicate 10 x))
  , benchOnText wideRRope "wide R.concat 10" (\x -> R.concat (replicate 10 x))
  , benchOnText longORope "long O.concat 100" (\x -> O.concat (replicate 100 x))
  , benchOnText longRRope "long R.concat 100" (\x -> R.concat (replicate 100 x))
  , benchOnText wideORope "wide O.concat 100" (\x -> O.concat (replicate 100 x))
  , benchOnText wideRRope "wide R.concat 100" (\x -> R.concat (replicate 100 x))
  ]

instance NFData R.Rope where
  rnf r = R.toString r `deepseq` ()

instance NFData O.Rope where
  rnf r = O.toString r `deepseq` ()

longText :: String
longText = force . unlines
         $ replicate 1000 "Lorem Спасибопожалусто dolor 中文測試 amet"
{-# NOINLINE longText #-}

longRRope :: R.Rope
longRRope = force (R.fromString longText)
{-# NOINLINE longRRope #-}

longORope :: O.Rope
longORope = force (O.fromString longText)
{-# NOINLINE longORope #-}

wideText :: String
wideText = force . unlines
         $ replicate 10 . concat
         $ replicate 100 "Lorem Спасибопожалусто dolor 中文測試 amet "
{-# NOINLINE wideText #-}

wideORope :: O.Rope
wideORope = force (O.fromString wideText)
{-# NOINLINE wideORope #-}

wideRRope :: R.Rope
wideRRope = force (R.fromString wideText)
{-# NOINLINE wideRRope #-}
