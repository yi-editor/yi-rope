{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Yi.RopeSpec (main, spec) where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck.Instances ()
import qualified Yi.Rope as F

main ∷ IO ()
main = hspec spec
{-
infixr 2 `isLike`
isLike :: (Show a, Eq a) => (R.Rope -> a) -> (F.YiString -> a)
       -> String -> Expectation
f `isLike` g = \s -> f (R.fromString s) `shouldBe` g (F.fromString s)

infixr 2 `stringIsLike`
stringIsLike :: (String -> R.Rope) -> (String -> F.YiString)
             -> String -> Expectation
f `stringIsLike` g = \s -> (R.toString . f $ s) `shouldBe` (F.toString . g $ s)

infixr 2 `sIsLike`
sIsLike :: (R.Rope -> R.Rope) -> (F.YiString -> F.YiString) -> String -> Expectation
f `sIsLike` g = R.toString . f `isLike` F.toString . g

infixr 2 `ssIsLike`

ssIsLike :: (R.Rope -> R.Rope) -> (F.YiString -> F.YiString) -> String -> Expectation
f `ssIsLike` g = \s ->
  (R.toString . f . R.fromString) s `shouldBe` (F.toString . g . F.fromString) s
-}
spec ∷ Spec
spec = modifyMaxSize (const 10000) $ do
  describe "tests" $ do
    it "" pending
{-
    prop "toString" $ R.toString `isLike` F.toString
    prop "toReverseString" $ R.toReverseString `isLike` F.toReverseString
    prop "null" $ R.null `isLike` F.null
    prop "empty" $ const R.empty `stringIsLike` const F.empty
    prop "take" $ \i -> R.take i `sIsLike` F.take i
    prop "drop" $ \i -> R.drop i `sIsLike` F.drop i
    prop "length" $ R.length `isLike` F.length
    prop "reverse" $ R.reverse `sIsLike` F.reverse
    prop "countNewLines" $ R.countNewLines `isLike` F.countNewLines
    -- prop "split" $ map R.toString . R.split 10 `isLike` map F.toString . F.lines
    prop "fst . splitAt" $ \i -> fst . R.splitAt i `sIsLike` fst . F.splitAt i
    prop "snd . splitAt" $ \i -> snd . R.splitAt i `sIsLike` snd . F.splitAt i
    prop "fst . splitAtLine"
      $ \i -> fst . R.splitAtLine i `sIsLike` fst . F.splitAtLine i
    prop "snd . splitAtLine"
      $ \i -> snd . R.splitAtLine i `sIsLike` snd . F.splitAtLine i
    modifyMaxSize (const 100) $ prop "append"
      $ \s -> R.append (R.fromString s) `ssIsLike` F.append (F.fromString s)
    prop "concat" $ \s -> (R.toString . R.concat . map R.fromString) s
                          `shouldBe`
                          (F.toString . F.concat . map F.fromString) s
-}
