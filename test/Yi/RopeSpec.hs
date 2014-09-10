{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Yi.RopeSpec (main, spec) where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck.Instances ()
import qualified Yi.OldRope as O
import qualified Yi.Rope as R

main ∷ IO ()
main = hspec spec

infixr 2 `isLike`
isLike :: (Show a, Eq a) => (R.Rope -> a) -> (O.Rope -> a)
       -> String -> Expectation
f `isLike` g = \s -> f (R.fromString s) `shouldBe` g (O.fromString s)

infixr 2 `stringIsLike`
stringIsLike :: (String -> R.Rope) -> (String -> O.Rope)
             -> String -> Expectation
f `stringIsLike` g = \s -> (R.toString . f $ s) `shouldBe` (O.toString . g $ s)

infixr 2 `sIsLike`
sIsLike :: (R.Rope -> R.Rope) -> (O.Rope -> O.Rope) -> String -> Expectation
f `sIsLike` g = R.toString . f `isLike` O.toString . g

infixr 2 `ssIsLike`

ssIsLike :: (R.Rope -> R.Rope) -> (O.Rope -> O.Rope) -> String -> Expectation
f `ssIsLike` g = \s ->
  (R.toString . f . R.fromString) s `shouldBe` (O.toString . g . O.fromString) s

spec ∷ Spec
spec = do
  describe "Comparisons" $ do
    prop "toString" $ R.toString `isLike` O.toString
    prop "toReverseString" $ R.toReverseString `isLike` O.toReverseString
    prop "null" $ R.null `isLike` O.null
    prop "empty" $ const R.empty `stringIsLike` const O.empty
    prop "take" $ \i -> R.take i `sIsLike` O.take i
    prop "drop" $ \i -> R.drop i `sIsLike` O.drop i
    prop "length" $ R.length `isLike` O.length
    prop "reverse" $ R.reverse `sIsLike` O.reverse
    prop "countNewLines" $ R.countNewLines `isLike` O.countNewLines
    prop "split"
      $ \i -> map R.toString . R.split i `isLike` map O.toString . O.split i
    prop "fst . splitAt" $ \i -> fst . R.splitAt i `sIsLike` fst . O.splitAt i
    prop "snd . splitAt" $ \i -> snd . R.splitAt i `sIsLike` snd . O.splitAt i
    prop "fst . splitAtLine"
      $ \i -> fst . R.splitAtLine i `sIsLike` fst . O.splitAtLine i
    prop "snd . splitAtLine"
      $ \i -> snd . R.splitAtLine i `sIsLike` snd . O.splitAtLine i
    prop "append"
      $ \s -> R.append (R.fromString s) `ssIsLike` O.append (O.fromString s)
    prop "concat" $ \s -> (R.toString . R.concat . map R.fromString) s
                          `shouldBe`
                          (O.toString . O.concat . map O.fromString) s
