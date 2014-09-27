{-# LANGUAGE OverloadedStrings #-}
module Yi.RopeSpec (main, spec) where

import           Control.Applicative
import           Data.Char (isUpper, toUpper, isSpace)
import qualified Data.Text as T
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck.Instances ()
import qualified Yi.Rope as R

main :: IO ()
main = hspec spec

infix 1 `isLike`
-- | Converts the input to R.YiString before comparing results.
isLike :: (Show a, Eq a) => (R.YiString -> a)
       -> (T.Text -> a)
       -> T.Text
       -> Expectation
f `isLike` g = \t -> (f . R.fromText) t `shouldBe` g t

infix 1 `isLikeT`
-- | Applies given function over underlying 'R.YiString'.
isLikeT :: (R.YiString -> R.YiString)
        -> (T.Text -> T.Text)
        -> T.Text
        -> Expectation
f `isLikeT` g = \t -> (R.toText . f . R.fromText) t `shouldBe` g t

spec :: Spec
spec = modifyMaxSize (const 1000) $ do
  describe "Working with YiString is just like working with Text" $ do
    prop "id ~ id" $ id `isLikeT` id
    prop "R.take ~ T.take" $ \i -> R.take i `isLikeT` T.take i
    prop "R.drop ~ T.drop" $ \i -> R.drop i `isLikeT` T.drop i
    prop "R.reverse ~ T.reverse" $ R.reverse `isLikeT` T.reverse
    prop "R.length ~ T.length" $ R.length `isLike` T.length
    prop "R.null ~ T.null" $ R.null `isLike` T.null
    prop "R.countNewLines ~ T.count \\n" $ R.countNewLines `isLike` T.count "\n"
    prop "R.empty ~ T.empty" $ R.toText R.empty `shouldBe` T.empty
    prop "fst . R.splitAt ~ fst . T.splitAt" $ \i ->
      fst . R.splitAt i `isLikeT` fst . T.splitAt i
    prop "snd . R.splitAt ~ snd . T.splitAt" $ \i ->
      snd . R.splitAt i `isLikeT` snd . T.splitAt i
    prop "R.append ~ T.append" $ \t ->
      R.append (R.fromText t) `isLikeT` T.append t
    prop "R.concat ~ T.concat" $ \s ->
      (R.toText . R.concat . map R.fromText) s `shouldBe` T.concat s
    prop "R.head ~ T.head" $ R.head `isLike` (\x -> if T.null x
                                                    then Nothing
                                                    else Just (T.head x))
    prop "R.last ~ T.last" $ R.last `isLike` (\x -> if T.null x
                                                    then Nothing
                                                    else Just (T.last x))
    prop "R.cons ~ T.cons" $ \c -> R.cons c `isLikeT` T.cons c
    prop "R.snoc ~ T.snoc" $ \c -> R.cons c `isLikeT` T.cons c
    prop "R.singleton ~ T.singleton" $
      \c -> (R.toText . R.singleton) c `shouldBe` T.singleton c
    prop "\\p -> R.any p ~ T.any p $ const True" $ \t ->
      R.any (const True) (R.fromText t) `shouldBe` T.any (const True) t
    prop "\\p -> R.any p ~ T.any p $ const False" $ \t ->
      R.any (const False) (R.fromText t) `shouldBe` T.any (const False) t
    prop "\\f -> R.withText ~ f $ T.toTitle" $
      R.withText T.toTitle `isLikeT` T.toTitle
    prop "\\p -> R.dropWhile p ~ T.dropWhile p $ isUpper" $
      R.dropWhile isUpper `isLikeT` T.dropWhile isUpper
    prop "\\p -> R.takeWhile p ~ T.takeWhile p $ isUpper" $
      R.takeWhile isUpper `isLikeT` T.takeWhile isUpper
    prop "R.compare ~ T.compare" $ \t t' ->
      compare (R.fromText t) (R.fromText t') `shouldBe` compare t t'
    prop "\\p -> R.filter p ~ T.filter p $ isUpper" $
      R.filter isUpper `isLikeT` T.filter isUpper
    prop "\\f -> R.map f ~ T.map f $ toUpper" $
      R.map toUpper `isLikeT` T.map toUpper
    prop "\\p -> R.dropWhileEnd p ~ T.dropWhileEnd p $ isSpace" $
      R.dropWhileEnd isSpace `isLikeT` T.dropWhileEnd isSpace
    prop "\\p -> R.takeWhileEnd p ~ rev . T.takeWhile p . rev $ isSpace" $
      R.takeWhileEnd isSpace
      `isLikeT` T.reverse . T.takeWhile isSpace . T.reverse
    prop "R.words ~ T.words" $ \t ->
      R.toText <$> R.words (R.fromText t) `shouldBe` T.words t
    prop "R.unwords ~ T.unwords" $ \t ->
      R.toText (R.unwords (R.fromText <$> t)) `shouldBe` T.unwords t
    prop "\\p -> R.split p ~ T.split p $ isUpper" $ \t ->
      R.toText <$> R.split isUpper (R.fromText t) `shouldBe` T.split isUpper t
    prop "non-empty s ⊢ R.init s ~ T.init s" $ \t ->
      let t' = t `T.snoc` 'a' -- ensure non-empty
      in (fmap R.toText . R.init . R.fromText) t' `shouldBe` (Just . T.init) t'
    prop "non-empty s ⊢ R.tail s ~ T.tail s" $ \t ->
      let t' = t `T.snoc` 'a'
      in (fmap R.toText . R.tail . R.fromText) t' `shouldBe` (Just . T.tail) t'
