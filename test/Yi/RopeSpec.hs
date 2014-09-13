{-# LANGUAGE OverloadedStrings #-}
module Yi.RopeSpec (main, spec) where

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
