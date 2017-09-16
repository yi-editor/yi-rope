module Yi.Rope.Internal.ListLikeHelpers 
  ( chunksOf
  , split
  ) where

import qualified Data.ListLike as LL

chunksOf :: (LL.ListLike full item) => Int -> full -> [full]
chunksOf k = go
  where
    go t = case LL.splitAt k t of
             (a, b) | LL.null a    -> []
                    | otherwise    -> a : go b

split :: (LL.ListLike full item) => (item -> Bool) -> full -> [full]
split p t = loop t
    where loop s | LL.null s'   = [l]
                 | otherwise = l : loop (LL.tail s')
              where (l, s') = LL.span (not . p) s
