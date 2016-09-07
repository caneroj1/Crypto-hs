module Internal.Uncons
    ( uncons4
    , uncons3
    , uncons2
    ) where

import qualified Data.ByteString as BS
import Internal.TupleUtils
import Data.Word

uncons4 :: BS.ByteString -> Maybe (Word8, Word8, Word8, Word8, BS.ByteString)
uncons4 bs = uncons3 bs >>= (\(h, hh, hhh, t) ->
                              tupleConcat3 (h, hh, hhh) <$> BS.uncons t)

uncons3 :: BS.ByteString -> Maybe (Word8, Word8, Word8, BS.ByteString)
uncons3 bs = uncons2 bs >>= (\(h, hh, t) -> tupleConcat (h, hh) <$> BS.uncons t)

uncons2 :: BS.ByteString -> Maybe (Word8, Word8, BS.ByteString)
uncons2 bs = BS.uncons bs >>= (\(h, t) -> tupleCons h <$> BS.uncons t)
