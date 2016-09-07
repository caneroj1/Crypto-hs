module Internal.TupleUtils
    ( tupleCons
    , tupleConcat
    , tupleConcat3
    ) where

tupleCons :: c -> (a, b) -> (c, a, b)
tupleCons c (a, b) = (c, a, b)

tupleConcat :: (a, b) -> (c, d) -> (a, b, c, d)
tupleConcat (a, b) (c, d) = (a, b, c, d)

tupleConcat3 :: (a, b, c) -> (d, e) -> (a, b, c, d, e)
tupleConcat3 (a, b, c) (d, e) = (a, b, c, d, e)
