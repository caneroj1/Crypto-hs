module Internal.TupleUtils
    ( tupleCons
    , tupleConcat
    , tupleConcat3
    , flatten
    , trd
    , fst3
    ) where

tupleCons :: c -> (a, b) -> (c, a, b)
tupleCons c (a, b) = (c, a, b)

tupleConcat :: (a, b) -> (c, d) -> (a, b, c, d)
tupleConcat (a, b) (c, d) = (a, b, c, d)

tupleConcat3 :: (a, b, c) -> (d, e) -> (a, b, c, d, e)
tupleConcat3 (a, b, c) (d, e) = (a, b, c, d, e)

flatten :: ((a, b), c) -> (a, b, c)
flatten ((a, b), c) = (a, b, c)

trd :: (a, b, c) -> c
trd     (_, _, c)   = c

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a
