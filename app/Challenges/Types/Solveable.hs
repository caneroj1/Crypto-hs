module Challenges.Types.Solveable
(
  Solveable(..)
) where


class Solveable a where
  solve :: a -> IO ()
