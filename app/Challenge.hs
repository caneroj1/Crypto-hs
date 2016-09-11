module Challenge
(
  Challenge(..)
, Solveable
, solve
)
where

import Challenges.Types.Solveable
import Challenges.Challenge1
import Challenges.Challenge2
import Challenges.Challenge3

data Challenge =
  Challenge1 |
  Challenge2 |
  Challenge3
  deriving Read

instance Solveable Challenge where
  solve Challenge1 = challenge1
  solve Challenge2 = challenge2
  solve Challenge3 = challenge3