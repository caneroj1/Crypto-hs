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
import Challenges.Challenge4
import Challenges.Challenge5
import Challenges.Challenge6

data Challenge =
  Challenge1 |
  Challenge2 |
  Challenge3 |
  Challenge4 |
  Challenge5 |
  Challenge6
  deriving Read

instance Solveable Challenge where
  solve Challenge1 = challenge1
  solve Challenge2 = challenge2
  solve Challenge3 = challenge3
  solve Challenge4 = challenge4
  solve Challenge5 = challenge5
  solve Challenge6 = challenge6
