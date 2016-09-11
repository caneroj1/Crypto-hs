{-# LANGUAGE OverloadedStrings #-}

module Challenges.Challenge3 where

import Lib (breakCipher)
import Data.HexString
import Control.Monad
import Data.Ord

-- Challenge 3 is to break single-character XOR encryption
input :: HexString
input = hexString "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

challenge3 :: IO ()
challenge3 = do
  putStrLn $ "Input: " ++ show input
  forM_ (breakCipher input) $ \(bs, Down score) ->
    putStrLn $ "Decoded: " ++ show bs ++ ", Score: " ++ show score
