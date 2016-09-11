{-# LANGUAGE OverloadedStrings #-}

module Challenges.Challenge2 where

import Lib (fixedXOR, hexToBytes)
import Data.Maybe (maybe)
import Data.ByteString (ByteString)

-- Challenge 2 is a fixed XOR
input :: ByteString
input = hexToBytes "1c0111001f010100061a024b53535009181c"

xor :: ByteString
xor = hexToBytes "686974207468652062756c6c277320657965"

output :: ByteString
output = hexToBytes "746865206b696420646f6e277420706c6179"

challenge2 :: IO ()
challenge2 = do
  putStrLn $ "Input: " ++ show input
  putStrLn $ "XOR:   " ++ show xor
  let xorred = input `fixedXOR` xor
  putStrLn $ "Expected: " ++ show output
  putStrLn $ "Got:      " ++ show xorred
  putStrLn $ "Equal:    " ++ show (maybe False (output ==) xorred)
