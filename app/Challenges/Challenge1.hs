{-# LANGUAGE OverloadedStrings #-}

module Challenges.Challenge1 where

import Lib (hexToBytes, toBase64)
import Data.Maybe (fromJust)
import Data.ByteString (ByteString)

-- Challenge 1 is convert hex to base64.
input :: ByteString
input = hexToBytes "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

output :: ByteString
output = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

challenge1 :: IO ()
challenge1 = do
  putStrLn $ "Input: " ++ show input
  putStrLn "Encoding to Base64"
  let encoded = toBase64 input
  let out     = show encoded
  putStrLn $ "Expected: " ++ show output
  putStrLn $ "Got:      " ++ out
  putStrLn $ "Equal:    " ++ show (out == show output)
