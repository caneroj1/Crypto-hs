{-# LANGUAGE OverloadedStrings #-}

module Challenges.Challenge5 where

import Lib (repeatingKeyXOR)
import Data.HexString
import Control.Monad
import Data.Ord
import Data.Monoid
import Data.Text
import Data.ByteString (ByteString)

-- Challenge 3 is to break single-character XOR encryption
input :: ByteString
input = "Burning 'em, if you ain't quick and nimble\n" <>
        "I go crazy when I hear a cymbal"

output :: HexString
output = hexString $
  "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272" <>
  "a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"

key :: ByteString
key = "ICE"

challenge5 :: IO ()
challenge5 = do
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Key:   " ++ show key
  putStrLn "Encryting with repeating-key XOR"
  let encrypted = fromBytes $ key `repeatingKeyXOR` input
  putStrLn $ "Expected: " ++ show output
  putStrLn $ "Got:      " ++ show encrypted
  putStrLn $ "Equal:    " ++ show (encrypted == output)
