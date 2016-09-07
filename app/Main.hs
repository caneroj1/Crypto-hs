{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import qualified Data.ByteString as BS
import Data.Monoid

text :: BS.ByteString
text = "Man is distinguished, not only by his reason, but by this singular passion from" <>
       "other animals, which is a lust of the mind, that by a perseverance of delight"   <>
       "in the continued and indefatigable generation of knowledge, exceeds the short"   <>
       "vehemence of any carnal pleasure."

main :: IO ()
main = print $ toBase64 text
