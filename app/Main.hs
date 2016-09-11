{-# LANGUAGE OverloadedStrings #-}

module Main where

import Challenge
import Data.Maybe
import Text.Read
import System.Environment
import Control.Monad
import System.Exit

main :: IO ()
main = do
  mbStr <- listToMaybe <$> getArgs
  when (isNothing mbStr) $ do
    putStrLn "No args supplied"
    exitSuccess
  let arg = readMaybe (fromJust mbStr) :: Maybe Challenge
  case arg of
    Nothing -> putStrLn "No challenge specified"
    Just ch -> solve ch
