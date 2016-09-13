{-# LANGUAGE OverloadedStrings #-}

module Challenges.Challenge4 where

import Lib (breakCipher)
import Data.HexString
import System.Environment
import Data.Maybe
import System.Exit
import System.IO (IOMode(ReadMode), openFile, Handle, hIsEOF)
import qualified Data.ByteString as BS
import Data.Ord
import Control.Monad

secondArg :: [String] -> Maybe String
secondArg (_:fp:_) = Just fp
secondArg _        = Nothing

eachLine :: Handle -> IO ()
eachLine h = do
  eof <- hIsEOF h
  unless eof $ do
    mapM_ print =<< (breakCipher . hexString . BS.init) <$> BS.hGetLine h
    putStrLn ""
    eachLine h

challenge4 :: IO ()
challenge4 = do
  mbArg <- secondArg <$> getArgs
  when (isNothing mbArg) $ do
    putStrLn "No second argument supplied. Need filepath"
    exitSuccess
  eachLine =<< openFile (fromJust mbArg) ReadMode
