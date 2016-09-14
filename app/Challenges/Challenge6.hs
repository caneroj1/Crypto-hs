module Challenges.Challenge6 where

--import Lib (breakCipherRepeatingXOR, fromBase64, alreadyEncoded)
import System.Environment
import Data.Maybe
import System.Exit
import System.IO (IOMode(ReadMode), openFile, Handle, hIsEOF)
import qualified Data.ByteString as BS
import Control.Monad
import Data.Char
import Data.Word

secondArg :: [String] -> Maybe String
secondArg (_:fp:_) = Just fp
secondArg _        = Nothing

removeLines :: BS.ByteString -> BS.ByteString
removeLines = BS.filter nonWSDelim
  where newLineByte = fromIntegral $ ord '\n'
        cReturn    = fromIntegral $ ord '\r'
        nonWSDelim x = x /= newLineByte && x /= cReturn

output :: [BS.ByteString] -> IO ()
output = mapM_ (\x -> putStrLn $ "Decoded:\n-------\n" ++ show x)

challenge6 :: IO ()
challenge6 = return ()--do
  -- mbArg <- secondArg <$> getArgs
  -- when (isNothing mbArg) $ do
  --   putStrLn "No second argument supplied. Need filepath"
  --   exitSuccess
  -- openFile (fromJust mbArg) ReadMode >>=
  --   BS.hGetContents                  >>=
  --     \x ->
  --       (output . breakCipherRepeatingXOR)
  --         (fromBase64 . alreadyEncoded $ removeLines x)
