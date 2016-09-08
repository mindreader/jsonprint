module Main where


import Data.Aeson (decode)

import qualified Data.ByteString.Lazy.Char8 as BS
import System.Environment

import Lib

main :: IO ()
main = do
  args <- getArgs

  let source = (if length args > 0 then args !! 0 else "-")
  txt <- if source == "-" then BS.getContents else BS.readFile source

  case decode txt of
    Nothing -> return ()
    Just val -> printValue val >>= BS.putStr
