module Lib where

import Network.Wreq
import Control.Lens
import Data.ByteString.Lazy.Internal (ByteString)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- testHTML :: IO (Response ByteString)
testHTML :: IO ByteString
testHTML = do
  r <- get "http://www.google.com"
  return $ r ^. responseBody
