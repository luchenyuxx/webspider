{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Network.Wreq (get, responseBody)
import Control.Lens ((^.))
import Text.ParserCombinators.Parsec
import Control.Applicative ((*>), (<*))
import Data.ByteString.Lazy.Char8 (unpack)
import Data.List (group, sort, sortOn)

someFunc :: IO ()
someFunc = undefined

countWordsHTML :: String -> IO (Either ParseError [(Int, String)])
countWordsHTML u = do
  r <- get u 
  return $ fmap (sortOn fst . frequency) $ parse words' "error parsing html" (unpack $ r ^. responseBody)

findWords :: String -> Either ParseError [String]
findWords = parse words' "test" 

word = many1 letter

symbol = noneOf "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNMéêèçà"

symbols = many symbol

word' = symbols *> word <* symbols 

words' = many word'

frequency :: Ord s => [s] -> [(Int, s)]
frequency = map (\l -> (length l, head l)) . group . sort

tagStart = char '<' *> word

tagEnd = string "/>"

tag = tagStart *> (skipMany anyChar) <* tagEnd
