{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Network.Wreq (get, responseBody)
import Control.Lens ((^.))
import Text.ParserCombinators.Parsec
import Control.Applicative ((*>), (<*))
import Data.ByteString.Lazy.Char8 (unpack)
import Data.List (group, sort, sortOn)
import Text.HTML.TagSoup
import Text.StringLike
import Control.Arrow ((&&&))
import Data.Char (toLower)

someFunc :: IO ()
someFunc = undefined

loadContent :: String -> IO (Either ParseError [String])
loadContent s = parse words' "" <$> renderTags <$> onlyText <$> excludeScript <$> getBody <$> parseTags <$> loadURL s

loadURL u = do
  r <- get u
  return $ unpack $ r ^. responseBody

getBody :: [Tag String] -> [Tag String]
getBody = takeWhile (~/= ("</body>"::String)) . dropWhile (~/= ("<body>"::String)) 

excludeScript ts = takeWhile (~/= ("<script>"::String)) ts ++ dropWhile (~/= ("</script>"::String)) ts

onlyText = filter isTagText 

countWordsHTML :: String -> IO (Either ParseError [(Int, String)])
countWordsHTML u = do
  r <- get u 
  return $ sortOn fst . frequency <$> parse words' "error parsing html" (unpack $ r ^. responseBody)

findWords :: String -> Either ParseError [String]
findWords = parse words' "test" 

word = many1 letter

symbol = noneOf "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNMéêèçà"

symbols = many symbol

word' = symbols *> word <* symbols 

words' = many word'

frequency :: [String] -> [(Int, String)]
frequency = sortOn fst . map (length &&& head) . group . sort

frequencyIgnoreCase :: [String] -> [(Int, String)]
frequencyIgnoreCase = frequency . fmap lowerCase

lowerCase :: String -> String
lowerCase = fmap toLower

tagStart = char '<' *> word

tagEnd = string "/>"

tag = tagStart *> skipMany anyChar <* tagEnd
