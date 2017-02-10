{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Control.Exception (handle, SomeException)
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
import qualified Data.Map.Strict as Map

someFunc :: IO ()
someFunc = undefined

-- models
type Link = String

type WordFrequency = Map.Map String Int

emptyWordFrequency = Map.empty

-- tests
testCountWordsHTML = countWordsHTML "https://en.wikipedia.org/wiki/Wikipedia"

testLoadContent = loadContent "https://en.wikipedia.org/wiki/Wikipedia"

testLoadLink = findLinks <$> testLoadTags

testLoadTags = loadTags "https://en.wikipedia.org/wiki/Wikipedia"

-- IOs
deepCountWordsHTML :: Link -> IO WordFrequency
deepCountWordsHTML = undefined
                   
loadContent :: Link -> IO (Either ParseError [String])
loadContent s = parse words' "Can't parse words" <$> renderTags <$> onlyText <$> excludeScript <$> getBody <$> loadTags s

loadURL :: Link -> IO [Char]
loadURL u = do
  r <- get u
  handle errorHandler $ return $ unpack $ r ^. responseBody
  where
    errorHandler :: SomeException -> IO [Char]
    errorHandler _ = return []

loadTags :: Link -> IO [Tag String]
loadTags = fmap parseTags . loadURL

countWordsTags :: [Tag String] -> Either ParseError WordFrequency
countWordsTags = fmap frequencyIgnoreCase . parse words' "Can't parse words" . renderTags . onlyText . excludeScript . getBody

countWordsHTML :: Link -> IO (Either ParseError WordFrequency)
countWordsHTML u = do
  c <- loadContent u 
  return $ frequencyIgnoreCase <$> c 

-- tag transformers
getBody :: [Tag String] -> [Tag String]
getBody = takeWhile (~/= ("</body>"::String)) . dropWhile (~/= ("<body>"::String)) 

excludeScript ts = takeWhile (~/= ("<script>"::String)) ts ++ dropWhile (~/= ("</script>"::String)) ts

onlyText = filter isTagText 

findWords :: String -> Either ParseError [String]
findWords = parse words' "test" 

-- parser
word = many1 letter

symbols = many notLetter

notLetter = notp letter

notp p = try (do{w <- try p; unexpected (show w)} <|> anyToken)

word' = symbols *> word <* symbols 

words' = many word'

-- Methods to find href links

findLinks :: [Tag String] -> [Link]
findLinks = fmap getLink . filter herf . findAttributes

findAttributes :: StringLike s => [Tag s] -> [Attribute s]
findAttributes = concatMap f
  where 
    f (TagOpen s as) = as
    f _ = []

herf = (==) "href" . fst

getLink = snd

-- calculation
frequency :: [String] -> WordFrequency
frequency = Map.fromList . map (head &&& length) . group . sort

frequencyIgnoreCase :: [String] -> WordFrequency
frequencyIgnoreCase = frequency . fmap lowerCase

mergeFrequency :: WordFrequency -> WordFrequency -> WordFrequency
mergeFrequency = Map.unionWith (+) 

-- helper methods
lowerCase :: String -> String
lowerCase = fmap toLower
