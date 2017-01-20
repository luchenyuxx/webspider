{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Network.Wreq
import Control.Lens
import Text.ParserCombinators.Parsec

testHTML :: IO (Either ParseError [[String]])
testHTML = do
  r <- get "http://www.naliu.co.uk"
  return $ parse htmlToWords "html" (r ^. responseBody)

word = many1 letter

words' = many (manyTill anyChar word)

htmlToWords = endBy line (many anyChar >> eof)

line = many $ try (manyTill anyChar word)
