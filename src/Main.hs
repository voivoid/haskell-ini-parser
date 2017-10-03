module Main (main) where

import Text.Megaparsec
import Text.Megaparsec.String
import qualified Data.List
import qualified Data.String.Utils

type KeyVal = (String, String)

main = do
    input <- getContents
    parseTest iniParser (filterComments input)
    where filterComments = unlines . (filter $ not . isComment) . lines

iniParser :: Parser [KeyVal]
iniParser = do
  sections <- some sectionParser
  return $ concat sections

sectionParser :: Parser [KeyVal]
sectionParser = do
  section <- sectionHeaderParser
  many ( varParser section )

sectionHeaderParser :: Parser String
sectionHeaderParser = do
  char '['
  section <- some alphaNumChar
  char ']'
  space
  return section

varParser :: String -> Parser KeyVal
varParser section = do
  key <- some alphaNumChar
  char '='
  value <- some alphaNumChar
  space
  return (section ++ '.' : key, value)
  
isComment :: String -> Bool
isComment = (Data.List.isPrefixOf ";") . Data.String.Utils.strip
