module Main (main) where

import Text.Megaparsec
import Text.Megaparsec.String
import System.Environment

type KeyVal = (String, String)

main = do
    input <- getContents
    parseTest iniParser input

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


  
