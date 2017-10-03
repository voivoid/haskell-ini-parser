module Main (main) where

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.String as MPS
import System.Environment

type KeyVal = (String, String)

main = do
    input <- getContents
    MP.parseTest iniParser input

iniParser :: MPS.Parser [KeyVal]
iniParser = do
  sections <- MP.some sectionParser
  return $ concat sections

sectionParser :: MPS.Parser [KeyVal]
sectionParser = do
  section <- sectionHeaderParser
  MP.many ( varParser section )

sectionHeaderParser :: MPS.Parser String
sectionHeaderParser = do
  MP.char '['
  section <- MP.some MP.alphaNumChar
  MP.char ']'
  MP.space
  return section

varParser :: String -> MPS.Parser KeyVal
varParser section = do
  key <- MP.some MP.alphaNumChar
  MP.char '='
  value <- MP.some MP.alphaNumChar
  MP.space
  return (section ++ '.' : key, value)


  
