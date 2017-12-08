module Main (main) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Control.Monad.State.Lazy as MS
import qualified Data.List
import qualified Data.String.Utils
import qualified Data.Functor.Identity
import qualified Data.Either

type KeyVal = (String, String)
type IniParser = ParsecT String String ( MS.State [KeyVal] )

printKeysVals :: [KeyVal] -> IO()
printKeysVals = mapM_ ( putStrLn . show )

main = do
    input <- getContents  
    printKeysVals $ MS.execState ( runParserT iniParser "stdin" (filterComments input) ) []

filterComments :: String -> String
filterComments = unlines . filter (not . isComment) . lines

iniParser :: IniParser [KeyVal]
iniParser = do
  sections <- some sectionParser
  return $ concat sections

sectionParser :: IniParser [KeyVal]
sectionParser = do
  section <- sectionHeaderParser
  many ( keyValParser section )

sectionHeaderParser :: IniParser String
sectionHeaderParser = do
  char '['
  section <- some alphaNumChar
  char ']'
  space
  return section

keyParser :: IniParser String
keyParser = some alphaNumChar

varParser :: IniParser String
varParser = do
  char '$'
  char '{'
  var <- valueParser
  char '}'
  return var

valueParser :: IniParser String
valueParser = do
  some alphaNumChar
  space1
  return ""

keyValParser :: String -> IniParser KeyVal
keyValParser section = do
  key <- keyParser
  char '='
  value <- valueParser
  
  let kv = (section ++ '.' : key, value)
  MS.modify (kv:) 
  return kv
  
isComment :: String -> Bool
isComment = Data.List.isPrefixOf ";" . Data.String.Utils.strip
