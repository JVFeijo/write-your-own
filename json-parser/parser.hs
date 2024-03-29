#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers, directory, trifecta
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
import System.Directory
import qualified Data.Map as M
import Text.Trifecta
import Data.Char
import Control.Monad
import Data.Functor
import Control.Applicative

data JNumber = JDouble Double | JInteger Integer deriving (Show)
data JSON = JBool Bool | JNull | JNum JNumber | JString String | JObject (M.Map String JSON) deriving (Show)

jsonObject :: Parser JSON
jsonObject = (JObject . M.fromList) <$> (char '{' *> ((,) <$> jsonKey <* char ':' <*> jsonValue) `sepBy` char ',' <* char '}')

jsonKey :: Parser String
jsonKey = jsonString `surroundedBy` spaces >>= \(JString s) -> return s

jsonChar :: Parser Char
jsonChar =    string "\\\"" $> '"'
          <|> string "\\\\" $> '\\'
          <|> string "\\/"  $> '/'
          <|> string "\\b"  $> '\b'
          <|> string "\\f"  $> '\f'
          <|> string "\\n"  $> '\n'
          <|> string "\\r"  $> '\r'
          <|> string "\\t"  $> '\t'
          <|> (chr . read) <$> (string "\\u" *> replicateM 4 hexDigit)
          <|> satisfy (\c -> not (c == '\"' || c == '\\' || isControl c))

jsonString :: Parser JSON
jsonString = JString <$> (char '"' *> many jsonChar <* char '"')

jIntegerOrDouble :: Parser JSON
jIntegerOrDouble = integerOrDouble >>= \eitherIntOrDou -> return (either (JNum . JInteger) (JNum . JDouble) eitherIntOrDou)

jsonNumber :: Parser JSON
jsonNumber = jIntegerOrDouble

jsonNull :: Parser JSON
jsonNull = string "null" $> JNull

jsonBool :: Parser JSON
jsonBool =    string "true" $> JBool True
          <|> string "false" $> JBool False

jsonValue :: Parser JSON
jsonValue = (jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonObject) `surroundedBy` spaces

parseJSON :: Parser JSON
parseJSON = jsonValue

baseDirsPath = "input-tests/"
testDirs = ["step1/", "step2/", "step3/"]
testDirsPath = map ((++) baseDirsPath) testDirs

main =       
       traverse listDirectory testDirsPath >>=
       \filesByDir -> return (mconcat (zipWith (fmap . (++)) testDirsPath filesByDir)) >>=
       \filesPaths -> traverse readFile filesPaths >>=
       \filesContents -> return ((parseString parseJSON mempty) <$> filesContents) >>=
       \results -> traverse print results
