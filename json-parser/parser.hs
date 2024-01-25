#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers, directory, trifecta
-}
import System.Directory
import qualified Data.Map as M
import Text.Trifecta

data JSON = JObject (M.Map String JSON) deriving (Show)

parseEmptyObject :: Parser JSON
parseEmptyObject = between (symbol "{") (symbol "}") (string "") >> return (JObject M.empty)

parseJSON :: Parser JSON
parseJSON = parseEmptyObject <* eof

baseDirsPath = "input-tests/"
testDirs = ["step1/"]
testDirsPath = map ((++) baseDirsPath) testDirs

main =       
       traverse listDirectory testDirsPath >>=
       \filesByDir -> return (mconcat (zipWith (fmap . (++)) testDirsPath filesByDir)) >>=
       \filesPaths -> traverse readFile filesPaths >>=
       \filesContents -> return ((parseString parseJSON mempty) <$> filesContents) >>=
       \results -> traverse print results
