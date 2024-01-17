{- cabal:
build-depends: base, containers
-}
import qualified Data.Map as M
import Text.Trifecta

data JSON = JObject (M.Map String JSON) deriving (Show)

parseEmptyObject :: Parser JSON
parseEmptyObject = between (symbol "{") (symbol "}") (string "") >> return (JObject M.empty)

parseJSON :: Parser JSON
parseJSON = parseEmptyObject

