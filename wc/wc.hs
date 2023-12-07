import System.Environment
import System.IO
import Data.Char
import qualified Data.ByteString.Lazy as BL
import Data.Set (fromList, toList)
import Control.Monad
import Control.Exception

numberOfBytes :: BL.ByteString -> Integer
numberOfBytes bs = (fromIntegral . BL.length) bs

isValidOptionAux :: String -> Bool
isValidOptionAux (c:cs) | c == 'c' = True
                        | otherwise = False

isValidOption :: String -> Bool
isValidOption ('-':cs) = isValidOptionAux cs
isValidOption _ = False

removeHyphen :: String -> String
removeHyphen str | isValidOption str == True = tail str
                 | otherwise = str

wc :: String -> BL.ByteString -> String
wc [] bs = show (numberOfBytes bs)
wc (c:cs) bs | c == 'c' = show (numberOfBytes bs)


main = do
         args <- getArgs
         let fileName = head args
         let options = (toList . fromList . mconcat . (map removeHyphen)) (tail args)
         handle (\(e :: IOException) -> print e >> return ()) (join $ putStrLn <$> (++ " " ++ fileName) <$> (wc options) <$> (BL.readFile fileName))
