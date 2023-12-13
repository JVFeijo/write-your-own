import System.Environment
import System.IO
import Data.Char
import qualified Data.ByteString.Lazy as BL
import Data.Set (fromList, toList)
import Control.Monad
import Control.Exception
import Data.List

data ValidOption = C

data InvalidOptionException = IVE String

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates xs = length (nub xs) /= length xs

checkDuplicate :: String -> Either InvalidOptionException String
checkDuplicate str | hasDuplicates str = Left (IVE "Invalid Option provided")
                   | otherwise = Right str

removeHyphen :: String -> Either InvalidOptionException String
removeHyphen ('-':[]) = Left (IVE "Invalid Option provided")
removeHyphen ('-':cs) = Right cs
removeHyphen _ = Left (IVE "Invalid Option provided")

parseOptions :: [String] -> Either InvalidOptionException [ValidOption]
parseOptions strs = (traverse removeHyphen strs) >>= (checkDuplicate . mconcat) >>= (traverse isValidOption)

isValidOption :: Char -> Either InvalidOptionException ValidOption
isValidOption ch | ch == 'c' = Right C
                 | otherwise = Left (IVE "Invalid Option")

numberOfBytes :: BL.ByteString -> Integer
numberOfBytes bs = (fromIntegral . BL.length) bs

wc :: [ValidOption] -> BL.ByteString -> String
wc [] bs = show (numberOfBytes bs)
wc (C : rest) bs = show (numberOfBytes bs)
