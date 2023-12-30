{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import System.IO
import Data.Char
--import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BL
import Data.Set (fromList, toList)
import Control.Monad
import Control.Exception
import Data.List

data ValidOption = C | L | W deriving (Show)

data InvalidOptionException = IVE String deriving (Show)

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates xs = length (nub xs) /= length xs

checkDuplicate :: String -> Either InvalidOptionException String
checkDuplicate str | hasDuplicates str = Left (IVE "Invalid option provided")
                   | otherwise = Right str

removeHyphen :: String -> Either InvalidOptionException String
removeHyphen ('-':[]) = Left (IVE "Invalid option provided")
removeHyphen ('-':cs) = Right cs
removeHyphen _ = Left (IVE "Invalid option provided")

parseOptions :: [String] -> Either InvalidOptionException [ValidOption]
parseOptions strs = (traverse removeHyphen strs) >>= (checkDuplicate . mconcat) >>= (traverse isValidOption)

isValidOption :: Char -> Either InvalidOptionException ValidOption
isValidOption ch | ch == 'c' = Right C
                 | ch == 'l' = Right L
                 | ch == 'w' = Right W
                 | otherwise = Left (IVE "Invalid Option provided")

numberOfBytes :: BL.ByteString -> Integer
numberOfBytes = fromIntegral . BL.length

numberOfLines :: BL.ByteString -> Integer
numberOfLines = fromIntegral . (BL.count '\n')

numberOfWords :: BL.ByteString -> Integer
numberOfWords = fromIntegral . length . BL.words

wc :: ValidOption -> BL.ByteString -> String
wc C bs = show (numberOfBytes bs)
wc L bs = show (numberOfLines bs)
wc W bs = show (numberOfWords bs)

formatWCResult :: [String] -> String -> String
formatWCResult strs fileName = unwords (strs ++ [fileName])

main = do
         args <- getArgs
         let fileName = head args
         let options = parseOptions (tail args)
         fileContent <- BL.readFile fileName
         either (\(IVE err) -> putStrLn err)  putStrLn (((map (\opt -> wc opt fileContent)) <$> options) >>= (\results -> return (formatWCResult results fileName)))
         
