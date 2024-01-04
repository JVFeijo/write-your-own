#!/usr/bin/env cabal
{- cabal:
build-depends: base, bytestring, containers, utf8-string, directory
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
import System.Environment
import System.IO
import System.Directory
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.UTF8 as BLUTF8
import Data.Set (fromList, toList)
import Control.Monad
import Control.Exception
import Data.List

data ValidOption = C | L | W | M | Default deriving (Show)

data InvalidOptionException = IVE String deriving (Show)

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates xs = length (nub xs) /= length xs

checkDuplicate :: [String] -> Either InvalidOptionException [String]
checkDuplicate str | hasDuplicates str = Left (IVE "Invalid option provided")
                   | otherwise = Right str

removeHyphen :: String -> Either InvalidOptionException String
removeHyphen ('-':[]) = Left (IVE "Invalid option provided")
removeHyphen ('-':cs) = Right cs
removeHyphen _ = Left (IVE "Invalid option provided")

parseOptions :: [String] -> Either InvalidOptionException [ValidOption]
parseOptions [] = Right [Default]
parseOptions strs = (traverse removeHyphen strs) >>= checkDuplicate >>= (traverse isValidOption)

isValidOption :: String -> Either InvalidOptionException ValidOption
isValidOption str | str == "c" = Right C
                  | str == "l" = Right L
                  | str == "w" = Right W
                  | str == "m" = Right M
                  | otherwise = Left (IVE "Invalid Option provided")

numberOfBytes :: BL.ByteString -> Integer
numberOfBytes = fromIntegral . BL.length

numberOfLines :: BL.ByteString -> Integer
numberOfLines = fromIntegral . (BL.count '\n')

numberOfWords :: BL.ByteString -> Integer
numberOfWords = fromIntegral . length . BL.words

numberOfCharacters :: BL.ByteString -> Integer
numberOfCharacters = fromIntegral . BLUTF8.length

wc :: ValidOption -> BL.ByteString -> String
wc C bs = show (numberOfBytes bs)
wc L bs = show (numberOfLines bs)
wc W bs = show (numberOfWords bs)
wc M bs = show (numberOfCharacters bs)
wc Default bs = unwords (map ((flip wc) bs) [C, L, W])

formatWCResult :: [String] -> String -> String
formatWCResult strs fileName = unwords (strs ++ [fileName])

getStdinOrFileHandle :: String -> IO Handle
getStdinOrFileHandle filePath = do
                                   fileExists <- doesFileExist filePath
                                   if fileExists
                                      then openFile filePath ReadMode
                                      else return stdin

parseWCArgs :: [String] -> IO (Either InvalidOptionException (Handle, FilePath, [ValidOption]))
parseWCArgs args = do
                     let (initArgs, lastArg) = if null args then ([], "") else (init args, last args)
                     fileExists <- doesFileExist lastArg
                     if fileExists
                        then (openFile lastArg ReadMode) >>= (\handle -> return (((handle, lastArg,) <$> parseOptions initArgs)))
                        else return ((stdin, "",) <$> parseOptions (initArgs ++ [lastArg]))

main = do
         args <- getArgs
         wcArgs <- parseWCArgs args
         case wcArgs of
             Left (IVE errMessage) -> putStrLn errMessage
             Right (handle, filePath, options) -> (BL.hGetContents handle >>= (\content -> return (map (\opt -> wc opt content) options)) >>= (\results -> putStrLn (formatWCResult results filePath)))
--       either (\(IVE err) -> putStrLn err) putStrLn (((map (\opt -> wc opt fileContent)) <$> options) >>= (\results -> return (formatWCResult results fileName)))
         
