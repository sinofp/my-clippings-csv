{-# LANGUAGE OverloadedStrings #-}

module Main where

import CSV
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.IO as T
import Parser
import System.Environment
import Text.Megaparsec

main :: IO ()
main = do
  fileName <- head <$> getArgs
  text <- decodeUtf8 <$> BS.readFile fileName
  case parse pFile fileName text of
    Left e -> putStrLn $ errorBundlePretty e
    Right xs -> do
      mapM_ printInfo $ ls xs
      mapM_ writeCSV $ toCSVs xs
  where
    writeCSV (title, bs) = BSL.writeFile (title <> ".csv") bs
    printInfo (title, author) = T.putStrLn $ T.concat ["《", title, "》，", author]