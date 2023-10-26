{-# LANGUAGE OverloadedStrings #-}

module Main where

import CSV
import Control.Monad.Reader
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.IO as T
import Options.Applicative
import Parser
import System.Directory
import System.Environment
import Text.Megaparsec

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  args <- execParser $ pOptions cwd

  let fileName = inputFile args
  text <- decodeUtf8 . stripUtf8Bom <$> BS.readFile fileName
  case parse pFile fileName text of
    Left e -> putStrLn $ errorBundlePretty e
    Right xs -> do
      mapM_ printInfo $ ls xs
      mapM_ (writeCSV $ bom args) $ runReader (toCSVs xs) args
  where
    stripUtf8Bom bs = fromMaybe bs $ BS.stripPrefix "\239\187\191" bs
    writeCSV bom (fileName, bs) = BSL.writeFile fileName (if bom then "\239\187\191" else "" <> bs) -- for Excel
    printInfo (title, author) = T.putStrLn $ T.concat ["《", title, "》，", author]

pOptions cwd = info (cmdOpt <**> helper) (fullDesc <> progDesc "把中文My Clippings.txt转成CSV " <> header "Header")
  where
    cmdOpt =
      CmdOpt
        <$> strOption (long "input" <> short 'i' <> metavar "TXT" <> help "从Kindle导出的My Clippings.txt" <> showDefault <> value "My Clippings.txt")
        <*> strOption (long "output-dir" <> short 'd' <> metavar "DIR" <> help "把生成的CSV放到哪里" <> showDefault <> value cwd)
        <*> switch (long "rm-junk" <> help "去掉<您已达到本内容的剪贴上限>")
        <*> switch (long "bom" <> help "添加UTF-8 BOM，否则使用Excel打开会出现乱码")
