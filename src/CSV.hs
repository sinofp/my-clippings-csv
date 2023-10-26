{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE TupleSections #-}

module CSV where

import Control.Monad.Reader
import Data.ByteString.Lazy (ByteString)
import Data.Csv
import Data.Function
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import GHC.Exts
import Parser
import System.FilePath

data CmdOpt = CmdOpt
  { inputFile :: FilePath,
    outputDir :: FilePath,
    removeJunk :: Bool
  }

type OptReader = Reader CmdOpt

ls :: [Clipping] -> [(Text, Text)]
ls xs =
  [ (the title, the author)
    | Clipping {title, author} <- xs,
      then group by
        (title, author)
      using
        groupWith
  ]

clean :: [Clipping] -> OptReader [Clipping]
clean xs = do
  keepJunk <- asks $ not . removeJunk
  let p = \case
        Clipping {type_ = Bookmark} -> False
        Clipping {type_ = Highlight, content = " <您已达到本内容的剪贴上限>"} -> keepJunk
        _ -> True
  pure $ filter p xs

utcToText :: UTCTime -> Text
utcToText = T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

type ReadwiseRow = (Text, Text, Text, Maybe Text, Int, Text)

instance ToNamedRecord ReadwiseRow where
  toNamedRecord (title, author, highlight, note, loc, date) =
    namedRecord ["Title" .= title, "Author" .= author, "Highlight" .= highlight, "Note" .= note, "Location" .= loc, "Date" .= date]

toRows :: [Clipping] -> [ReadwiseRow]
toRows = go . sort
  where
    sort = sortOn (\case Clipping {loc, type_, content} -> (loc, type_, T.length content))
    go [] = []
    go [x] = [to x Nothing] -- Branch for only one Highlight/Cut, since Note always comes with a Highlight
    go (x : xs@(y : ys))
      | eqOn loc x y && eqOn type_ x y = go xs -- Pick the longer Highlight/Note (drop x)
      | eqOn loc x y && type_ y == Note = to x (Just $ content y) : go ys
      | otherwise = to x Nothing : go xs
    to x = (title x,author x,content x,,loc x,utcToText $ date x)
    eqOn f = (==) `on` f

toCSVs :: [Clipping] -> OptReader [(FilePath, ByteString)]
toCSVs xs = do
  grouped <- groupWith title <$> clean xs
  let titles = map (T.unpack . title . head) grouped -- FilePath is String
  outputDir <- asks outputDir
  let fileNames = map (\x -> outputDir </> x <.> "csv") titles
  pure $ fileNames `zip` map toCSV grouped
  where
    toCSV = encodeByName (header ["Title", "Author", "Highlight", "Note", "Location", "Date"]) . toRows
