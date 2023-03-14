{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE TupleSections #-}

module CSV where

import Data.ByteString.Lazy (ByteString)
import Data.Csv
import Data.Function
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import GHC.Exts
import Parser

ls :: [Clipping] -> [(Text, Text)]
ls xs =
  [ (the title, the author)
    | Clipping {title, author} <- clean xs,
      then group by
        (title, author)
      using
        groupWith
  ]

clean :: [Clipping] -> [Clipping]
clean =
  filter $ \case
    Clipping {type_ = Bookmark} -> False
    Clipping {type_ = Highlight, content = " <您已达到本内容的剪贴上限>"} -> False
    _ -> True

utcToText :: UTCTime -> Text
utcToText = T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

type ReadwiseRow = (Text, Text, Text, Maybe Text, Int, Text)

instance ToNamedRecord ReadwiseRow where
  toNamedRecord (title, author, highlight, note, loc, date) =
    namedRecord ["Title" .= title, "Author" .= author, "Highlight" .= highlight, "Note" .= note, "Location" .= loc, "Date" .= date]

toRows :: [Clipping] -> [ReadwiseRow]
toRows xs = go sorted
  where
    sorted = sortOn (\case Clipping {loc, type_, content} -> (loc, type_, T.length content)) xs
    go [] = []
    go [x] = [to x Nothing] -- Branch for only one Highlight/Cut, since Note always comes with a Highlight
    go (x : xs@(y : ys))
      | eqOn loc x y && eqOn type_ x y = go xs -- Pick the longer Highlight/Note (drop x)
      | eqOn loc x y && type_ y == Note = to x (Just $ content y) : go ys
      | otherwise = to x Nothing : go xs
    to x = (title x,author x,content x,,loc x,utcToText $ date x)
    eqOn f = (==) `on` f

toCSVs :: [Clipping] -> [(String, ByteString)]
toCSVs xs = titles `zip` map toCSV grouped
  where
    grouped = groupWith title $ clean xs
    titles = map (T.unpack . title . head) grouped -- FilePath is String
    toCSV = encodeByName (header ["Title", "Author", "Highlight", "Note", "Location", "Date"]) . toRows