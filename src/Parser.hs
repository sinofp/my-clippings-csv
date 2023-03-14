{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative hiding (many, some)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug

type Parser = Parsec Void Text

sameLineNot :: Char -> Char -> Bool
sameLineNot = liftA2 (&&) (/= '\n') . (/=)

pLocation :: Parser Int
pLocation = (try range <|> L.decimal) <* takeWhileP (Just "）的/ 的/的") (sameLineNot '的') <* char '的'
  where
    range = L.decimal *> char '-' *> L.decimal

data ClippingType = Highlight | Bookmark | Cut | Note deriving (Show, Eq, Ord) -- Ensure Note is the greatest

pType :: Parser ClippingType
pType =
  choice
    [ Highlight <$ string "标注",
      Note <$ string "笔记",
      Bookmark <$ string "书签",
      Cut <$ string "文章剪切"
    ]
    <* string " | 添加于 "

pDay :: Parser Day
pDay = fromGregorian <$> L.decimal <* char '年' <*> L.decimal <* char '月' <*> L.decimal <* takeP (Just "忽略“日星期几 ”") 5

pTime :: Parser TimeOfDay
pTime = TimeOfDay <$> pHour <*> pMinute <*> pSecond
  where
    pHour = (+) <$> (0 <$ string "上午" <|> 12 <$ string "下午") <*> L.decimal <* char ':'
    pMinute = L.decimal <* char ':'
    pSecond = L.decimal <* eol <* eol

pUTCTime :: Parser UTCTime
pUTCTime = UTCTime <$> pDay <*> (timeOfDayToTime <$> pTime)

pLine :: Parser Text
pLine = takeWhileP (Just "a line") (sameLineNot '\r') <* eol -- takeWhileP doesn't consume predicate

pContent :: Parser Text -- Keep \r\n?
pContent = T.intercalate "\n" <$> someTill pLine (string "==========" <* eol)

data Clipping = Clipping
  { title :: Text,
    author :: Text,
    loc :: Int,
    type_ :: ClippingType,
    date :: UTCTime,
    content :: Text
  }
  deriving (Show, Eq)

pTitleAuthor :: Parser Text
pTitleAuthor = pLine <* takeWhile1P (Just "- 您在位置 #") (sameLineNot '#') <* char '#'

pClipping :: Parser Clipping
pClipping = Clipping <$> pTitle <*> pAuthor <*> pLocation <*> pType <*> pUTCTime <*> pContent

pFile :: Parser [Clipping]
pFile = some pClipping

pTitle :: Parser Text -- I can't stop at whitespace, so I must strip it.
pTitle = T.strip <$> takeWhile1P (Just "标题") (not . isTitleEndMark) <* junks
  where
    isTitleEndMark c = c == '(' || c == '（' || c == '【' || c == '：'
    junks = takeWhileP (Just "作者前的垃圾话") (/= '(') <* manyTill anySingle (try $ lookAhead $ pAuthor' <* eol)

pAuthor' :: Parser Text -- I can't stop at hspace, but there can be a hspace between name and origName
pAuthor' = T.strip <$> (char '(' *> optional nation *> name <* optional origName <* char ')' <* hspace)
  where
    name = takeWhileP (Just "作者名") (\c -> c /= '(' && c /= ')' && c /= '（' && c /= '）')
    nation =
      choice
        [ between' '(' ')',
          between' '（' '）', -- <* notFollowedBy (char ')'), -- (（安·兰德）)
          between' '[' ']',
          between' '［' '］',
          between' '【' '】'
        ]
        <* hspace
    between' open close = (char open *> takeWhileP (Just "国籍") (/= close) <* char close) :: Parser Text
    origName = (char '(' *> name <* char ')') <|> char '（' *> name <* char '）'

pAuthor :: Parser Text
pAuthor = last <$> some pAuthor' <* eol <* takeWhile1P (Just "- 您在位置 #") (/= '#') <* char '#'
