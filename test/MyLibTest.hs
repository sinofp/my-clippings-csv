{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Time
import Parser
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char

main :: IO ()
main = hspec $
  describe "Parser" $ do
    describe "Location Parser" $ do
      it "returns the last number of range" $
        parse pLocation "" "2599-2600的" `shouldParse` 2600
      it "returns the only number of one number" $
        parse pLocation "" "15231 的" `shouldParse` 15231
      it "consumes trailing junks 1" $
        pLocation `shouldConsumeAll` "15231 的"
      it "consumes trailing junks 2" $
        pLocation `shouldConsumeAll` "5424-5425）的"

    describe "Type Parser" $ do
      it "returns Highlight" $
        parse pType "" "标注 | 添加于 " `shouldParse` Highlight
      it "returns Clipping" $
        parse pType "" "文章剪切 | 添加于 " `shouldParse` Cut
      it "returns Highlight" $
        parse pType "" "书签 | 添加于  " `shouldParse` Bookmark
      it "returns Highlight" $
        parse pType "" "笔记 | 添加于 " `shouldParse` Note
      it "consumes trailing junks" $
        pType `shouldConsumeAll` "笔记 | 添加于 "

    describe "Date & Time Parser" $ do
      it "returns Gregorian day" $
        parse pDay "" "2018年11月19日星期一 " `shouldParse` fromGregorian 2018 11 19
      it "consumes trailing junks after date" $
        pDay `shouldConsumeAll` "2018年11月19日星期一 "
      it "returns afternoon time of day" $
        parse pTime "" "下午2:23:42\n\n" `shouldParse` TimeOfDay 14 23 42
      it "returns morning time of day" $
        parse pTime "" "上午11:09:07\r\n\r\n" `shouldParse` TimeOfDay 11 09 07
      it "returns 12 AM correctly" $
        parse pTime "" "上午12:39:58\r\n\r\n" `shouldParse` TimeOfDay 0 39 58
      it "returns 12 PM correctly" $
        parse pTime "" "下午12:15:12\r\n\r\n" `shouldParse` TimeOfDay 12 15 12
      it "consumes trailing junks after time" $
        pTime `shouldConsumeAll` "下午4:13:41\r\n\r\n"
      it "returns UTCTime" $ -- I don't convert TimeZone
        parse pUTCTime "" "2018年11月19日星期一 下午2:23:42\n\n" `shouldParse` parseTimeOrError True defaultTimeLocale "%d %b %Y %l:%M:%S %p" "19 Nov 2018 2:23:42 PM"

    describe "Line Parser" $ do
      it "returns only one line" $
        parse pLine "" "abc\r\n123\r\n" `shouldParse` "abc"

    describe "Title & Author Parser" $ do
      it "consumes trailing junks 1" $
        pTitleAuthor `shouldConsumeAll` "The Economist (calibre)\r\n- 在位置 #"
      it "consumes trailing junks 2" $
        pTitleAuthor `shouldConsumeAll` "阿特拉斯耸耸肩 (重现经典) (（安·兰德）)\r\n- 您在位置 #"
      it "consumes trailing junks 3" $
        pTitleAuthor `shouldConsumeAll` "铁道之旅：19世纪空间与时间的工业化（以城市规划、心理学、建筑学、经济学等诸多视角，探索人类工业意识之起源） ((德)沃尔夫冈·希弗尔布施)\r\n- 您在第 128 页（位置 #"
      it "ignores some style" $ -- 只出现过一次，而且和其他的大不相同。所以我刻意忽略它
        parse pTitleAuthor "" `shouldFailOn` "Augustine  \r\n- 您在第 3-3 页的标注 | 添加于 2018年11月19日星期一 下午2:23:42\r\n"

    describe "Title Parser" $ do
      it "returns correct title 1" $
        parse pTitle "" "阿特拉斯耸耸肩 (重现经典) (（安·兰德）)\r\n" `shouldParse` "阿特拉斯耸耸肩"
      it "returns correct title 2" $
        parse pTitle "" "狼与香辛料 (【日】支仓冻砂)\r\n" `shouldParse` "狼与香辛料"
      it "returns correct title 3" $
        parse pTitle "" "Docker实践（异步图书） (伊恩·米尔(Ian Miell))\r\n" `shouldParse` "Docker实践"
      it "returns correct title 4" $
        parse pTitle "" "兒玉瑪利亞文學匯編 - 第01卷 (三島芳治)\r\n" `shouldParse` "兒玉瑪利亞文學匯編 - 第01卷"
      it "returns correct title 5" $
        parse pTitle "" "高效休息法　世界精英这样放松大脑 (久贺谷亮;ePUBw.COM)\r\n" `shouldParse` "高效休息法　世界精英这样放松大脑"
      it "returns correct title 6" $
        parse pTitle "" "从一到无穷大【爱因斯坦亲写推荐语、20世纪的科普经典！《浪潮之巅》、《文明之光》作者吴军博士力荐！】 (乔治·伽莫夫)\r\n" `shouldParse` "从一到无穷大"
      it "returns correct title 7" $
        parse pTitle "" "世界美术名作二十讲：有人通过此书欣赏世界，有人通过此书寻找本心。傅雷经传世之作，打开艺术鉴赏之门。全新修订，彩色典藏版。 (傅雷)\r\n" `shouldParse` "世界美术名作二十讲"
      it "consumes trailing junks 1" $
        runParser' pTitle (initialState "Docker实践（异步图书） (伊恩·米尔(Ian Miell))\r\n") `succeedsLeaving` "(伊恩·米尔(Ian Miell))\r\n"
      it "consumes trailing junks 2" $
        runParser' pTitle (initialState "从一到无穷大【爱因斯坦亲写推荐语、20世纪的科普经典！《浪潮之巅》、《文明之光》作者吴军博士力荐！】 (乔治·伽莫夫)\r\n") `succeedsLeaving` "(乔治·伽莫夫)\r\n"
      it "consumes trailing junks 3" $
        runParser' pTitle (initialState "希腊罗马神话(古典学学者写给大众的神话小书，追溯古希腊罗马神话的前世今生。） (菲利普·马蒂塞克)\r\n") `succeedsLeaving` "(菲利普·马蒂塞克)\r\n"

    describe "Author Parser" $ do
      it "returns correct author 1" $
        parse pAuthor' "" "(［苏］帕斯捷尔纳克)" `shouldParse` "帕斯捷尔纳克"
      it "returns correct author 2" $
        parse pAuthor' "" "([巴西] Luciano Ramalho)" `shouldParse` "Luciano Ramalho"
      it "returns correct author 3" $
        parse pAuthor' "" "(诺姆·乔姆斯基(Noam Chomsky))" `shouldParse` "诺姆·乔姆斯基"
      it "returns correct author 4" $ -- hspace
        parse pAuthor' "" "(米·布尔加科夫 (M.Bulgakov))" `shouldParse` "米·布尔加科夫"
      it "returns correct author 5" $ -- （origName）
        parse pAuthor' "" "(安迪·普迪科姆（Andy Puddicombe）)" `shouldParse` "安迪·普迪科姆"
      it "ignores some style 1" $
        parse pAuthor' "" "(（安·兰德）)" `shouldParse` ""
      it "ignores some style 2" $
        parse pAuthor' "" "(〔英〕罗伯特·路易斯·史蒂文森 [〔英〕罗伯特·路易斯·史蒂文森])" `shouldParse` "〔英〕罗伯特·路易斯·史蒂文森 [〔英〕罗伯特·路易斯·史蒂文森]"

    describe "Content Parser" $ do
      it "returns one line content" $
        parse pContent "" "“伙计，这是玄学。医生不叫我谈这一套，因为我的胃吃不消。”\r\n==========\r\n" `shouldParse` "“伙计，这是玄学。医生不叫我谈这一套，因为我的胃吃不消。”"
      it "returns multi-line content" $
        parse pContent "" "abc\r\n123\r\n==========\r\n" `shouldParse` "abc\n123"
      it "consumes trailing junks" $
        pContent `shouldConsumeAll` "“伙计，这是玄学。医生不叫我谈这一套，因为我的胃吃不消。”\r\n==========\r\n"

    describe "Clipping Parser" $ do
      it "returns correct Clipping 1" $
        let text =
              "大师和玛格丽特(译文名著精选) (米·布尔加科夫 (M.Bulgakov))\r\n\
              \- 您在第 381 页（位置 #5424-5425）的标注 | 添加于 2018年12月28日星期五 下午4:14:06\r\n\
              \\r\n\
              \“瞧，你们的结局多么美满。我就不是这样的。”他又若有所思地说：“不过，或许也会是这样的……”\r\n\
              \==========\r\n"
            date = parseTimeOrError True defaultTimeLocale "%d %b %Y %l:%M:%S %p" "28 Dec 2018 4:14:06 PM"
         in parse pClipping "" text `shouldParse` Clipping "大师和玛格丽特" "米·布尔加科夫" 5425 Highlight date "“瞧，你们的结局多么美满。我就不是这样的。”他又若有所思地说：“不过，或许也会是这样的……”"
      it "returns correct Clipping 2" $
        let text =
              "日瓦戈医生 (［苏］帕斯捷尔纳克)\r\n\
              \- 您在位置 #4422-4423的标注 | 添加于 2019年1月20日星期日 上午10:30:42\r\n\
              \\r\n\
              \这么多没完没了的准备工作是因为什么？是因为平庸无能。人生下来是要生活的，不是为准备生活而生。生活本身，生活的好坏，生活的本领，才是要紧不过的事！\r\n\
              \==========\r\n"
            date = parseTimeOrError True defaultTimeLocale "%d %b %Y %l:%M:%S %p" "20 Jan 2019 10:30:42 AM"
         in parse pClipping "" text `shouldParse` Clipping "日瓦戈医生" "帕斯捷尔纳克" 4423 Highlight date "这么多没完没了的准备工作是因为什么？是因为平庸无能。人生下来是要生活的，不是为准备生活而生。生活本身，生活的好坏，生活的本领，才是要紧不过的事！"
      it "returns correct Clipping 3" $
        let text =
              "世界上最简单的会计书 (会计极速入职晋级) (达雷尔·穆利斯)\r\n\
              \- 您在第 20 页（位置 #152）的笔记 | 添加于 2022年5月19日星期四 下午8:36:00\r\n\
              \\r\n\
              \Equity\r\n\
              \初始投资\r\n\
              \净值\r\n\
              \是一回事啊\r\n\
              \==========\r\n"
            date = parseTimeOrError True defaultTimeLocale "%d %b %Y %l:%M:%S %p" "19 May 2022 8:36:00 PM"
         in parse pClipping "" text `shouldParse` Clipping "世界上最简单的会计书" "达雷尔·穆利斯" 152 Note date "Equity\n初始投资\n净值\n是一回事啊"
      it "returns correct Clipping 4" $
        let text =
              "秦淮之夜（日本唯美派文学代表人物、备受周作人推崇的作家谷崎润一郎，首次被翻译引进的随笔集） (东瀛文人·印象中国) ([日]谷崎润一郎)\r\n\
              \- 您在第 117 页（位置 #1805-1805）的标注 | 添加于 2022年11月24日星期四 上午11:49:14\r\n\
              \\r\n\
              \ <您已达到本内容的剪贴上限>\r\n\
              \==========\r\n"
            date = parseTimeOrError True defaultTimeLocale "%d %b %Y %l:%M:%S %p" "24 Nov 2022 11:49:14 AM"
         in parse pClipping "" text `shouldParse` Clipping "秦淮之夜" "谷崎润一郎" 1805 Highlight date " <您已达到本内容的剪贴上限>"
  where
    shouldConsumeAll parser text = runParser' parser (initialState text) `succeedsLeaving` ""
    shouldParseAs parser text res = parse parser "" text `shouldParse` res
