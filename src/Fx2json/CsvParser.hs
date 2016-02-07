{-# LANGUAGE OverloadedStrings #-}
module Fx2json.CsvParser (
      Quote (..)
    , quote
    , csvFile
    , quoteParser
    , quoteFileParser
    , testString
    , testString2
    ) where

import Control.Monad
import Control.Applicative
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Data.List
import Data.Text (Text, unpack, pack)
import qualified Data.Text as T
import Data.Maybe
import Text.Printf
import qualified Data.ByteString.Char8 as B
import Data.UnixTime

data Quote = Quote {
      qTime :: UnixTime,
      qOpen :: Double,
      qHigh :: Double,
      qLow :: Double,
      qClose :: Double,
      qVolume :: Double
    } deriving (Show, Eq)

csvFile :: Parser [Quote]
csvFile = do
    q <- many1 quote
    endOfInput
    return q

qcomma  :: Parser ()
qcomma = char ',' *> pure ()

qtime :: Parser UnixTime
qtime = createTime <$> takeTill (\x -> x == ',')
        where defaultTime = (UnixTime 0 0)
              parseTimeText t = parseUnixTime (B.pack "%Y/%m/%d %H:%M" :: Format) (B.pack $ unpack t)
              createTime x = fromMaybe defaultTime $ Just $ parseTimeText x

quote :: Parser Quote
quote = Quote <$> (qtime <* qcomma)
              <*> (double <* qcomma)
              <*> (double <* qcomma)
              <*> (double <* qcomma)
              <*> (double <* qcomma)
              <*> (double <* endOfLine)

testString :: Text
testString = "2015/12/30 00:01,1.092575,1.0927,1.09255,1.09261,336\n"

testString2 :: Text
testString2="2015/12/30 00:01,1.092575,1.0927,1.09255,1.09261,336\n2015/12/30 00:02,1.09261,1.0928,1.09258,1.092675,423\n"

quoteParser = parseOnly quote

quoteFileParser = parseOnly csvFile
