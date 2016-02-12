{-# LANGUAGE OverloadedStrings #-}
module Fx2json.Util
    ( convJS
    ) where

import Fx2json.CsvParser
import Data.Aeson
import Data.UnixTime
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy (toStrict)
import Control.Monad

currencyDateFormat :: Format
currencyDateFormat = "%Y-%m-%d %H:%M"

convQuote x = do
  let Quote {qTime = ts, qOpen = open, qHigh = high, qLow = low, qClose = close, qVolume = volume} = x
  formatTS <-  B.unpack  <$> formatUnixTime currencyDateFormat ts
  return (formatTS, open, high, low, close, volume)

convJS csv = do
  let (Right result) = quoteFileParser $ T.pack csv
  cvdata <- mapM convQuote result
  putStrLn $ B.unpack $ toStrict $ encode cvdata
