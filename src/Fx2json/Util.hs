{-# LANGUAGE OverloadedStrings #-}
module Fx2json.Util
    ( someFunc
    ) where

import qualified Fx2json.CsvParser as P
import Data.Aeson
import Data.UnixTime
import Data.ByteString.Char8 (pack)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
