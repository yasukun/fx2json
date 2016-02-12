{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Environment

import Fx2json.Util
import Fx2json.CsvParser

main = do
  args <- getArgs
  case args of
    [] -> print "nothing todo"
    _ ->  do let (x:xs) = args
             csv <- readFile x
             convJS csv
