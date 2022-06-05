module Main where

import System.Environment (getArgs)

import Lib

main :: IO ()
main = do
  args <- getArgs
  let inRowPath = args !! 0
  let inPhoneticRadicalPath = args !! 1
  let outPath = args !! 2
  print [inRowPath, inPhoneticRadicalPath, outPath]
  convertCsvToTex inRowPath inPhoneticRadicalPath outPath
