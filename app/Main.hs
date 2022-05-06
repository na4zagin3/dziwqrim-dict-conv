module Main where

import System.Environment (getArgs)

import Lib

main :: IO ()
main = do
  args <- getArgs
  let inPath = args !! 0
  let outPath = args !! 1
  print [inPath, outPath]
  convertCsvToTex inPath outPath
