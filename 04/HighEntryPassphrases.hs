module HighEntryPassphrases where

import Data.List (nub)

main :: IO ()
main = do
  passphraseFile <- readFile "input"
  let phrases = lines passphraseFile
      noDuplicates str = (length $ words str) == (length $ nub $ words str)
      answerP1 = length $ filter noDuplicates phrases
  putStrLn $ show answerP1
