module HighEntryPassphrases where

import Data.List (nub, sort)

main :: IO ()
main = do
  passphraseFile <- readFile "input"
  let phrases = lines passphraseFile
      noDuplicates str = (length $ words str) == (length $ nub $ words str)
      noAnagrams str = (length $ words str) == (length $ nub $ map sort $ words str)
      answerP1 = length $ filter noDuplicates phrases
      answerP2 = length $ filter noAnagrams phrases
  putStrLn $ show answerP1
  putStrLn $ show answerP2
