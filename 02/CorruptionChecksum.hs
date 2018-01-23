module CorruptionChecksum where

checksumRow :: [Int] -> Int
checksumRow xs = maximum xs - minimum xs

checksum :: [[Int]] -> Int
checksum (row:rows) = checksumRow row + checksum rows
checksum [] = 0

parseRow :: String -> [Int]
parseRow row = map read $ words row

parseTable :: String -> [[Int]]
parseTable table = map parseRow $ lines table

main :: IO ()
main = do
  input <- readFile "test-input"
  let answerP1 = checksum . parseTable $ input
  putStrLn $ show answerP1
