module CorruptionChecksum where

checksumRow :: [Int] -> Int
checksumRow xs = maximum xs - minimum xs

checksum :: [[Int]] -> Int
checksum (row:rows) = checksumRow row + checksum rows
checksum [] = 0

cycles :: [a] -> [[a]]
cycles xs = map (\i -> drop i xs ++ take i xs) $ [0..(length xs) - 1]

pairs :: [a] -> [(a,a)]
pairs xs = concat $ map (\(y:ys) -> [ (y, z) | z <- ys ]) $ cycles xs

divisors :: [Int] -> [(Int, Int)]
divisors xs = filter (\(a,b) -> a `mod` b == 0) $ pairs xs

quotients :: [(Int, Int)] -> [Int]
quotients = map (\(a, b) -> a `div` b)

parseRow :: String -> [Int]
parseRow row = map read $ words row

parseTable :: String -> [[Int]]
parseTable table = map parseRow $ lines table

main :: IO ()
main = do
  input <- readFile "input"
  let answerP1 = checksum . parseTable $ input
      answerP2 = sum $ map (sum . quotients . divisors) $ parseTable $ input
  putStrLn $ show answerP1
  putStrLn $ show answerP2
