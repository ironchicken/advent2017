module InverseCaptcha where

forwardRepetitions :: (Eq a) => [a] -> [a]
forwardRepetitions xs = fr (head xs) xs
  where
    fr hd (a1:a2:as)
      | a2 == a1 = a2 : fr hd (a2 : as)
      | otherwise = fr hd (a2 : as)
    fr hd [a]
      | a == hd = [a]
      | otherwise = []
    fr _ [] = []

digitsToNumbers :: String -> [Int]
digitsToNumbers = map read . map (\c -> [c])

main :: IO ()
main = do
  input <- readFile "input"
  let answer = sum . forwardRepetitions . digitsToNumbers $ input
  putStrLn $ show answer
