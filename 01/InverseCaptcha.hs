module InverseCaptcha where

circulate :: Int -> [a] -> [a]
circulate i xs =
  let i' = i `mod` length xs
  in drop i' xs ++ take i' xs

digitsToNumbers :: String -> [Int]
digitsToNumbers = map read . map (\c -> [c])

main :: IO ()
main = do
  input <- readFile "input"
  let inputDigits = digitsToNumbers input
      answerP1 = sum $ map fst $ filter (\(a, b) -> a == b) $ zip inputDigits $ circulate 1 inputDigits
      answerP2 = sum $ map fst $ filter (\(a, b) -> a == b) $ zip inputDigits $ circulate (length inputDigits `div` 2) inputDigits
  putStrLn $ show answerP1
  putStrLn $ show answerP2
