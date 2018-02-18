module JumpMaze where

import Data.Map (Map)
import qualified Data.Map as M

jump :: Int -> Int -> Map Int Int -> Int
jump counter position instructions =
  doJump $ M.lookup position instructions
  where
    doJump (Just jmp) = jump (counter + 1) (position + jmp) $ M.update (\j -> Just $ j + 1) position instructions
    doJump Nothing = counter

main :: IO ()
main = do
  input <- readFile "input"
  let instructions = M.fromList $ zip [0..] (map read $ lines input)
      answerP1 = jump 0 0 instructions
  putStrLn $ show answerP1
