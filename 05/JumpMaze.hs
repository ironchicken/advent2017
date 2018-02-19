{-# LANGUAGE BangPatterns #-}

module JumpMaze where

import Data.Map (Map)
import qualified Data.Map as M

jump :: Integer -> Integer -> (Integer -> Maybe Integer) -> Map Integer Integer -> Integer
jump counter position mutateOffset instructions =
  doJump $ M.lookup position instructions
  where
    doJump (Just !jmp) = jump nextCounter nextPosition mutateOffset $! M.update mutateOffset position instructions
      where
        !nextCounter = counter + 1
        !nextPosition = position + jmp
    doJump Nothing = counter


main :: IO ()
main = do
  input <- readFile "input"
  let instructions = M.fromList $ zip [0..] (map read $ lines input)
      p1MutateOffset offs = Just $ offs + 1
      answerP1 = jump 0 0 p1MutateOffset instructions

      p2MutateOffset offs
        | offs >= 3 = Just $ offs - 1
        | otherwise = Just $ offs + 1
      answerP2 = jump 0 0 p2MutateOffset instructions

  putStrLn $ show answerP1
  putStrLn $ show answerP2
