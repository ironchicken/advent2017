module MemoryReallocation where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isJust, fromJust)

lookupAndUpdate :: (Ord k, Show k) => Map k a -> (a -> Maybe a) -> k -> Map k a
lookupAndUpdate m update key =
  doUpdate $ M.lookup key m
  where
    doUpdate (Just value) = M.update update key m
    doUpdate Nothing = error $ "No key: " ++ (show key)

redistribute :: Int -> Int -> Int -> Map Int Int -> Map Int Int
redistribute _ _ 0 memory = memory
redistribute memorySize pos memVal memory =
  redist $ M.lookup pos memory
  where
    nextPos = ((pos + 1) `mod` memorySize)
    updatedMem = M.update (\currentVal -> Just $ currentVal + 1) pos memory
    redist (Just _) = redistribute memorySize nextPos (memVal - 1) updatedMem
    redist Nothing = error $ "Memory has no block at " ++ show pos

maxValue :: (Ord v, Ord k) => (k, v) -> Map k v -> (k, v)
maxValue init = M.foldlWithKey (\(cK, cV) k v -> if v > cV || (v == cV && k <= cK) then (k, v) else (cK, cV)) init

nextAllocation :: Int -> Map Int Int -> Map Int Int
nextAllocation memorySize memory =
  redistribute memorySize pos v initialMem
  where
    pos = (k + 1) `mod` memorySize
    (k, v) = maxValue (0,0) memory
    initialMem = lookupAndUpdate memory (\_ -> Just 0) k

reallocate :: Int -> [Map Int Int] -> Map Int Int -> [Map Int Int]
reallocate memorySize memory nextBlock
  | nextBlock `elem` memory = memory
  | otherwise = reallocate memorySize (nextBlock : memory) $ nextAllocation memorySize nextBlock

main :: IO ()
main = do
  input <- readFile "input"
  let memory = M.fromList . zip [0..] . map read . words $ input
      memorySize = length memory
      answerP1 = length $ reallocate memorySize [] memory
  putStrLn $ show answerP1
