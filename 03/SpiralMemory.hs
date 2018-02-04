module SpiralMemory where

data Walker = Walker
  { x :: Int
  , y :: Int
  , dX :: Int
  , dY :: Int
  , xB :: Int
  , yB :: Int
  } deriving (Eq, Show)

move :: Walker -> Walker
move w
  | headingLeft w && not (atLeftEdge w) = moveLeft w
  | atLeftEdge w = moveDown . headDown $ w
  | headingDown w && not (atBottomEdge w) = moveDown w
  | atBottomEdge w = moveRight . headRight $ w
  | headingRight w && not (atRightEdge w) = moveRight w
  | atRightEdge w = moveUp . headUp $ w
  | headingUp w && not (atTopEdge w) = moveUp w
  | atTopEdge w = moveLeft . headLeft $ w

headingLeft :: Walker -> Bool
headingLeft w = (dX w) < 0

headingRight :: Walker -> Bool
headingRight w = (dX w) > 0

headingUp :: Walker -> Bool
headingUp w = (dY w) > 0

headingDown :: Walker -> Bool
headingDown w = (dY w) < 0

atLeftEdge :: Walker -> Bool
atLeftEdge w = (x w) == (xB w) && headingLeft w

atRightEdge :: Walker -> Bool
atRightEdge w = (x w) == (xB w) && headingRight w

atTopEdge :: Walker -> Bool
atTopEdge w = (y w) == (yB w) && headingUp w

atBottomEdge :: Walker -> Bool
atBottomEdge w = (y w) == (yB w) && headingDown w

moveLeft :: Walker -> Walker
moveLeft w@(Walker { x = cX }) = w { x = cX - 1 }

moveRight :: Walker -> Walker
moveRight w@(Walker { x = cX }) = w { x = cX + 1 }

moveUp :: Walker -> Walker
moveUp w@(Walker { y = cY }) = w { y = cY + 1 }

moveDown :: Walker -> Walker
moveDown w@(Walker { y = cY }) = w { y = cY - 1 }

headLeft :: Walker -> Walker
headLeft w@(Walker { xB = cXB }) = w { dX = (-1), dY = 0, xB = -cXB }

headRight :: Walker -> Walker
headRight w@(Walker { xB = cXB }) = w { dX = 1, dY = 0, xB = abs cXB + 1 }

headUp :: Walker -> Walker
headUp w@(Walker { yB = cYB }) = w { dY = 1, dX = 0, yB = abs cYB + 1 }

headDown :: Walker -> Walker
headDown w@(Walker { yB = cYB }) = w { dY = (-1), dX = 0, yB = -cYB }

loop :: (a -> a) -> a -> [a]
loop f a = let next = f a in a : loop f next

spirals :: [(Int, Int)]
spirals = map (\w -> (x w, y w)) $ loop move start
  where
    start = Walker { x = 0, y = 0, dX = 1, dY = 0, xB = 1, yB = 0 }

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = abs (y2 - y1) + abs (x2 - x1)

main :: IO ()
main = do
  let answerP1 = distance (0, 0) $ last $ take 312051 spirals
  putStrLn $ show answerP1
