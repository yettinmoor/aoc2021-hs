{-# LANGUAGE TupleSections #-}

import           Control.Arrow                ((&&&))
import           Control.Monad
import           Data.Char
import           Data.Function
import           Data.List
import qualified Data.Map                     as M
import           Data.Semigroup
import           Text.ParserCombinators.ReadP

type Point = (Int, Int)

type Line = (Point, Point)

-- Only works for horizontal, vertical, and 45 degree lines.
linePoints :: Line -> [Point]
linePoints ((x1, y1), (x2, y2))
  | x1 == x2 = zip (repeat x1) [min y1 y2 .. max y1 y2]
  | y1 == y2 = zip [min x1 x2 .. max x1 x2] (repeat y1)
  | (x1 < x2) == (y1 < y2) = zip [min x1 x2 .. max x1 x2] [min y1 y2 .. max y1 y2]
  | otherwise = zip [min x1 x2 .. max x1 x2] (reverse [min y1 y2 .. max y1 y2])

isOrtho :: Line -> Bool
isOrtho ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

intersections :: [Line] -> [Point]
intersections = M.keys . M.filter (> 1) . foldl1 (M.unionWith (<>)) . map lineMap
  where
    lineMap = M.fromList . map (, Sum 1) . linePoints

part1 :: [Line] -> Int
part1 = length . intersections . filter isOrtho

part2 :: [Line] -> Int
part2 = length . intersections

main :: IO ()
main = input >>= print . (part1 &&& part2)

input :: IO [Line]
input = map (fst . head . readP_to_S parseLine) . lines <$> readFile "05.txt"

parseLine :: ReadP Line
parseLine = do
  let number = read <$> munch1 isDigit
  let point = (,) <$> number <*> (char ',' *> number)
  p1 <- point
  string " -> "
  p2 <- point
  eof
  return (p1, p2)

test :: IO [Line]
test =
  return
    [ ((0, 9), (5, 9))
    , ((8, 0), (0, 8))
    , ((9, 4), (3, 4))
    , ((2, 2), (2, 1))
    , ((7, 0), (7, 4))
    , ((6, 4), (2, 0))
    , ((0, 9), (2, 9))
    , ((3, 4), (1, 4))
    , ((0, 0), (8, 8))
    , ((5, 5), (8, 2))
    ]
