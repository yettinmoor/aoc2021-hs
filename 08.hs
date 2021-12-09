import           Control.Arrow ((&&&))
import           Data.Char
import           Data.List
import qualified Data.Map      as Map
import           Data.Maybe
import qualified Data.Set      as Set

type Point = (Int, Int)

type Grid a = Map.Map Point a

neighbors :: Point -> Set.Set Point
neighbors (y, x) = Set.fromList [(y, x), (y, x - 1), (y, x + 1), (y - 1, x), (y + 1, x)]

lowPoints :: Ord a => Grid a -> Grid a
lowPoints m = Map.filterWithKey (\k v -> all (>= v) $ m `Map.restrictKeys` neighbors k) m

basin :: Grid Int -> (Point, Int) -> Grid Int
basin m (p, h) =
  let found = Map.filter (\v -> v /= 9 && v >= h) $ m `Map.restrictKeys` neighbors p
   in foldl Map.union found $ map (basin (m Map.\\ found)) (Map.toList found)

part1 :: Grid Int -> Int
part1 = sum . map (+ 1) . Map.elems . lowPoints

part2 :: Grid Int -> Int
part2 m = product . take 3 . sortOn negate . map (Map.size . basin m) . Map.toList . lowPoints $ m

main :: IO ()
main = input >>= print . (part1 &&& part2) . toGrid

toGrid :: [[a]] -> Grid a
toGrid = Map.fromList . concat . zipWith zip [[(y, x) | x <- [0 ..]] | y <- [0 ..]]

input :: IO [[Int]]
input = map (map digitToInt) . lines <$> readFile "08.txt"

test :: IO [[Int]]
test =
  return
    [ [2, 1, 9, 9, 9, 4, 3, 2, 1, 0]
    , [3, 9, 8, 7, 8, 9, 4, 9, 2, 1]
    , [9, 8, 5, 6, 7, 8, 9, 8, 9, 2]
    , [8, 7, 6, 7, 8, 9, 6, 7, 8, 9]
    , [9, 8, 9, 9, 9, 6, 5, 6, 7, 8]
    ]
