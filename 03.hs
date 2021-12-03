import           Control.Monad
import           Data.Char
import           Data.Function
import           Data.List
import           Data.Maybe
import           State

mostCommon :: [Int] -> Int
mostCommon xs =
  case uncurry (compare `on` length) (partition (== 1) xs) of
    LT -> 0
    GT -> 1
    EQ -> 1

leastCommon :: [Int] -> Int
leastCommon xs = 1 - mostCommon xs

toNum :: Int -> [Int] -> Int
toNum base = foldl1 ((+) . (* base))

part1 :: [[Int]] -> Int
part1 = liftM2 ((*) `on` toNum 2) id (map (1 -)) . map mostCommon . transpose

part2 :: [[Int]] -> Int
part2 = liftM2 ((*) `on` toNum 2 . flip execState []) (go mostCommon) (go leastCommon)
  where
    go _ [x] = modify (\prefix -> reverse prefix ++ x)
    go f xs = do
      let prefix = f . head . transpose $ xs
      modify (prefix :)
      go f . mapMaybe (stripPrefix [prefix]) $ xs

main :: IO ()
main = do
  i <- input
  print $ part1 i
  print $ part2 i

input :: IO [[Int]]
input = map (map digitToInt) . lines <$> readFile "03.txt"

test :: IO [[Int]]
test =
  return . map (map digitToInt) $
  [ "00100"
  , "11110"
  , "10110"
  , "10111"
  , "10101"
  , "01111"
  , "00111"
  , "11100"
  , "10000"
  , "11001"
  , "00010"
  , "01010"
  ]
