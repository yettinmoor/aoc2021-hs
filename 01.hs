import           Control.Monad

windows :: Int -> [a] -> [[a]]
windows n = takeWhile (\xs -> length xs == n) . liftM2 (:) (take n) (windows n . tail)

part1 :: [Int] -> Int
part1 = length . filter id . liftM2 (zipWith (<)) id tail

part2 :: [Int] -> Int
part2 = part1 . map sum . windows 3

main :: IO ()
main = do
  i <- input
  print $ part1 i
  print $ part2 i

input :: IO [Int]
input = map read . lines <$> readFile "01.txt"

test :: IO [Int]
test = return [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
