import           Control.Arrow ((&&&))

solve :: (Int -> Int -> Int) -> [Int] -> Int
solve costFn xs = minimum $ map (\x -> sum $ map (costFn x) xs) [minimum xs .. maximum xs]

part1 :: [Int] -> Int
part1 = solve $ \x y -> abs $ x - y

part2 :: [Int] -> Int
part2 = solve $ \x y -> triangleNumber (abs $ x - y)

triangleNumber :: Int -> Int
triangleNumber n = n * (n + 1) `div` 2

main :: IO ()
main = input >>= print . (part1 &&& part2)

input :: IO [Int]
input = read . (\xs -> "[" ++ xs ++ "]") <$> readFile "07.txt"

test :: IO [Int]
test = return [16, 1, 2, 0, 4, 2, 7, 1, 2, 14]
