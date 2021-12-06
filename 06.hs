import           Control.Arrow   ((&&&))
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Semigroup

count :: Ord k => [k] -> M.Map k (Sum Int)
count = foldr (M.unionWith (<>) . flip M.singleton (Sum 1)) M.empty

solve :: Int -> [Int] -> Int
solve n = getSum . foldMap snd . M.toList . (!! n) . iterate go . count
  where
    go m =
      let zeroes = fromMaybe mempty (M.lookup 0 m)
       in M.unionWith (<>) (M.fromList [(6, zeroes), (8, zeroes)]) .
          M.mapKeys (subtract 1) . M.delete 0 $
          m

part1 :: [Int] -> Int
part1 = solve 80

part2 :: [Int] -> Int
part2 = solve 256

main :: IO ()
main = input >>= print . (part1 &&& part2)

input :: IO [Int]
input = read . (\xs -> "[" ++ xs ++ "]") <$> readFile "06.txt"

test :: IO [Int]
test = return [3, 4, 3, 1, 2]
