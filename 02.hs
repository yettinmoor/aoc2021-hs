import           Control.Monad
import           Data.Function
import           State

data Inst
  = X Int
  | Y Int

parseInst :: String -> Inst
parseInst s =
  case words s of
    ["forward", d] -> X $ read d
    ["up", d]      -> Y $ -read d
    ["down", d]    -> Y $ read d
    _              -> undefined

part1 :: [Inst] -> Int
part1 = uncurry (*) . foldl go (0, 0)
  where
    go (x, y) (X d) = (x + d, y)
    go (x, y) (Y d) = (x, y + d)

part2 :: [Inst] -> Int
part2 = uncurry (*) . fst . foldl go ((0, 0), 0)
  where
    go ((x, y), aim) (X d) = ((x + d, y + d * aim), aim)
    go ((x, y), aim) (Y d) = ((x, y), aim + d)

main :: IO ()
main = do
  i <- input
  print $ part1 i
  print $ part2 i

input :: IO [Inst]
input = map parseInst . lines <$> readFile "02.txt"

test :: IO [Inst]
test = return $ map parseInst ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]
