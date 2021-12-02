import           Control.Monad
import           Data.Function
import           State

data Inst
  = Forward Int
  | Up Int
  | Down Int

parseInst :: String -> Inst
parseInst s =
  case words s of
    ["forward", x] -> Forward $ read x
    ["up", x]      -> Up $ read x
    ["down", x]    -> Down $ read x
    _              -> undefined

step1 :: Inst -> (Int, Int)
step1 (Forward x) = (x, 0)
step1 (Up x)      = (0, -x)
step1 (Down x)    = (0, x)

-- I cannot believe this worked
step2 :: Inst -> State Int (Int, Int)
step2 (Forward x) = gets $ \aim -> (x, x * aim)
step2 (Up x)      = modify (subtract x) >> return (0, 0)
step2 (Down x)    = modify (+ x) >> return (0, 0)

foldPairs :: [(Int, Int)] -> Int
foldPairs = uncurry (*) . foldr1 (\(a, b) (c, d) -> (a + c, b + d))

part1 :: [Inst] -> Int
part1 = foldPairs . map step1

part2 :: [Inst] -> Int
part2 = foldPairs . flip evalState 0 . mapM step2

main :: IO ()
main = do
  i <- input
  print $ part1 i
  print $ part2 i

input :: IO [Inst]
input = map parseInst . lines <$> readFile "02.txt"

test :: IO [Inst]
test = return $ map parseInst ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]
