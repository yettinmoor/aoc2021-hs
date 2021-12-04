import           Control.Monad
import           Data.Bifunctor
import           Data.Function
import           Data.List
import           Data.Maybe
import qualified Data.Set       as S

type Board = [Int]

split :: Eq a => a -> [a] -> [[a]]
split c [] = []
split c xs = liftM2 (:) (takeWhile (/= c)) (split c . drop 1 . dropWhile (/= c)) xs

winningSpans :: [[Int]]
winningSpans = [[x .. x + 4] | x <- [0,5 .. 20]] ++ [[x,x + 5 .. x + 20] | x <- [0 .. 4]]

-- Return score iff board has bingo.
hasBingo :: [Int] -> Board -> Maybe Int
hasBingo draw board =
  let (marked, unmarked) = partition ((`elem` draw) . snd) $ zip [0 ..] board
   in if any (all (`elem` map fst marked)) winningSpans
        then Just $ sum (map snd unmarked) * last draw
        else Nothing

findWinners :: [Int] -> [Board] -> [(Int, Int)]
findWinners draws boards =
  nubBy ((==) `on` fst) $ do
    draw <- inits draws
    mapMaybe (sequence . second (hasBingo draw)) . zip [1 ..] $ boards

main :: IO ()
main = input >>= print . uncurry findWinners . parse

parse :: String -> ([Int], [Board])
parse =
  bimap (map read . split ',') (map (concatMap (map read . words))) .
  liftM2 (,) (head . head) tail . split "" . lines

input :: IO String
input = readFile "04.txt"

test :: IO String
test =
  return . unlines $
  [ "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
  , ""
  , "22 13 17 11  0"
  , " 8  2 23  4 24"
  , "21  9 14 16  7"
  , " 6 10  3 18  5"
  , " 1 12 20 15 19"
  , ""
  , " 3 15  0  2 22"
  , " 9 18 13 17  5"
  , "19  8  7 25 23"
  , "20 11 10 24  4"
  , "14 21 16 12  6"
  , ""
  , "14 21 17 24  4"
  , "10 16 15  9 19"
  , "18  8 23 26 20"
  , "22 11 13  6  5"
  , " 2  0 12  3  7"
  ]
