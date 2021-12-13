import           Control.Applicative
import           Control.Monad
import           Data.Bifunctor
import           Data.List
import           Data.Maybe
import qualified Data.Set            as Set

type Point = (Int, Int)

data Fold
  = X Int
  | Y Int
  deriving (Show)

data Map =
  Map
    { points :: Set.Set Point
    , folds  :: [Fold]
    }
  deriving (Show)

step :: Map -> Map
step (Map ps (f:fs)) = Map (Set.map (go f) ps) fs
  where
    go (X f) (x, y) = (min x (2 * f - x), y)
    go (Y f) (x, y) = (x, min y (2 * f - y))
step m = m

pointsToStr :: Set.Set Point -> String
pointsToStr ps = unlines [[" #" !! fromEnum ((x, y) `elem` ps) | x <- xs] | y <- ys]
  where
    xs = range $ Set.map fst ps
    ys = range $ Set.map snd ps
    range = liftM2 enumFromTo minimum maximum

part1 :: Map -> Int
part1 = Set.size . points . step

part2 :: Map -> String
part2 = pointsToStr . points . until (null . folds) step

main :: IO ()
main = do
  i <- input
  print $ part1 i
  putStrLn $ part2 i

parse :: String -> Map
parse =
  uncurry Map . bimap (Set.fromList . map parsePair) (map parseFold . tail) . break null . lines
  where
    parsePair s = read $ "(" ++ s ++ ")"
    parseFold s = fromJust $ (X . read <$> stripPrefix xp s) <|> (Y . read <$> stripPrefix yp s)
    xp = "fold along x="
    yp = "fold along y="

input :: IO Map
input = parse <$> readFile "13.txt"
