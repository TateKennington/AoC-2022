import Data.Function
import Data.List (findIndex, nub)
import Data.Maybe (fromJust)
import System.IO

group _ [] = []
group n list = take n list : group n (drop 1 list)

findMarker contents n =
  contents
    & group n
    & findIndex (\group -> (group & nub & length) == n)
    & (+ n) . fromJust
    & show

part1 contents = findMarker contents 4

part2 contents = findMarker contents 14

main = do
  small_contents <- readFile "in.small"
  contents <- readFile "in"

  putStrLn $ part1 small_contents
  putStrLn $ part1 contents
  putStrLn $ part2 small_contents
  putStrLn $ part2 contents
