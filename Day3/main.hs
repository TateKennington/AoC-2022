import Data.Char
import Data.Function
import qualified Data.Set as Set
import System.IO

group _ [] = []
group n list = take n list : group n (drop n list)

priority a =
  if isAsciiLower a
    then ord a - ord 'a' + 1
    else ord a - ord 'A' + 27

part1 contents =
  lines contents
    & map
      ( \line ->
          let (first, second) = splitAt (length line `div` 2) line
              a = Set.fromList first
              b = Set.fromList second
              overlap = Set.elemAt 0 $ Set.intersection a b
           in priority overlap
      )
    & sum
    & show

part2 contents =
  lines contents
    & group 3
    & map
      ( \group ->
          let [first, second, third] = map Set.fromList group
              overlapSet = Set.intersection first $ Set.intersection second third
              overlap = Set.elemAt 0 overlapSet
           in priority overlap
      )
    & sum
    & show

main = do
  small_contents <- readFile "in.small"
  contents <- readFile "in"

  putStrLn $ part1 small_contents
  putStrLn $ part1 contents
  putStrLn $ part2 small_contents
  putStrLn $ part2 contents
