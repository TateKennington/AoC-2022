import Data.Function
import Data.List (elemIndex, findIndex)
import Data.Maybe
import System.IO

mix x i l
  | abs x >= l -1 = mix (x `mod` (l -1)) i l
  | i + x >= l = i + x - l + 1
  | otherwise = i + x

insert i j arr =
  let newArr = take i arr ++ drop (i + 1) arr
   in if j >= 0 then take j newArr ++ (arr !! i) : drop j newArr else take (length newArr + j) newArr ++ (arr !! i) : drop (length newArr + j) newArr

times 1 arr = arr
times n arr = arr ++ times (n -1) arr

mixArr :: [Int] -> Int -> [Int]
mixArr arr n =
  let indexedArr :: [(Int, Int)]
      indexedArr = zip arr [0 ..]
      mixed = foldl (\acc num -> let i = fromJust (elemIndex num acc) in insert i (mix (fst num) i (length acc)) acc) indexedArr (times n indexedArr)
      newArr = mixed & map fst
   in newArr

part1 contents =
  let arr :: [Int]
      arr =
        contents
          & lines
          & map read
      mixed = mixArr arr 1
      i = fromJust $ elemIndex 0 mixed
      cycled = mixed & cycle
   in cycled !! (i + 1000) + cycled !! (i + 2000) + cycled !! (i + 3000) & show

part2 contents =
  let arr :: [Int]
      arr =
        contents
          & lines
          & map ((* 811589153) . read)
      mixed = mixArr arr 10
      i = fromJust $ elemIndex 0 mixed
      cycled = mixed & cycle
   in cycled !! (i + 1000) + cycled !! (i + 2000) + cycled !! (i + 3000) & show

main = do
  small_contents <- readFile "in.small"
  contents <- readFile "in"

  putStrLn $ part1 small_contents
  putStrLn $ part1 contents
  putStrLn $ part2 small_contents
  putStrLn $ part2 contents
