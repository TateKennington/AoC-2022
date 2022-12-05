import Data.Char
import Data.Function
import Data.List (transpose)
import System.IO

replaceAt n item list = take n list ++ item : drop (n + 1) list

part1 contents =
  let input = lines contents
      (stackInput, rest) = input & break (any isNumber)
      moves = drop 2 rest
      stack =
        stackInput & transpose
          & map (filter isAlpha)
          & filter (not . null)
      movedStack =
        foldl
          ( \acc move ->
              let count, from, to :: Int
                  [count, from, to] = move & filter (\c -> isNumber c || isSpace c) & words & map read
                  moved = acc !! (from -1) & take count & reverse
                  new = moved ++ acc !! (to - 1)
                  left = acc !! (from -1) & drop count
                  stack = acc & replaceAt (from -1) left & replaceAt (to - 1) new
               in stack
          )
          stack
          moves
   in show $ map head movedStack

part2 contents =
  let input = lines contents
      (stackInput, rest) = input & break (any isNumber)
      moves = drop 2 rest
      stack =
        stackInput & transpose
          & map (filter isAlpha)
          & filter (not . null)
      movedStack =
        foldl
          ( \acc move ->
              let count, from, to :: Int
                  [count, from, to] = move & filter (\c -> isNumber c || isSpace c) & words & map read
                  moved = acc !! (from -1) & take count
                  new = moved ++ acc !! (to - 1)
                  left = acc !! (from -1) & drop count
                  stack = acc & replaceAt (from -1) left & replaceAt (to - 1) new
               in stack
          )
          stack
          moves
   in show $ map head movedStack

main = do
  small_contents <- readFile "in.small"
  contents <- readFile "in"

  putStrLn $ part1 small_contents
  putStrLn $ part1 contents
  putStrLn $ part2 small_contents
  putStrLn $ part2 contents
