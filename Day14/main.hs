import Data.Bifunctor (Bifunctor (second))
import Data.Char (isNumber)
import Data.Function
import Data.List (find)
import Data.Maybe (fromJust, isJust)
import Data.Set (empty, insert, member)
import System.IO

add (x, y) (a, b) = (x + a, y + b)

group' [x] = []
group' arr = take 2 arr : group' (drop 1 arr)

parsePoints :: [Char] -> [(Int, Int)]
parsePoints [] = []
parsePoints line =
  let (x, rest) = span isNumber line
      (y, rest') = rest & drop 1 & span isNumber
   in (read x, read y) : parsePoints (drop 4 rest')

parseLine set line =
  let points = parsePoints line
   in map
        ( \[start, end] ->
            [ (x, y)
              | x <- [(min (fst start) (fst end)) .. (max (fst start) (fst end))],
                y <- [(min (snd start) (snd end)) .. (max (snd start) (snd end))]
            ]
        )
        (group' points)
        & foldl (\(set, m) arr -> foldl (\(set, m) item -> (insert item set, max (snd item) m)) (set, m) arr) (set, 0)

parseLines =
  foldl
    ( \(set, m) line ->
        let (newSet, newM) = parseLine set line
         in (newSet, max m newM)
    )
    (empty, 0)

placeSand (set, maxDepth, pos) =
  let nextPositions = [pos `add` (0, 1), pos `add` (-1, 1), pos `add` (1, 1)]
      nextPos = find (\pos -> not $ member pos set) nextPositions
   in if snd pos > maxDepth
        then 0
        else
          if isJust nextPos
            then placeSand (set, maxDepth, fromJust nextPos)
            else 1 + placeSand (insert pos set, maxDepth, (500, 0))

placeSand' (set, maxDepth, pos) =
  let nextPositions = [pos `add` (0, 1), pos `add` (-1, 1), pos `add` (1, 1)]
      nextPos = find (\pos -> snd pos < maxDepth && not (member pos set)) nextPositions
   in if isJust nextPos
        then placeSand' (set, maxDepth, fromJust nextPos)
        else
          if pos == (500, 0)
            then 1
            else 1 + placeSand' (insert pos set, maxDepth, (500, 0))

part1 contents =
  parseLines (lines contents) & (\(a, b) -> (a, b, (500, 0))) & placeSand & show

part2 contents =
  parseLines (lines contents) & (\(a, b) -> (a, b + 2, (500, 0))) & placeSand' & show

main = do
  small_contents <- readFile "in.small"
  contents <- readFile "in"

  putStrLn $ part1 small_contents
  putStrLn $ part1 contents
  putStrLn $ part2 small_contents
  putStrLn $ part2 contents
