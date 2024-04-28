import Data.Char (isNumber, isSpace)
import Data.Function
import Data.List (find, partition, nub)
import Data.Maybe (isNothing, fromJust)
-- import Data.Set (delete, empty, insert, size)
import System.IO

intersects (a, b) (c, d) = d >= a && c <= b

intersect (a, b) (c, d) = (max a c, min b d)
union (a, b) (c, d) = (min a c, max b d)
includes list x = case find (\(a,b)-> a<=x && x<=b) list of Just _ -> True
                                                            _ -> False

deleteAt list n = take n list ++ drop (n + 1) list

replaceAt list n item = take n list ++ (item : drop (n + 1) list)

width (a, b) = b - a

insertRange [] range = [range]
insertRange list range =
  let (joint, disjoint) = partition (intersects range) list
   in if not (null joint) then insertRange disjoint (foldl union range joint) else range:disjoint

dist (a, b) (c, d) = abs (a - c) + abs (b - d)

parseLine :: [Char] -> ((Int, Int), (Int, Int))
parseLine line =
  line & filter (\c -> c == '-' || isNumber c || isSpace c) & words & map read & (\arr -> ((arr !! 0, arr !! 1), (arr !! 2, arr !! 3)))

parseInput = map parseLine

part1 contents y =
  let sensors = contents & lines & parseInput & filter (\(a, b) -> dist a (fst a, y) <= dist a b)
      ranges = foldl (\acc (a, b) -> insertRange acc (fst a - (dist a b - dist (fst a, y) a), fst a + (dist a b - dist (fst a, y) a))) [] sensors
      totalWidth = ranges & foldl insertRange [] & map width & sum
      resWidth = totalWidth - length (filter((==y).snd.snd) sensors)
   in show totalWidth

part2 contents limit =
  let sensors = contents & lines & parseInput
      (x, y) = concatMap (\(a,b) -> [(x,y)|
        x <- [(fst a - dist a b - 1) ..(fst a + dist a b + 1)],
        y <- [(snd a - dist a b - 1) ..(snd a + dist a b + 1)],
        dist a b + 1 == dist a (x, y),
        x >= 0 && y >= 0 && x <= limit && y <= limit]
        ) sensors
        & find (\c -> all (\(a,b) -> dist a b < dist a c) sensors)
        & fromJust
  in x*4000000 + y & show


main = do
  small_contents <- readFile "in.small"
  contents <- readFile "in"

  putStrLn $ part1 small_contents 10
  putStrLn $ part1 contents 2000000
  putStrLn $ part2 small_contents 20
  putStrLn $ part2 contents 4000000
