import Data.Function
import Data.List (nub)
import qualified Data.Set as S
import System.IO

add (x, y, z) (a, b, c) = (x + a, b + y, z + c)

parseLine :: [Char] -> (Int, Int, Int)
parseLine line =
  line & map (\c -> if c == ',' then ' ' else c) & words & map read & (\arr -> (arr !! 0, arr !! 1, arr !! 2))

checkNeighbours :: S.Set (Int, Int, Int) -> (Int, Int, Int) -> Int
checkNeighbours set point =
  [ (1, 0, 0),
    (-1, 0, 0),
    (0, 1, 0),
    (0, -1, 0),
    (0, 0, 1),
    (0, 0, -1)
  ]
    & map (\dir -> if S.member (add dir point) set then 0 else 1)
    & sum

checkNeighbours' :: S.Set (Int, Int, Int) -> ((Int, Int, Int) -> Bool) -> (Int, Int, Int) -> Int
checkNeighbours' set isOutside point =
  [ (1, 0, 0),
    (-1, 0, 0),
    (0, 1, 0),
    (0, -1, 0),
    (0, 0, 1),
    (0, 0, -1)
  ]
    & map (\dir -> if S.member (add dir point) set || not (isOutside (add dir point)) then 0 else 1)
    & sum

-- isOutside points maxX maxY maxZ set =
--   let newPoints =
--         nub $
--           concatMap
--             ( \point ->
--                 [ (1, 0, 0),
--                   (-1, 0, 0),
--                   (0, 1, 0),
--                   (0, -1, 0),
--                   (0, 0, 1),
--                   (0, 0, -1)
--                 ]
--                   & map (add point)
--                   & filter (\p -> not (S.member p set))
--             )
--             points
--    in if any (\(x, y, z) -> not (S.member (x, y, z) set) && (x > maxX || y > maxY || z > maxZ || x < 0 || y < 0 || z < 0)) points
--         then True
--         else
--           if newPoints == points
--             then False
--             else
--               isOutside
--                 newPoints
--                 maxX
--                 maxY
--                 maxZ
--                 set

part1 contents =
  let set =
        contents
          & lines
          & map parseLine
          & S.fromList
   in set & S.toList & map (checkNeighbours set) & sum & show

part2 contents =
  let points =
        contents
          & lines
          & map parseLine
      set = S.fromList points
      maxX = points & map (\(x, _, _) -> x) & maximum
      maxY = points & map (\(_, y, _) -> y) & maximum
      maxZ = points & map (\(_, _, z) -> z) & maximum
      f (x, y, z)
        | x < 0 || x > maxX = True
        | y < 0 || y > maxY = True
        | z < 0 || z > maxZ = True
        | S.member (x, y, z) set = False
        | otherwise =
          any
            ( f . add (x, y, z)
            )
            [ (1, 0, 0),
              (-1, 0, 0),
              (0, 1, 0),
              (0, -1, 0),
              (0, 0, 1),
              (0, 0, -1)
            ]
      m = map (\x -> map (\y -> map (\z -> f (x, y, z)) [0 .. maxZ]) [0 .. maxY]) [0 .. maxX]
      fm (x, y, z) = if x < 0 || y < 0 || z < 0 then True else m !! x !! y !! z
      test = f (2, 2, 5)
   in test & show

main = do
  small_contents <- readFile "in.small"
  contents <- readFile "in"

  putStrLn $ part1 small_contents
  putStrLn $ part1 contents
  putStrLn $ part2 small_contents
  putStrLn $ part2 contents
