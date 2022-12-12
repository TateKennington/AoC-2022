import Data.Char (ord)
import Data.Function
import qualified Data.Set as Set
import System.IO

getAdj grid curr =
  let elevation = (grid !! fst curr) !! snd curr
   in [(fst curr + 1, snd curr), (fst curr - 1, snd curr), (fst curr, snd curr + 1), (fst curr, snd curr - 1)]
        & filter ((>= 0) . fst)
        & filter ((>= 0) . snd)
        & filter ((< length (head grid)) . snd)
        & filter ((< length grid) . fst)
        & filter (\(x, y) -> ((grid !! x) !! y) <= elevation || ((grid !! x) !! y) - elevation == 1)

getAdjDescending grid curr =
  let elevation = (grid !! fst curr) !! snd curr
   in [(fst curr + 1, snd curr), (fst curr - 1, snd curr), (fst curr, snd curr + 1), (fst curr, snd curr - 1)]
        & filter ((>= 0) . fst)
        & filter ((>= 0) . snd)
        & filter ((< length (head grid)) . snd)
        & filter ((< length grid) . fst)
        & filter (\(x, y) -> ((grid !! x) !! y) >= elevation || ((grid !! x) !! y) - elevation == -1)

processInput input =
  let (row, g, start, end) =
        foldl
          ( \(row, grid, start, end) c ->
              let elevation = case c of
                    'S' -> ord 'a'
                    'E' -> ord 'z'
                    _ -> ord c
                  newStart = if c == 'S' then (length grid, length row) else start
                  newEnd = if c == 'E' then (length grid, length row) else end
                  newRow = if c == '\n' then [] else row ++ [elevation]
                  newGrid = if c == '\n' then grid ++ [row] else grid
               in (newRow, newGrid, newStart, newEnd)
          )
          ([], [], (0, 0), (0, 0))
          input
      grid = g ++ [row]
   in (grid, start, end)

findSteps grid start pred listAdj =
  iterate
    ( \(queue, seen) ->
        let (curr, steps) = head queue
            adj = listAdj grid curr & filter (\adj -> not $ Set.member adj seen)
            newSeen = foldl (flip Set.insert) seen (curr : adj)
            newQueue = tail queue ++ map (\adj -> (adj, steps + 1)) adj
         in (newQueue, newSeen)
    )
    ([(start, 0)], Set.empty)
    & dropWhile (\(queue, _) -> not (null queue) && not (pred (queue & head)))
    & fst . head
    & (\queue -> if null queue then length grid ^ 3 else snd (head queue))

part1 contents =
  let (grid, start, end) = processInput contents
   in findSteps grid start ((== end) . fst) getAdj
        & show

part2 contents =
  let (grid, start, end) = processInput contents
   in findSteps grid end (\((x, y), _) -> (grid !! x) !! y == ord 'a') getAdjDescending
        & show

main = do
  small_contents <- readFile "in.small"
  contents <- readFile "in"

  putStrLn $ part1 small_contents
  putStrLn $ part1 contents
  putStrLn $ part2 small_contents
  putStrLn $ part2 contents
