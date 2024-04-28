import Data.Char (isNumber)
import Data.Function
import Data.List (elemIndex)
import qualified Data.Map as M
import Data.Maybe
import System.IO

data Dir = Forward Int | Turn Int deriving (Show)

parseMaze lines =
  let start = (1 + (lines & head & elemIndex '.' & fromJust), 1)
      maze =
        lines
          & map (zip [1 ..])
          & zip [1 ..]
          & foldl
            ( \map (row, line) ->
                line
                  & foldl
                    ( \m (col, c) -> case c of
                        '.' -> M.insert (col, row) True m
                        '#' -> M.insert (col, row) False m
                        _ -> m
                    )
                    map
            )
            M.empty
   in (start, maze)

parseDir [] = []
parseDir line
  | isNumber (head line) = Forward (line & takeWhile isNumber & read) : parseDir (dropWhile isNumber line)
  | head line == 'R' = Turn 1 : parseDir (tail line)
  | otherwise = Turn (-1) : parseDir (tail line)

parseInput lines =
  let (mazeInput, dirInput) = break null lines
   in (parseMaze mazeInput, parseDir (dirInput !! 1))

getValue (1, 0) = 0
getValue (-1, 0) = 2
getValue (0, 1) = 1
getValue (0, -1) = 3

turn dir x =
  let dirs = [(1, 0), (0, 1), (-1, 0), (0, -1)]
      i = fromJust $ elemIndex dir dirs
      next = if (i + x) < 0 then dirs !! (i + x + 4) else dirs !! ((i + x) `mod` 4)
   in next

add (x, y) (a, b) = (x + a, y + b)

wrap pos dir maze =
  let back = turn (turn dir 1) 1
      candidates = iterate (add back) pos
   in last $ takeWhile (`M.member` maze) candidates

wrapCube (x, y) dir maze =
  let cubeX = x `div` 50
      cubeY = y `div` 50
      nextCubePos = add (cubeX, cubeY) dir
   in case ((cubeX, cubeY), dir) of
        ((1, 0), (0, -1)) -> ((x, 150), (1,0))
        ((1, 0), (-1, 0)) -> ((0, 150), (1,0))

processMove pos [] dir _ = 1000 * snd pos + 4 * fst pos + getValue dir
processMove pos ((Turn x) : rest) dir maze = processMove pos rest (turn dir x) maze
processMove pos ((Forward x) : rest) dir maze =
  let candidates = iterate (add dir) pos
      (valid, others) = span (\pos -> M.member pos maze && maze M.! pos) candidates
      wrapped = wrap (last valid) dir maze
   in if length valid > x
        then processMove (valid !! x) rest dir maze
        else
          if not (M.member (head others) maze) && maze M.! wrapped
            then processMove wrapped (Forward (x - length valid) : rest) dir maze
            else processMove (last valid) rest dir maze

processMove' pos [] dir _ = 1000 * snd pos + 4 * fst pos + getValue dir
processMove' pos ((Turn x) : rest) dir maze = processMove' pos rest (turn dir x) maze
processMove' pos ((Forward x) : rest) dir maze =
  let candidates = iterate (add dir) pos
      (valid, others) = span (\pos -> M.member pos maze && maze M.! pos) candidates
      (wrappedPos, wrappedDir) = wrapCube (last valid) dir maze
   in if length valid > x
        then processMove' (valid !! x) rest dir maze
        else
          if not (M.member (head others) maze) && maze M.! wrappedPos
            then processMove' wrappedPos (Forward (x - length valid) : rest) wrappedDir maze
            else processMove' (last valid) rest dir maze

part1 contents =
  let ((start, maze), dir) = contents & lines & parseInput
   in processMove start dir (1, 0) maze & show

part2 contents =
  let ((start, maze), dir) = contents & lines & parseInput
   in processMove' start dir (1, 0) maze & show

main = do
  small_contents <- readFile "in.small"
  contents <- readFile "in"

  putStrLn $ show (turn (1, 0) (-1))
  putStrLn $ part1 small_contents
  putStrLn $ part1 contents
  putStrLn $ part2 small_contents
  putStrLn $ part2 contents
