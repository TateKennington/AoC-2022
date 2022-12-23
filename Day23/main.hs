import Data.Function
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import System.IO

add (x, y) (a, b) = (x + a, y + b)

sub (x, y) (a, b) = (x - a, y - b)

tangent (x, y) = (- y, x)

parseInput lines =
  lines
    & map (zip [0 ..])
    & zip [0 ..]
    & foldl (\s (y, line) -> foldl (\acc (x, c) -> if c == '#' then S.insert (x, y) acc else acc) s line) S.empty

calculateBounds positions =
  let list = S.toList positions
      maxX = list & map fst & maximum
      minX = list & map fst & minimum
      maxY = list & map snd & maximum
      minY = list & map snd & minimum
   in (1 + maxX - minX) * (1 + maxY - minY) - S.size positions

processRound :: S.Set (Int, Int) -> [(Int, Int)] -> Int -> S.Set (Int, Int)
processRound positions _ 0 = positions
processRound positions dirs turn =
  let proposals =
        positions
          & foldl
            ( \m pos ->
                if not (any (`S.member` positions) (concatMap (\dir -> map (add pos) [dir, dir `add` tangent dir, dir `sub` tangent dir]) dirs))
                  then M.insertWith (++) pos [pos] m
                  else
                    let maybeDir =
                          find
                            ( \dir -> not (any ((`S.member` positions) . add pos) [dir, dir `add` tangent dir, dir `sub` tangent dir])
                            )
                            dirs
                        newPos = case maybeDir of
                          (Just dir) -> pos `add` dir
                          Nothing -> pos
                     in M.insertWith (++) newPos [pos] m
            )
            M.empty
      newPositions =
        proposals & M.toList
          & foldl
            ( \s (pos, orig) ->
                if length orig > 1 then S.union s (S.fromList orig) else S.insert pos s
            )
            S.empty
   in processRound newPositions (tail dirs ++ [head dirs]) (turn - 1)

processRound' positions dirs =
  let proposals =
        positions
          & foldl
            ( \m pos ->
                if not (any (`S.member` positions) (concatMap (\dir -> map (add pos) [dir, dir `add` tangent dir, dir `sub` tangent dir]) dirs))
                  then M.insertWith (++) pos [pos] m
                  else
                    let maybeDir =
                          find
                            ( \dir -> not (any ((`S.member` positions) . add pos) [dir, dir `add` tangent dir, dir `sub` tangent dir])
                            )
                            dirs
                        newPos = case maybeDir of
                          (Just dir) -> pos `add` dir
                          Nothing -> pos
                     in M.insertWith (++) newPos [pos] m
            )
            M.empty
      newPositions =
        proposals & M.toList
          & foldl
            ( \s (pos, orig) ->
                if length orig > 1 then S.union s (S.fromList orig) else S.insert pos s
            )
            S.empty
   in if positions == newPositions then 1 else 1 + processRound' newPositions (tail dirs ++ [head dirs])

part1 contents =
  let positions = contents & lines & parseInput
      finalPositions = processRound positions [(0, -1), (0, 1), (-1, 0), (1, 0)] 10
   in calculateBounds finalPositions & show

part2 contents =
  let positions = contents & lines & parseInput
   in processRound' positions [(0, -1), (0, 1), (-1, 0), (1, 0)] & show

main = do
  small_contents <- readFile "in.small"
  contents <- readFile "in"

  putStrLn $ part1 small_contents
  putStrLn $ part1 contents
  putStrLn $ part2 small_contents
  putStrLn $ part2 contents
