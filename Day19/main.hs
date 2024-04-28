import Data.Char (isNumber, isSpace)
import Data.Function
import Data.List (nub)
import qualified Data.Set as S
import System.IO

parseLine :: [Char] -> [[Int]]
parseLine line =
  line
    & filter (\c -> isSpace c || isNumber c)
    & words
    & map read
    & (\arr -> [[arr !! 1, 0, 0, 0], [arr !! 2, 0, 0, 0], [arr !! 3, arr !! 4, 0, 0], [arr !! 5, 0, arr !! 6, 0]])

parseInput lines =
  lines & map parseLine

incAt :: Num a => Int -> a -> [a] -> [a]
incAt n x list = take n list ++ (list !! n) + x : drop (n + 1) list

decAt n x list = take n list ++ (list !! n) - x : drop (n + 1) list

processBlueprintState (states, blueprint, time) =
  let raw =
        states
          & foldl
            ( \set (robots, resources) ->
                let newResources = zipWith (+) resources robots
                    newStates =
                      foldl
                        ( \acc index ->
                            let cost = blueprint !! index
                                diff = zipWith (-) resources cost
                             in if all (>= 0) diff then S.insert (incAt index 1 robots, zipWith (+) diff robots) acc else acc
                        )
                        (S.singleton (robots, newResources))
                        [0 .. length blueprint - 1]
                 in S.union set newStates
            )
            S.empty
   in -- m = raw & S.toList & map ((!! 3) . snd) & maximum
      -- filtered =
      --   raw
      --     & S.filter (\(_, resources) -> all (\i -> all (\b -> (b !! i) == 0 || (robots !! i) <= (b !! i)) blueprint) [0 .. 2])
      -- & S.filter (\(robots, _) -> all (\i -> all (\b -> (b !! i) == 0 || (robots !! i) <= (b !! i)) blueprint) [0 .. 2])
      -- & S.filter (\(_, resources) -> all (\i -> all (\b -> (b !! i) == 0 || (resources !! i) <= (b !! i) * time) blueprint) [0 .. 2])
      (raw, blueprint, time - 1)

processBlueprint robots blueprint =
  let states = iterate processBlueprintState (S.singleton (robots, [0, 0, 0, 0]), blueprint, 24)
   in take 24 states & last & (\(f, _, _) -> f) & S.toList & map ((!! 3) . snd) & maximum

part1 contents =
  let blueprints = contents & lines & parseInput
   in blueprints
        & map (processBlueprint [1, 0, 0, 0])
        & show

part2 contents =
  ""

main = do
  small_contents <- readFile "in.small"
  contents <- readFile "in"

  putStrLn $ part1 small_contents
  -- putStrLn $ part1 contents
  putStrLn $ part2 small_contents
  putStrLn $ part2 contents
