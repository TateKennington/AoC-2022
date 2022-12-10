import Data.Char
import Data.Function
import Data.List (transpose)
import System.IO

part1 contents =
  contents
    & lines
    & map (processRow . processRow . map (\c -> (ord c - ord '0', 0)))
    & transpose
    & sum . map (sum . map snd . processRow . processRow)
    & show
  where
    processRow =
      snd
        . foldl
          ( \(max, row) x ->
              if max < fst x
                then (fst x, (fst x, 1) : row)
                else (max, x : row)
          )
          (-1, [])

takeWhile' _ [] = []
takeWhile' pred list =
  if pred (head list)
    then head list : takeWhile' pred (tail list)
    else [head list]

getVisibility trees =
  trees
    & map
      ( \row ->
          map
            (\index -> row & take index & reverse & takeWhile' (< (row !! index)) & length)
            [0 .. length row - 1]
      )

part2 contents =
  let trees =
        contents
          & lines
          & map (map (\c -> ord c - ord '0'))
   in getVisibility trees
        & zipWith (zipWith (*)) (map reverse (getVisibility (map reverse trees)))
        & zipWith (zipWith (*)) (transpose (getVisibility (transpose trees)))
        & zipWith (zipWith (*)) (transpose $ map reverse (getVisibility (map reverse (transpose trees))))
        & maximum . map maximum
        & show

main = do
  small_contents <- readFile "in.small"
  contents <- readFile "in"

  putStrLn $ part1 small_contents
  putStrLn $ part1 contents
  putStrLn $ part2 small_contents
  putStrLn $ part2 contents
