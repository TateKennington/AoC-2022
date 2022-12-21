import Data.Function
import Data.List (findIndex)
import qualified Data.Map as M
import System.IO
import System.Win32 (COORD (x))

data Monkey = Op String String String | Value Int deriving (Show)

parseLine line =
  let w = line & filter (/= ':') & words
   in if length w == 2 then (head w, Value (read (w !! 1))) else (head w, Op (w !! 1) (w !! 2) (w !! 3))

compute id m = case m M.! id of
  (Value x) -> x
  (Op a "+" b) -> compute a m + compute b m
  (Op a "-" b) -> compute a m - compute b m
  (Op a "*" b) -> compute a m * compute b m
  (Op a "/" b) -> compute a m `div` compute b m

part1 contents =
  let m = contents & lines & map parseLine & M.fromList
   in compute "root" m & show

depends id dep m
  | id == dep = True
  | otherwise = case m M.! id of
    (Value x) -> False
    (Op a _ b) -> depends a dep m || depends b dep m

solve id target m =
  case m M.! id of
    (Value x) -> target
    (Op a op b) ->
      let x = if depends a "humn" m then b else a
          y = if depends a "humn" m then a else b
          newTarget = case op of
            "+" -> target - compute x m
            "*" -> target `div` compute x m
            "-" -> if x == a then compute x m - target else target + compute x m
            "/" -> if x == a then compute x m `div` target else target * compute x m
       in solve y newTarget m

solve' id m =
  let (Op a _ b) = m M.! id
   in if depends a "humn" m then solve a (compute b m) m else solve b (compute a m) m

part2 contents =
  let m = contents & lines & map parseLine & M.fromList
   in solve' "root" m & show

main = do
  small_contents <- readFile "in.small"
  contents <- readFile "in"

  putStrLn $ part1 small_contents
  putStrLn $ part1 contents
  putStrLn $ part2 small_contents
  putStrLn $ part2 contents
