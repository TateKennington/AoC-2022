import Data.Char (isNumber)
import Data.Function
import Data.List (find, sortBy)
import System.IO

data Monkey = Monkey Int [Int] (Int -> Int) (Int, Int, Int)

replaceAt index item list = take index list ++ (item : drop (index + 1) list)

throw (Monkey score items operation behaviour) newItems =
  Monkey score (items ++ newItems) operation behaviour

processOperation ["old", "*", "old"] old = old * old
processOperation ["old", "*", x] old = old * read x
processOperation ["old", "+", x] old = old + read x

getMonkeys :: [String] -> (Int -> Int) -> [Monkey]
getMonkeys [] _ = []
getMonkeys input worrier =
  let startingItemlines = input & tail
      startingItems = startingItemlines & head & words & drop 2 & map (read . filter isNumber)
      operationLines = startingItemlines & tail
      operationParts = operationLines & head & words & drop 3
      operation = worrier . processOperation operationParts
      testLines = operationLines & tail
      [test, true, false] = testLines & takeWhile (not . null) & map (\line -> line & words & last & read)
   in Monkey 0 startingItems operation (test, true, false) : getMonkeys (input & dropWhile (not . null) & drop 1) worrier

processRound monkeys _ =
  [0 .. length monkeys - 1]
    & foldl
      ( \acc index ->
          let (Monkey score items operation (test, true, false)) = acc !! index
              m = monkeys & map (\(Monkey _ _ _ (test, _, _)) -> test) & product
              newScore = score + length items
              trueItems = items & map ((`mod` m) . operation) & filter (\item -> item `mod` test == 0)
              falseItems = items & map ((`mod` m) . operation) & filter (\item -> item `mod` test /= 0)
              trueMonkey = throw (acc !! true) trueItems
              falseMonkey = throw (acc !! false) falseItems
              newMonkey = Monkey newScore [] operation (test, true, false)
           in acc & replaceAt index newMonkey & replaceAt true trueMonkey & replaceAt false falseMonkey
      )
      monkeys

part1 contents =
  let monkeys = foldl processRound (getMonkeys (lines contents) (`div` 3)) [1 .. 20]
   in monkeys & map (\(Monkey score items _ _) -> score) & sortBy (flip compare) & take 2 & product & show

part2 contents =
  let monkeys = foldl processRound (getMonkeys (lines contents) id) [1 .. 10000]
   in monkeys & map (\(Monkey score items _ _) -> score) & sortBy (flip compare) & take 2 & product & show

main = do
  small_contents <- readFile "in.small"
  contents <- readFile "in"

  putStrLn $ part1 small_contents
  putStrLn $ part1 contents
  putStrLn $ part2 small_contents
  putStrLn $ part2 contents
