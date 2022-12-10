import Data.Function
import Data.Set (empty, insert)
import System.IO

getMove :: [[Char]] -> ((Int, Int), Int)
getMove ["U", count] = ((0, 1), read count)
getMove ["L", count] = ((-1, 0), read count)
getMove ["R", count] = ((1, 0), read count)
getMove ["D", count] = ((0, -1), read count)

add, sub :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (a, b) (c, d) = (a + c, b + d)
sub (a, b) (c, d) = (a - c, b - d)

dist (a, b) = a ^ 2 + b ^ 2

norm (a, b) =
  ( if abs a > 0 then a `div` abs a else a,
    if abs b > 0 then b `div` abs b else b
  )

group' :: [a] -> [(a, a)]
group' [] = []
group' [x] = []
group' (x : xs) = (x, head xs) : group' xs

getNewPosition prev curr =
  if dist (prev `sub` curr) >= 4
    then norm (prev `sub` curr) `add` curr
    else curr

part1 contents =
  lines contents
    & foldl processMove (empty, (0, 0), (0, 0))
    & (\(visited, _, _) -> visited)
    & length
    & show
  where
    processMove state move =
      let parts = words move
          (direction, count) = getMove parts
          newState =
            [1 .. count]
              & foldl
                ( \(visited, head, tail') _ ->
                    let newHead = head `add` direction
                        newTail =
                          getNewPosition
                            newHead
                            tail'
                     in (insert newTail visited, newHead, newTail)
                )
                state
       in newState

part2 contents =
  lines contents
    & foldl processMove (empty, (0, 0), replicate 9 (0, 0))
    & (\(visited, _, _) -> visited)
    & length
    & show
  where
    processMove state move =
      let parts = words move
          (direction, count) = getMove parts
          newState =
            [1 .. count]
              & foldl
                ( \(visited, head', tails') _ ->
                    let newHead = head' `add` direction
                        newTails =
                          foldl
                            (\acc curr -> getNewPosition (head acc) curr : acc)
                            [newHead]
                            tails'
                            & reverse
                            & drop 1
                     in (insert (last newTails) visited, newHead, newTails)
                )
                state
       in newState

main = do
  small_contents <- readFile "in.small"
  contents <- readFile "in"

  putStrLn $ part1 small_contents
  putStrLn $ part1 contents
  putStrLn $ part2 small_contents
  putStrLn $ part2 contents
