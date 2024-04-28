import Data.Bits
import Data.Char (isNumber, isSpace, isUpper)
import Data.Function
import Data.List (findIndex)
import Data.Map (empty, fromList, insert, keys, mapKeys, (!))
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import System.IO

parseLine :: [Char] -> (String, Int, [String])
parseLine line =
  line
    & drop 1
    & filter (\c -> isUpper c || isNumber c || isSpace c)
    & words
    & (\arr -> (head arr, read (arr !! 1), drop 2 arr))

parseInput lines =
  let raw =
        lines
          & map parseLine
          & foldl (\acc (id, rate, adj) -> M.insert id (rate, adj) acc) empty
      processed = raw & mapKeys (flip M.findIndex raw) & M.map (\(rate, adj) -> (rate, map (flip M.findIndex raw) adj))
   in (processed, M.findIndex "AA" raw)

part1 contents =
  let (valves, start) = parseInput $ lines contents
      f :: Int -> Int -> Int -> Int
      f =
        ( \id time open ->
            if time < 0
              then -40000000
              else
                if time == 0
                  then 0
                  else
                    let (rate, adj) = valves ! id
                        best =
                          if rate > 0 && (open .&. (1 `shift` id)) == 0
                            then
                              adj
                                & concatMap (\adjId -> [fm adjId (time -1) open, (time -1) * rate + fm adjId (time -2) (open .|. (1 `shift` id))])
                                & maximum
                            else
                              adj
                                & concatMap (\adjId -> [fm adjId (time -1) open])
                                & maximum
                     in best
        )
      m = map (\x -> map (\y -> map (f x y) [0 .. (1 `shift` 60)]) [0 .. 30]) [0 .. (M.size valves -1)]
      fm x y z =
        if y < 0
          then -40000000
          else m !! x !! y !! z
   in fm start 30 0 & show

-- replaceAt (Just index) item queue = take index queue ++ (fst item, max (snd item) (snd (queue !! index))) : drop (index + 1) queue

-- insertQueue :: [((Int, Int), Int)] -> ((Int, Int), Int) -> [((Int, Int), Int)]
-- insertQueue [] item = [item]
-- insertQueue queue item =
--   if isJust (findIndex ((== fst item) . fst) queue)
--     then replaceAt (findIndex ((== fst item) . fst) queue) item queue
--     else takeWhile ((<= snd item) . snd) queue ++ item : dropWhile ((<= snd item) . snd) queue

-- process (queue, valves) =
--   let ((id, time), dist) = head queue
--    in if id == -1
--         then dist
--         else
--           if time > 30
--             then if time == 30 then process (insertQueue queue ((-1, 31), dist), valves) else process (queue, valves)
--             else
--               let (rate, adj) = valves ! id
--                   newQueue =
--                     if rate > 0
--                       then foldl (\acc adjId -> insertQueue (insertQueue queue ((adjId, time + 1), dist)) ((adjId, time + 2), rate * (28 - time) + dist)) queue adj
--                       else foldl (\acc adjId -> insertQueue queue ((adjId, time + 1), dist)) queue adj
--                in process (newQueue, valves)

-- part1 contents =
--   let (valves, start) = parseInput $ lines contents
--    in process ([((start, 0), 0)], valves) & show

part2 contents =
  ""

main = do
  small_contents <- readFile "in.small"
  contents <- readFile "in"

  putStrLn $ part1 small_contents
  putStrLn $ part1 contents
  putStrLn $ part2 small_contents
  putStrLn $ part2 contents
