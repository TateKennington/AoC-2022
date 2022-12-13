import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (isNumber, isSpace)
import Data.Function
import Data.List (elemIndex, find, sortBy)
import Data.Maybe (fromJust, fromMaybe, isJust)
import System.IO

data Item = Arr [Item] | Item Int deriving (Show, Eq)

parseArray :: [Char] -> ([Char], [Item], Bool)
parseArray line =
  let stripped_line = tail line
   in iterate
        ( \(stream, arr, done) ->
            let trimmed_stream = dropWhile (\c -> isSpace c || c == ',') stream
                c = head trimmed_stream
             in case c of
                  '[' -> (\(remaining, new_arr, _) -> (remaining, arr ++ [Arr new_arr], False)) (parseArray trimmed_stream)
                  ']' -> (drop 1 trimmed_stream, arr, True)
                  _ -> (trimmed_stream & dropWhile isNumber, arr ++ [Item (trimmed_stream & takeWhile isNumber & read)], False)
        )
        (stripped_line, [], False)
        & find (\(_, _, done) -> done)
        & fromJust

parsePair [a, b] =
  ((\(_, arr, _) -> arr) $ parseArray a, (\(_, arr, _) -> arr) $ parseArray b)

parseInput [] = []
parseInput input =
  parsePair (take 2 input) : parseInput (drop 3 input)

checkPair (Item left) (Arr right) = checkPair (Arr [Item left]) (Arr right)
checkPair (Arr left) (Item right) = checkPair (Arr left) (Arr [Item right])
checkPair (Item left) (Item right) = if left == right then Nothing else Just (left < right)
checkPair (Arr []) (Arr []) = Nothing
checkPair (Arr []) (Arr _) = Just True
checkPair (Arr _) (Arr []) = Just False
checkPair (Arr left) (Arr right) =
  let res = checkPair (head left) (head right)
   in if isJust res then res else checkPair (Arr $ tail left) (Arr $ tail right)

checkOrder (left, right) =
  checkPair (Arr left) (Arr right)
    & fromJust

part1 contents =
  lines contents
    & parseInput
    & zip [1 ..]
    & filter (checkOrder . snd)
    & map fst
    & sum
    & show

part2 contents =
  lines contents
    & parseInput
    & foldl (\acc (a, b) -> a : b : acc) [[Arr [Item 2]], [Arr [Item 6]]]
    & sortBy
      ( \a b -> case checkPair (Arr a) (Arr b) of
          Nothing -> EQ
          Just True -> LT
          Just False -> GT
      )
    & (\arr -> (1 + fromJust (elemIndex [Arr [Item 2]] arr)) * (1 + fromJust (elemIndex [Arr [Item 6]] arr)))
    & show

main = do
  small_contents <- readFile "in.small"
  contents <- readFile "in"

  putStrLn $ part1 small_contents
  putStrLn $ part1 contents
  putStrLn $ part2 small_contents
  putStrLn $ part2 contents
