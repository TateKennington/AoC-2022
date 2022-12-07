import Data.Bifunctor
import Data.Char (isNumber)
import Data.Function
import Data.List
import Data.Map.Strict (alter, empty, insert, keys, singleton, toList, update, (!))
import System.IO ()

adjustDir item Nothing = Just $ singleton (snd item) (fst item)
adjustDir item (Just map) = Just $ Data.Map.Strict.insert (snd item) (fst item) map

calculateSize map path =
  foldl
    ( \total (name, size) ->
        if size == 0
          then total + calculateSize map (path ++ name ++ "/")
          else total + size
    )
    0
    (toList (map ! path))

part1 contents =
  contents
    & lines
    & foldl
      ( \(path, map) line ->
          if "$ ls" `isPrefixOf` line
            then (path, map)
            else
              if "$ cd" `isPrefixOf` line
                then
                  let dir = line & drop 5
                   in case dir of
                        "/" -> ("/", map)
                        ".." -> (path & reverse & drop 1 & dropWhile (/= '/') & reverse, map)
                        _ -> (path ++ dir ++ "/", map)
                else
                  if "dir" `isPrefixOf` line
                    then
                      let dir = drop 4 line
                       in (path, map & alter (adjustDir (0, dir)) path)
                    else
                      let file :: (Int, [Char])
                          parts = line & span isNumber
                          file = bimap read (drop 1) parts
                          newMap = map & alter (adjustDir file) path
                       in (path, newMap)
      )
      ("", empty)
    & (\(_, files) -> files & keys & map (calculateSize files))
    & filter (<= 100000)
    & sum
    & show

part2 contents =
  contents
    & lines
    & foldl
      ( \(path, map) line ->
          if "$ ls" `isPrefixOf` line
            then (path, map)
            else
              if "$ cd" `isPrefixOf` line
                then
                  let dir = line & drop 5
                   in case dir of
                        "/" -> ("/", map)
                        ".." -> (path & reverse & drop 1 & dropWhile (/= '/') & reverse, map)
                        _ -> (path ++ dir ++ "/", map)
                else
                  if "dir" `isPrefixOf` line
                    then
                      let dir = drop 4 line
                       in (path, map & alter (adjustDir (0, dir)) path)
                    else
                      let file :: (Int, [Char])
                          parts = line & span isNumber
                          file = bimap read (drop 1) parts
                          newMap = map & alter (adjustDir file) path
                       in (path, newMap)
      )
      ("", empty)
    & ( \(_, files) ->
          let target = 30000000 - (70000000 - calculateSize files "/")
              sizes = files & keys & map (calculateSize files)
           in sizes & filter (>= target)
      )
    & minimum
    & show

main = do
  small_contents <- readFile "in.small"
  contents <- readFile "in"

  putStrLn $ part1 small_contents
  putStrLn $ part1 contents
  putStrLn $ part2 small_contents
  putStrLn $ part2 contents
