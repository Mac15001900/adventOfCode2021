import MyUtils
import Data.List
import qualified Data.Map as Map
import Data.Char

type Cave = (String, Bool, [String]) -- A caves has its name, a boolean that's True if it's large, and a list of cave names it's connected to
type Caves = Map.Map String Cave -- Maps names to caves
type Path = ([String], Cave) -- A list of already visited small caves and the current cave

parse :: [String] -> Caves
parse input = names |> map (\name -> (name, (name, isUpper (head name), connectionsFrom name))) |> Map.fromList  where
    postSplit = input |> map (splitOn '-')
    names = postSplit |> concat |> unique
    connectionsFrom name = (postSplit |> filter ((==name) . head) |> map last) ++ ((postSplit |> filter ((==name) . last) |> map head))

get :: Caves -> String -> Cave
get caves name = caves Map.! name

expand :: Caves -> Path -> [Path]
expand caves (visited, (current,big,connected)) = connected |> filter (not . ((flip elem) visited)) |> map (get caves) |>  map (pair newVisited) where
    newVisited = if big then visited else current:visited

-- Takes the caves map, current paths and previously found paths that reach the end. Returns all possible paths that start from given paths.
explore :: Caves -> [Path] -> [Path] -> [Path]
explore _     []    finished = finished
explore caves paths finished = explore caves unfinishedPaths (finished++finishedPaths)
    where (finishedPaths, unfinishedPaths) = map (expand caves) paths |> concat |> separate ((=="end") . fst3 . snd)

part1 :: [String] -> Int
part1 input = explore parsed [([], get parsed "start")] [] |> length where parsed = parse input

type Path2 = ([String], Cave, Bool) -- A list of already visited small caves, the current cave, and whether a small cave was already visited twice

expand2 :: Caves -> Path2 -> [Path2]
expand2 caves (visited, (current, isBig, connected), twiced) = newCavePaths ++ oldCavePaths where
    (visitedCaves, unvisitedCaves) = connected |> filter (/="start") |> separate ((flip elem) visited) |> mapBoth (map (get caves))
    newCavePaths = map (\newCave -> (visited', newCave, twiced)) unvisitedCaves
    oldCavePaths = if twiced then [] else map (\newCave -> (visited', newCave, True )) visitedCaves
    visited' = if isBig then visited else current:visited

explore2 :: Caves -> [Path2] -> [Path2] -> [Path2]
explore2 _     []    finished = finished
explore2 caves paths finished = explore2 caves unfinishedPaths (finished++finishedPaths)
    where (finishedPaths, unfinishedPaths) = map (expand2 caves) paths |> concat |> separate ((=="end") . fst3 . snd3)

part2 :: [String] -> Int
part2 input = explore2 parsed [([], get parsed "start", False)] [] |> length where parsed = parse input

test = ["fs-end", "he-DX", "fs-he", "start-DX", "pj-DX", "end-zg", "zg-sl", "zg-pj", "pj-he", "RW-he", "fs-DX", "pj-RW", "zg-RW", "start-pj", "he-WI", "zg-he", "pj-fs", "start-RW"]