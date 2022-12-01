import MyUtils
import GridUtils
import qualified Data.Map as Map
import Debug.Trace

type Cave = NeighbourMap Int --Cost of entering, total cost of getting there
type Frontier = [Point]
type Costs = Map.Map Point Int --Total costs of getting to places

parse :: [String] -> Cave
parse input = input |> map2 (:[]) |> map2 read |> gridFromList |> neighbourMapO 

expand :: Cave -> Costs -> Point -> Frontier -> Int
--expand cave costs goal frontier = if closest==goal then costs Map.! closest else trace (show frontier) (expand cave newCosts goal nextFrontier) where
expand cave costs goal frontier = if closest==goal then costs Map.! closest else expand cave newCosts goal nextFrontier where
    closest = minimumOn (costs Map.!) frontier
    (nextPoints, newCosts) = expandFromPoint cave costs closest
    nextFrontier = (filter (/=closest) frontier) ++ nextPoints


expandFromPoint :: Cave -> Costs -> Point -> ([Point], Costs)
expandFromPoint cave costs point = getNeighbours cave point |> map (mapSnd (+currentCost))       --map fst |> zipF (\n -> currentCost + (getValue cave n)) 
    |> filter (\(p,n)-> isCostHigherThan costs p n)  |> \res->(map fst res, updateCosts costs res)
    where currentCost = costs Map.! point

updateCosts :: Costs -> [(Point, Int)] -> Costs
updateCosts costs updates = foldr (uncurry Map.insert) costs updates

isCostHigherThan :: Costs -> Point -> Int -> Bool
isCostHigherThan costs point n = case Map.lookup point costs of
    Nothing -> True
    Just n2 -> n2 > n

part1 :: [String] -> Int
part1 input = expand (parse input) (Map.singleton (0,0) 0) goal [(0,0)] where
    goal = (length (input!!0), length input) |> mapBoth (subtract 1)

incBy :: Int -> Int -> Int
incBy x n = ((x+n-1) `mod` 9)+1

parse2 :: [String] -> Cave
parse2 input = [0..4] |> map (\n-> map2 (incBy n) base) |> concat |> map (\row-> map (\n2-> map (incBy n2) row) [0..4] |> concat) |> gridFromList |> neighbourMapO
    where base = input |> map2 (read . (:[])) :: [[Int]]

part2 :: [String] -> Int
part2 input = expand (parse2 input) (Map.singleton (0,0) 0) goal [(0,0)] where
    goal = (length (input!!0), length input) |> mapBoth (*5) |> mapBoth (subtract 1) 



test = ["1163751742", "1381373672", "2136511328", "3694931569", "7463417111", "1319128137", "1359912421", "3125421639", "1293138521", "2311944581"]
smolTest = ["4444","1444","1114","4411"]