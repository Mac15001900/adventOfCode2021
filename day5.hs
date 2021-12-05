import MyUtils
import Data.Char

type Point = (Int, Int)
type Line = [Point]
type Floor = [[Int]]

parseLine :: Bool -> String -> Line
parseLine diagonals input = input |> splitOn ' ' |> \parts -> [head parts, last parts] |> map parsePoint |> \points -> pointsToLine diagonals (head points) (last points)

parsePoint :: String -> Point
parsePoint s = s |> splitOn ',' |> map read |> \coords -> (head coords, last coords)

pointsToLine :: Bool -> Point -> Point -> Line
pointsToLine diagonals (x1,y1) (x2,y2) 
    | x1 == x2  = [(min y1 y2)..(max y1 y2)] |> map (\y-> (x1,y))
    | y1 == y2  = [(min x1 x2)..(max x1 x2)] |> map (\x-> (x,y1))
    | diagonals = [0..abs (x1-x2)] |> map (\i-> (x1 + i*(sign (x2-x1)), y1 + i*(sign (y2-y1))))
    | otherwise = []

emptyFloor :: Int -> Floor
emptyFloor n = repeat (repeat 0 |> take n) |> take n

addLine :: Line -> Floor -> Floor
addLine ln f = foldr addPoint f ln

addPoint :: Point -> Floor -> Floor
addPoint (x,y) f = changeElement2 x y (+1) f

part1 :: [String] -> Int
part1 input = input |> map (parseLine False) |> foldr addLine (emptyFloor 1000) |> map (count (>1)) |> sum

part2 :: [String] -> Int
part2 input = input |> map (parseLine True)  |> foldr addLine (emptyFloor 1000) |> map (count (>1)) |> sum



test= ["0,9 -> 5,9","8,0 -> 0,8","9,4 -> 3,4","2,2 -> 2,1","7,0 -> 7,4","6,4 -> 2,0","0,9 -> 2,9","3,4 -> 1,4","0,0 -> 8,8","5,5 -> 8,2"]