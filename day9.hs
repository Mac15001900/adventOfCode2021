import MyUtils
import Data.List

type Map = [[Int]]
type Pos = (Int,Int)

getValue :: Map -> Pos -> Int
getValue heights (x,y) = (heights!!y)!!x

valid :: Map -> Pos -> Bool
valid (h:hs) (x,y) = x>=0 && y>=0 && x<length h && y<=length hs

getNeighbours :: Map -> Pos -> [Pos]
getNeighbours heights (x,y) =  map (\(x2,y2)-> (x+x2, y+y2)) [(-1,0),(1,0),(0,-1),(0,1)] |> filter (valid heights)

isLowPoint :: Map -> Pos -> Bool
isLowPoint heights pos = (getNeighbours heights pos) |> map (getValue heights) |> minimum |> (>getValue heights pos)

part1 :: [String] -> Int
part1 input = combinations [0..length (heights!!0) - 1] [0..length heights - 1] |> filter (isLowPoint heights) |> map (getValue heights) |> map (+1) |> sum where heights = map2 (read . (:[])) input

expandArea :: Map -> [Pos] -> [Pos]
expandArea heights area = map (getNeighbours heights) area |> concat |> filter (not . (==9) . (getValue heights)) |> (++area) |> unique |> sort

expandFully :: Map -> [Pos] -> [Pos]
expandFully heights area = if newArea == area then area else expandFully heights newArea where newArea = expandArea heights area

part2 :: [String] -> Int
part2 input = combinations [0..length (heights!!0) - 1] [0..length heights - 1] |> filter (isLowPoint heights) |> map (:[]) |> map (expandFully heights) |> map length |> sort |> reverse |> take 3 |> foldr1 (*) where
    heights = map2 (read . (:[])) input




test = ["2199943210", "3987894921", "9856789892", "8767896789", "9899965678"]