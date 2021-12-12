import MyUtils
import Data.List
import Data.Tuple

type Octopus = (Int,Bool) -- Power level, and whether it flashed already
type Octopuses = [[Octopus]] 
type State = (Octopuses, Int) --Octopuses and either: total amount of flashes so far (part 1) or number of steps (part 2)
type Pos = (Int, Int)

parse :: [String] -> State
parse s = s |> map2 (read . (:[])) |> map2 (pairS False) |> pairS 0 

allPositions :: Octopuses -> [Pos]
allPositions octo = combinations [0..length (octo!!0) - 1] [0..length octo - 1]

valid :: Octopuses -> Pos -> Bool
valid (o:os) (x,y) = x>=0 && y>=0 && x<length o && y<=length os

getNeighbours :: Octopuses -> Pos -> [Pos]
getNeighbours octo (x,y) = directions2D |> map (\(dx,dy) -> (x+dx, y+dy)) |> filter (valid octo)

get :: Octopuses -> Pos -> Octopus
get octo (x,y) = (octo!!y)!!x

--Creates a flash at pos, incrementing everything around it. Does *not* affect the octopus directly at pos.
flash :: Pos -> Octopuses -> Octopuses
flash pos octo = getNeighbours octo pos |> foldr increment octo

increment :: Pos -> Octopuses -> Octopuses
increment pos octo = if get octo pos |> snd then octo else
    if (get octo pos |> fst) >= 9 then flash pos (changeElement2 (fst pos) (snd pos) (\(n,b)->(n+1,True)) octo) else
        changeElement2 (fst pos) (snd pos) (\(n,b)->(n+1,b)) octo

coolDown :: Octopus -> Octopus
coolDown (n,b) = if b then (0,False) else (n,False)

step :: State -> State
step (octo,n) = (map2 coolDown newOcto, n + (count2 snd newOcto)) where newOcto = allPositions octo |> foldr increment octo

part1 :: [String] -> Int
part1 input = parse input |> repeatF 100 step |> snd

step2 :: State -> State
step2 (octo,n) = (allPositions octo |> foldr increment octo |> map2 coolDown, n + 1)

allZeroes :: State -> Bool
allZeroes (octo,_) = octo |> map2 fst |> concat |> filter (/=0) |> length |> (==0)

part2 :: [String] -> Int
part2 input = parse input |> repeatUntil allZeroes step2 |> snd

showOcto :: Octopuses -> IO()
showOcto octo = map2 fst octo |> map2 (\n->mod n 10) |> map2 show |> map (foldr1 (++)) |> joinWith ("\n") |> putStrLn


test = ["5483143223", "2745854711", "5264556173", "6141336146", "6357385478", "4167524645", "2176841721", "6882881134", "4846848554", "5283751526"]
input = ["6111821767", "1763611615", "3512683131", "8582771473", "8214813874", "2325823217", "2222482823", "5471356782", "3738671287", "8675226574"]