import MyUtils

data Direction = Down | Up | Forward deriving (Show, Eq, Read)
type Instruction = (Direction, Int)
type Position = (Int, Int) --X, Y
type Position2 = (Int, Int, Int) --X, Y, Aim

part1 :: [String] -> Int
part1 input = map parse input |> foldr move (0,0) |> \(x,y)-> x*y

parse :: String -> Instruction
parse s = splitOn ' ' s |> \(a)-> parse' (head a, read (a!!1))

parse' :: (String, Int) -> Instruction
parse' ("down", v) = (Down, v)
parse' ("up", v) = (Up, v)
parse' ("forward", v) = (Forward, v)
parse' (s,v) = error s

move :: Instruction -> Position -> Position
move (Up, v) (x,y) = (x,y-v)
move (Down, v) (x,y) = (x,y+v)
move (Forward, v) (x,y) = (x+v,y)

part2 :: [String] -> Int
part2 input = map parse input |> reverse |> foldr move2 (0,0,0) |> \(x,y,a)-> x*y

move2 :: Instruction -> Position2 -> Position2
move2 (Up, v)      (x,y,a) = (x,   y,     a-v)
move2 (Down, v)    (x,y,a) = (x,   y,     a+v)
move2 (Forward, v) (x,y,a) = (x+v, y+v*a, a)

test :: [String]
test = ["forward 5","down 5","forward 8","up 3","down 8","forward 2"]