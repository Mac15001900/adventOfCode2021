import MyUtils

parse :: [String] -> [Int]
parse input = read ("["++(head input)++"]")

check ::  (Int->Int) -> [Int] -> Int -> Int
check costF crabs n = map (\c-> abs (n-c) |> costF) crabs |> sum

solution :: (Int->Int) -> [String] -> Int
solution costF input = [0..2000] |> map (check costF (parse input)) |> zip [0..2000] |> foldr1 (\(i1,v1) (i2,v2)-> if v1>v2 then (i2,v2) else (i1,v1)) |> snd
--part1 input = sum crabs |> (flip divF) (length crabs) |> round where crabs = parse input

part1 :: [String] -> Int
part1 = solution id

part2:: [String] -> Int
part2 = solution (\n->n*(n+1) `div` 2)

test = ["16,1,2,0,4,2,7,1,2,14"]