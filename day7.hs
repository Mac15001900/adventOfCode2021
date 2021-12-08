import MyUtils

parse :: [String] -> [Int]
parse input = read ("["++(head input)++"]")

check ::  (Int->Int) -> [Int] -> Int -> Int
check costF crabs n = map (\c-> (costF . abs) (n-c)) crabs |> sum

solution :: (Int->Int) -> [String] -> Int
solution costF input = [0..2000] |> map (check costF (parse input)) |> minimum

part1 :: [String] -> Int
part1 = solution id

part2:: [String] -> Int
part2 = solution (\n->n*(n+1) `div` 2)

test = ["16,1,2,0,4,2,7,1,2,14"]