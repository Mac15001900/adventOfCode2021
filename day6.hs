import MyUtils

type Fish = [Int]

parse :: String -> [Int]
parse input = read ("["++input++"]")

addFish :: Int -> Fish -> Fish
addFish n = changeElement n (+1)

simulateDay :: Fish -> Fish
simulateDay (zeroes:rest) = (changeElement 6 (+zeroes) rest) ++ [zeroes]

solution :: Int -> [String] -> Int
solution days input = input |> head |> parse |> foldr addFish (take 9 (repeat 0)) |> repeatF days simulateDay |> sum

part1 :: [String] -> Int
part1 = solution 80

part2 :: [String] -> Int
part2 = solution 256

test = ["3,4,3,1,2"]