import MyUtils


part1 :: [Int] -> Int
part1 input = zip (init input) (tail input) |> count (uncurry (<))

part2 :: [Int] -> Int
part2 input = zip3 (init $ init input) (init $ tail input) (tail $ tail input) |> map (\(a,b,c)->a+b+c) |> part1
