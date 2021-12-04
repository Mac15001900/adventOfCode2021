import MyUtils
import Data.Function 
import Data.List

type Board = [[(Int, Bool)]]

parseBoard :: [String] -> Board
parseBoard input = input |> map (splitOn ' ') |> map (filter (/="")) |> map2 read |> map2 (\n->(n,False))

parse :: [String] -> ([Int],[Board])
parse input = (read ("["++(input!!0)++"]"), input |> splitOn "" |> tail |> map parseBoard)

mark :: Int -> Board -> Board
mark n = map2 (\(n1,b) -> if n1==n then (n1,True) else (n1,b))

checkH :: Board -> Bool
checkH = exists (and . (map snd))

check :: Board -> Bool
check board = (checkH board) || (checkH (transpose board))

score :: Board -> Int
score board = board |> concat |> filter (not . snd) |> map fst |> sum

part1 :: [String] -> Int
part1 input = play (parse input)

play :: ([Int],[Board]) -> Int
play (n:ns,bs) = case winner of
    Nothing -> play (ns, newBoards)
    Just b  -> n * (score b) 
    where newBoards = map (mark n) bs
          winner = find check newBoards

part2 :: [String] -> Int
part2 input = play2 (parse input)

play2 :: ([Int],[Board]) -> Int
play2 (n:ns,[b]) = if check marked then n * (score marked) else play (ns,[marked]) where marked = mark n b
play2 (n:ns,bs)  = play2 (ns, map (mark n) bs |> filter (not . check))
    

test = ["7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1", "", "22 13 17 11  0", " 8  2 23  4 24", "21  9 14 16  7", " 6 10  3 18  5", " 1 12 20 15 19", "", " 3 15  0  2 22", " 9 18 13 17  5", "19  8  7 25 23", "20 11 10 24  4", "14 21 16 12  6", "", "14 21 17 24  4", "10 16 15  9 19", "18  8 23 26 20", "22 11 13  6  5", " 2  0 12  3  7"]