import MyUtils
import qualified Data.Map as Map
import Data.List

--Player 1 position, player 2 position, p1 score, p2 score, is it the first player's turn, next dice rolls, amount of rolls so far
type State = (Int, Int, Int, Int, Bool, [Int], Int) 

d100 :: [Int]
d100 = repeat [1..100] |> concat

parse :: [String] -> State
parse input = map (splitOn ' ') input |> map last |> \[a,b]-> (read a, read b, 0, 0, True, d100, 0)

showState :: State -> State --For when we want to see the state, otherwise it would try to print an infinite list
showState (x, y, sx, sy, t, dice, r) = (x, y, sx, sy, t,take 10 dice, r)

playStep :: State -> State
playStep (x, y, sx, sy, True , (d1:d2:d3:dice), r) = (newX, y   , sx+newX, sy     , False, dice, r+3) where newX = ((x+d1+d2+d3-1) `mod` 10) + 1
playStep (x, y, sx, sy, False, (d1:d2:d3:dice), r) = (x   , newY, sx     , sy+newY, True , dice, r+3) where newY = ((y+d1+d2+d3-1) `mod` 10) + 1

hasVictor :: State -> Bool
hasVictor (_,_,x,y,_,_,_) = x>=1000 || y>=1000

answer :: State -> Int
answer (_,_,x,y,_,_,r) = (min x y) * r

part1 :: [String] -> Int
part1 input = parse input |> repeatUntil hasVictor playStep |> answer

type State2 = (Int, Int, Int, Int, Bool) --Player 1 position, player 2 position, p1 score, p2 score, is it the first player's turn
type Victories = (Int, Int) -- How many universes did players 1 and 2 win in
type Known = Map.Map State2 Victories

parse2 :: [String] -> State2
parse2 input = map (splitOn ' ') input |> map last |> \[a,b]-> (read a, read b, 0, 0, True)

dd :: [(Int,Int)] --Dirac's dice!
dd = unique dice |> zipF (freq dice) where dice = combinations [1,2,3] [1,2,3] |> combinations [1,2,3] |> map (\(a,(b,c))-> a+b+c)
--dd = [(3,1),(4,3),(5,6),(6,7),(7,6),(8,3),(9,1)], though it's nice not hard-coding it. Represents each possible result, with the amount of times it happens

play :: State2 -> Known -> (Victories,Known)
play s known = case Map.lookup s known of
    Just v  -> (v, known)
    Nothing -> case checkWinners s known of
        Just res -> res
        Nothing  -> playAll (0,0) (createStates s) known |> \(v,k)-> (v, Map.insert s v k)

checkWinners :: State2 -> Known -> Maybe (Victories,Known)
checkWinners (x, y, sx, sy, t) known = if sx>=21 then Just ((1,0), Map.insert (x, y, sx, sy, t) (1,0) known) else
    if sy>=21 then Just ((0,1), Map.insert (x, y, sx, sy, t) (0,1) known) else Nothing

createStates :: State2 -> [(State2, Int)]
createStates (x, y, sx, sy, True)  = dd |> map (\(v, a)-> ((incPos x v, y, sx+(incPos x v), sy, False), a))
createStates (x, y, sx, sy, False) = dd |> map (\(v, a)-> ((x, incPos y v, sx, sy+(incPos y v), True ), a))

incPos :: Int -> Int -> Int
incPos a b  = ((a+b-1) `mod` 10) + 1

playAll :: Victories -> [(State2, Int)] -> Known -> (Victories,Known)
playAll vs [] known = (vs, known)
playAll vs ((state, mult):states) known = play state known |> \(v,k)-> playAll (addVs (mapBoth (*mult) v) vs) states k

addVs :: Victories -> Victories -> Victories
addVs (x1,y1) (x2,y2) = (x1+x2, y1+y2)

part2 :: [String] -> Int
part2 input = play (parse2 input) Map.empty |> fst |> uncurry max

test  = ["Player 1 starting position: 4", "Player 2 starting position: 8"]
input = ["Player 1 starting position: 8", "Player 2 starting position: 3"]