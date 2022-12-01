import MyUtils
import Data.Maybe

type Rooms = [String]
type Spots = [Maybe Char]
type State = (Rooms, Spots, Int) --Which rooms have unmoved amphipods, state of hallway spots, total cost so far

canExit :: State -> Int -> Int -> Bool --Can the amphipod from room r go to hallway spot h?
canExit (rooms,spots,_) r h = 

spotsBetweenRoomAndSpot :: State -> Int -> Int -> Spots
spotsBetweenRoomAndSpot (_,spots,_) x' y' = [x..y-1] |> map (*2) |> map (+2) |> map (spots!!) where 
    (x,y) = if x'<y' then (x',y') else (y',x')

{-

occupied :: State -> Int -> Bool --Checks if a given x-coordinate of the hallway occupied
occupied (rooms, spots, _) n = if n `elem` [2,4,6,8] then False else 

xCoord :: Bool -> Int -> Int --The x value of a place, either room (True) or spot (False)
xCoord True n  = 2+n*2
xCoord False n = if n==6 then 10 else n + (if n>=2 then n-1 else 0)
-}



{-
#############
#...........#
###A#C#B#B###
  #D#D#A#C#
  #########
-}

--if n `elem` [2,4,6,8] then rooms!!((n `div` 2) - 1) /= [] else 