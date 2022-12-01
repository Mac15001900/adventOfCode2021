import MyUtils
import Data.List

type Target = (Double, Double, Double, Double) --minX, maxX, minY, maxY

parse :: [String] -> Target
parse input = input |> head |> split (\c->c=='=' || c==',' || c=='.') |> map read |> \pieces-> (pieces!!1, pieces!!2, pieces!!4, pieces!!5)

part1 :: [String] -> Int
part1 input = floor $ minY * (minY+1) / 2 where (_, _, minY, _) = parse input

intsBetween :: Double -> Double -> [Int]
intsBetween a b = [ceiling a .. floor b]

xsForStep :: Target -> Double -> [Int]
xsForStep (minX, maxX, _, _) step = (intsBetween (minX/step +(step-1)/2) (maxX/step +(step-1)/2) |> filter (>=floor step)) ++ --For velocities that don't stop
    ((intsBetween (sqrt (2*minX+1/4) - 1/2) (sqrt (2*maxX+1/4) - 1/2)) |> filter (<floor step)) --For velocities that do stop

ysForStep :: Target -> Double -> [Int]
ysForStep (_, _, minY, maxY) step = intsBetween (minY/step +(step-1)/2) (maxY/step +(step-1)/2)

part2 :: [String] -> Int
part2 input = [1..(-minY*2)] |> map (\step -> combinations (ysForStep target step) (xsForStep target step)) |> concat |> unique |> length where
    target = parse input
    (_, _, minY, _) = target

test = ["target area: x=20..30, y=-10..-5"]