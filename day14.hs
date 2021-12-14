import MyUtils
import Data.List
import qualified Data.Map as Map

type Rules = Map.Map (Char,Char) Char
type Count = Map.Map Char Int
type Known = Map.Map ((Char,Char), Int) Count

parseRules :: [String] -> Rules
parseRules input = map (\line->splitOn ' ' line |> \splat-> ((head $ head splat,last $ head splat), head (last splat))) input |> Map.fromList

emptyCount :: String -> Count
emptyCount input = input |> unique |> (flip zip) (repeat 0) |> Map.fromList

countFor :: Rules -> Int -> (Char, Char) -> Known -> (Count,Known)
countFor rules n (a,b) known = case Map.lookup ((a,b), n) known of
    Just counted -> (counted, known)
    Nothing -> if n == 0 then (Map.singleton a 1, Map.insert ((a,b), n) (Map.singleton a 1) known) else
        (Map.unionWith (+) count1 count2, known''') where
            middle = rules Map.! (a,b)
            (count1, known')  = countFor rules (n-1) (a, middle) known
            (count2, known'') = countFor rules (n-1) (middle, b) known'
            known''' = Map.unions [known'', Map.singleton ((a, middle), n-1) count1, Map.singleton ((middle, b), n-1) count2]

solve :: Int -> [String] -> Int
solve n input = counts |> ((Map.singleton (last polymer) 1):) |> foldr1 (Map.unionWith (+)) |> Map.toList |> map snd |>  sort |> \res -> (last res) - (head res) where
    [polymerInput, rulesInput] = splitOn "" input
    polymer = head polymerInput
    counts = zip (init polymer) (tail polymer) |> map (\pair -> countFor (parseRules rulesInput) n pair Map.empty) |> map fst

part1 :: [String] -> Int
part1 = solve 10

part2 :: [String] -> Int
part2 = solve 40

countFromString :: String -> Count
countFromString s = s |> map ((flip Map.singleton) 1) |> foldr1 (Map.unionWith (+))

test = ["NNCB", "", "CH -> B", "HH -> N", "CB -> H", "NH -> C", "HB -> C", "HC -> B", "HN -> C", "NN -> C", "BH -> H", "NC -> B", "NB -> B", "BN -> B", "BB -> N", "BC -> B", "CC -> N", "CN -> C"]

{- Old solution
type Polymer = String

expand :: Rules -> Polymer -> Polymer
expand rules p = zip (init p) (tail p) |> map (expandOne rules) |> concat |> (++[last p])

expandOne :: Rules -> (Char,Char) -> String
expandOne rules (a,b) = a:[rules Map.! (a,b)]

part1 :: [String] -> Int
part1 input = repeatF 10 (expand $ parseRules rulesInput) (head polymer) |> \longPolymer-> (unique longPolymer |> map (freq longPolymer)) |> sort |> \res -> (last res) - (head res)  where 
    [polymer, rulesInput] = splitOn "" input
    --rules = parseRules rulesInput

partTest :: Int -> [String] -> String
partTest n input = repeatF n (expand $ parseRules rulesInput) (head polymer) where
    [polymer, rulesInput] = splitOn "" input
-}