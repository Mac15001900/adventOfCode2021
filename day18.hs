import MyUtils
import Data.Maybe

data Number = Value Int | Pair Number Number deriving (Show, Eq, Read)

parseLine :: String -> Number
parseLine [x] = Value (read [x])
parseLine xs = Pair (parseLine as) (parseLine bs) where (as,bs) = splitOnMiddleComma xs

splitOnMiddleComma :: String -> (String, String)
splitOnMiddleComma xs = (tail as, init bs) where (as,bs) = splitOnMiddleComma' 0 xs

splitOnMiddleComma' :: Int ->String ->  (String, String)
splitOnMiddleComma' depth ('[':xs) = splitOnMiddleComma' (depth+1) xs |> mapFst ('[':)
splitOnMiddleComma' depth (']':xs) = splitOnMiddleComma' (depth-1) xs |> mapFst (']':)
splitOnMiddleComma' 1     (',':xs) = ("", xs)
splitOnMiddleComma' depth (x:xs)   = splitOnMiddleComma' depth xs     |> mapFst (x:)

explode :: Int -> Number -> Maybe (Number,Int,Int) --Returs both the number and explosion values that still need to added on the left and right
explode 4 (Pair (Value x) (Value y)) = Just (Value 0, x, y)
explode _ (Value _) = Nothing
explode n (Pair xs ys) = case explode (n+1) xs of
    Just (newXs, left, right) -> Just (Pair newXs (addLeft right ys), left, 0)
    Nothing -> case explode (n+1) ys of
        Just (newYs, left, right) -> Just (Pair (addRight left xs) newYs, 0, right)
        Nothing -> Nothing

addLeft :: Int -> Number -> Number
addLeft n (Value x) = Value (x+n)
addLeft n (Pair xs ys) = Pair (addLeft n xs) ys

addRight :: Int -> Number -> Number
addRight n (Value x) = Value (x+n)
addRight n (Pair xs ys) = Pair xs (addRight n ys)

splitNumber :: Number -> Maybe Number
splitNumber (Value x) = if x>=10 then Just (Pair (Value (floor half)) (Value (ceiling half))) else Nothing where half = (fromIntegral x)/2
splitNumber (Pair xs ys) = case splitNumber xs of
    Just newXs -> Just (Pair newXs ys)
    Nothing -> case splitNumber ys of
        Just newYs -> Just (Pair xs newYs)
        Nothing -> Nothing

reduce :: Number -> Number
reduce xs = case explode 0 xs of
    Just (newXs, _, _) -> reduce newXs
    Nothing -> case splitNumber xs of
        Just newXs -> reduce newXs
        Nothing -> xs

addNumbers :: Number -> Number -> Number
addNumbers xs ys = Pair xs ys |> reduce

magnitude :: Number -> Int
magnitude (Value x) = x
magnitude (Pair xs ys) = 3*(magnitude xs) + 2*(magnitude ys)

part1 :: [String] -> Int
part1 input = input |> map parseLine |> foldl1 addNumbers |> magnitude

part2 :: [String] -> Int
part2 input = input |> map parseLine |> \xs -> combinations xs xs |> filter (\(xs,ys) -> xs/=ys) |> map (uncurry addNumbers) |> map magnitude |> maximum


test = ["[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]", "[[[5,[2,8]],4],[5,[[9,9],0]]]", "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]", "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]", "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]", "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]", "[[[[5,4],[7,7]],8],[[8,3],8]]", "[[9,3],[[9,9],[6,[4,9]]]]", "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]", "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"]