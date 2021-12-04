import MyUtils 

data Bit = Zero | One deriving (Show, Read, Eq)

part1 :: [String] -> Int
part1 input = [0..(length (input!!0)-1)] |> map (\i->map (!!i) bits) |> map mostCommon |> \res-> (binaryToInt res) * (binaryToInt (flipBinary res))  where
    bits = map parse input

parse :: String -> [Bit]
parse  []    = []
parse ('0':xs) = Zero:(parse xs)
parse ('1':xs) = One:(parse xs)
parse _ = error "Parsing failed"

binaryToInt :: [Bit] -> Int
binaryToInt xs = binaryToInt' (reverse xs)

binaryToInt' :: [Bit] -> Int
binaryToInt' [] = 0
binaryToInt' (Zero:xs) = 2*(binaryToInt' xs)
binaryToInt' (One:xs)  = 1 + 2*(binaryToInt' xs)

flipBinary :: [Bit] -> [Bit]
flipBinary = map flipBit

flipBit :: Bit -> Bit
flipBit Zero = One
flipBit One = Zero

mostCommon :: [Bit] -> Bit
mostCommon bits = if (fromIntegral ones) >= size `divF` 2 then One else Zero where
    size = length bits
    ones = count (==One) bits

trim :: Bool -> Int -> [[Bit]] -> [Bit]
trim _ _ [] = error "No values left" 
trim _ _ [x] = x
trim b i xs = trim b (i+1) (filter (\x-> ((x!!i)==common)==b) xs) where
    common = mostCommon $ map (!!i) xs 

part2 :: [String] -> Int
part2 input = [True, False] |> map (\b-> trim b 0 (map parse input)) |> map binaryToInt |> foldr1 (*)





test :: [String]
test = ["00100","11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","01010"]