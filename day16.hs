import MyUtils

data Packet = Op Int Int [Packet] | Literal Int Int deriving (Read, Show, Eq) --First int is the operator type/value, second is version

hexToBits :: Char -> String
hexToBits '0' = "0000"
hexToBits '1' = "0001"
hexToBits '2' = "0010"
hexToBits '3' = "0011"
hexToBits '4' = "0100"
hexToBits '5' = "0101"
hexToBits '6' = "0110"
hexToBits '7' = "0111"
hexToBits '8' = "1000"
hexToBits '9' = "1001"
hexToBits 'A' = "1010"
hexToBits 'B' = "1011"
hexToBits 'C' = "1100"
hexToBits 'D' = "1101"
hexToBits 'E' = "1110"
hexToBits 'F' = "1111"

toInt :: String -> Int
toInt = toInt' . reverse

toInt' :: String -> Int
toInt' [] = 0
toInt' ('1':xs) = 2 * (toInt' xs) + 1
toInt' ('0':xs) = 2 * (toInt' xs)

parse :: String -> Packet
parse s = map hexToBits s |> concat |> parsePacket |> fst

parsePacket :: String -> (Packet, String)
parsePacket (v1:v2:v3:'1':'0':'0':rest) = (Literal value (toInt [v1,v2,v3]), remaining) where (value,remaining) = parseLiteral rest 0
parsePacket (v1:v2:v3:op1:op2:op3:'0':rest) = (Op (toInt [op1,op2,op3]) (toInt [v1,v2,v3]) (parseAllPackets (rest |> drop 15 |> take len)), drop (15+len) rest) where len = toInt (take 15 rest)
parsePacket (v1:v2:v3:op1:op2:op3:'1':rest) = parseNPackets (drop 11 rest) amount |> mapFst (\packets-> Op (toInt [op1,op2,op3]) (toInt [v1,v2,v3]) packets)  where amount = toInt (take 11 rest)
parsePacket other = error ("Invalid string: "++other)

parseLiteral :: String -> Int -> (Int, String)
parseLiteral ('0':bits) acc = (16*acc + toInt (take 4 bits), drop 4 bits)
parseLiteral ('1':bits) acc = parseLiteral (drop 4 bits) (16*acc + (toInt (take 4 bits)))

parseAllPackets :: String -> [Packet]
parseAllPackets [] = []
parseAllPackets s = packet:(parseAllPackets remaining) where (packet, remaining) = parsePacket s

parseNPackets :: String -> Int -> ([Packet], String)
parseNPackets s 0 = ([], s)
parseNPackets s n = parseNPackets rest (n-1) |> mapFst (packet:) where (packet, rest) = parsePacket s

sumVersions :: Packet -> Int
sumVersions (Literal _ v) = v
sumVersions (Op _ v ps) = map sumVersions ps |> sum |> (+v)

part1 :: [String] -> Int
part1 s = s |> head |> parse |> sumVersions

ops :: [[Int] -> Int]
ops = [sum, foldr1 (*), minimum, maximum, head, \[a,b]-> if a>b then 1 else 0, \[a,b]-> if a<b then 1 else 0, \[a,b]-> if a==b then 1 else 0]

evaluate :: Packet -> Int
evaluate (Literal n _) = n
evaluate (Op n _ ps) = (ops!!n) (map evaluate ps)

part2 :: [String] -> Int
part2 s = s |> head |> parse |> evaluate

