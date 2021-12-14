import MyUtils

type Paper = [[Bool]]
data Instruction = Vertical Int | Horizontal Int deriving (Show, Read, Eq)

parsePaper :: Int -> Int -> [String] -> Paper
parsePaper width height input =  foldr addDot (repeat False |> take width |> repeat |> take height) tuples where
    tuples = map (\line -> read ("("++line++")")) input :: [(Int, Int)]

addDot :: (Int, Int) -> Paper -> Paper
addDot (x,y) = setElement2 x y True

parseInstructions :: [String] -> [Instruction]
parseInstructions [] = []
parseInstructions (i:is) = (if i!!11 == 'x' then Horizontal n else Vertical n):(parseInstructions is) where 
    n = i |> splitOn '=' |> last |> read :: Int

parse :: [String] -> (Paper, [Instruction])
parse input = (splitOn "" input |> head |> parsePaper width height, instructions) where
    instructions = splitOn "" input |> last |> parseInstructions
    (height,width) = separate isVertical instructions |> mapBoth (\inst-> 1 + 2 * (inst |> head |> value)) 

isVertical :: Instruction -> Bool
isVertical (Vertical   _) = True
isVertical (Horizontal _) = False

value :: Instruction -> Int
value (Vertical   n) = n
value (Horizontal n) = n

merge :: Paper -> Paper -> Paper
merge x y = zip2 x y |> map2 (uncurry (||))

foldPaper :: Instruction -> Paper -> Paper
foldPaper (Vertical   n) p = merge (take n p) (drop (n+1) p |> reverse)
foldPaper (Horizontal n) p = merge (map (take n) p) (map (drop (n+1)) p |> map reverse)

part1 :: [String] -> Int
part1 input = foldPaper (head instructions) paper |> count2 id where
    (paper, instructions) = parse input

showPaper :: Paper -> IO()
showPaper paper = map2 (\x-> if x then '#' else '.') paper |> joinWith "\n" |> putStrLn

part2 :: [String] -> IO() --Must be used with runOnFile2
part2 input = foldr foldPaper paper (reverse instructions) |> showPaper where
    (paper, instructions) = parse input

test = ["6,10", "0,14", "9,10", "0,3", "10,4", "4,11", "6,0", "6,12", "4,1", "0,13", "10,12", "3,4", "3,0", "8,4", "1,10", "2,14", "8,10", "9,0", "", "fold along y=7", "fold along x=5"]