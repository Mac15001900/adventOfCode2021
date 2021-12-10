import MyUtils
import Data.List
import qualified Data.Map as Map

opening = "([{<"
closing = ")]}>"
scoreTable = Map.fromList [('(',3), ('[',57), ('{',1197), ('<',25137)]

score :: Char -> Int
score bracket = scoreTable Map.! (toOpening bracket)

toOpening :: Char -> Char
toOpening ')' = '('
toOpening ']' = '['
toOpening '}' = '{'
toOpening '>' = '<'
toOpening  b  =  b

processLine :: Bool -> String -> Int
processLine isPart1 s = processLine' isPart1 s "" 

processLine' :: Bool -> String -> String -> Int
processLine' p []     bs     = if p then 0 else score2 0 bs
processLine' p (x:xs) []     = if elem x opening then processLine' p xs [x] else error "too many closing brackets"
processLine' p (x:xs) (b:bs) = if elem x opening then processLine' p xs (x:b:bs) else 
        if toOpening x == b then processLine' p xs bs else if p then score x else 0

part1 :: [String] -> Int
part1 input = map (processLine True) input |> sum


scoreTable2 = Map.fromList [('(',1), ('[',2), ('{',3), ('<',4)]

score2 :: Int -> String -> Int
score2 n [] = n
score2 n (x:xs) = score2 (5*n + scoreTable2 Map.! x) xs

part2 :: [String] -> Int
part2 input = map (processLine False) input |> filter (>0) |> sort |> \scores -> scores !! ((length scores) `div` 2)

test = ["[({(<(())[]>[[{[]{<()<>>", "[(()[<>])]({[<{<<[]>>(", "{([(<{}[<>[]}>{[]{[(<()>", "(((({<>}<{<{<>}{[]{[]{}", "[[<[([]))<([[{}[[()]]]", "[{[{({}]{}}([{[{{{}}([]", "{<[[]]>}<{[{[{[]{()[[[]", "[<(<(<(<{}))><([]([]()", "<{([([[(<>()){}]>(<<{{", "<{([{{}}[<[[[<>{}]]]>[]]"]