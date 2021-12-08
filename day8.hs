import MyUtils
import Data.List

part1 :: [String] -> Int
part1 input = input |> map (\line-> line |> splitOn '|' |> last |> splitOn ' ' |> tail |> map length |> count (\w->elem w [2,3,4,7])) |> sum

type Wires = [(Char,Char)] --Tuple ('a','b') means that signal from wire 'a' wrongly outputs to 'b'.

candidates = "abcedfg"
numbers = [("abcefg",'0'), ("cf",'1'), ("acdeg",'2'), ("acdfg",'3'), ("bcdf",'4'), ("abdfg",'5'), ("abdefg",'6'), ("acf",'7'), ("abcdefg",'8'), ("abcdfg",'9')]

--Figures out which wires go where
assign :: [String] -> Wires
assign input = [
    ('a', a), --Part of 7, but not 1 (logic later below)
    ('b', filterAmount input 6 |> head), --Only letter that occurs 6 times
    ('c', filterAmount input 8 |> dropWhile (==a) |> head), --Occurs 8 times, but isn't 'a'
    ('d', filterAmount input 7 |> filter ((flip elem) four) |> head), --Occurs 7 times and is part of 4
    ('e', filterAmount input 4 |> head), --Only letter that occurs 4 times
    ('f', filterAmount input 9 |> head), --Only letter that occurs 9 times
    ('g', filterAmount input 7 |> dropWhile ((flip elem) four) |> head) --Occurs 7 times and isn't part of 4
    ] where one   = filter ((==2) . length) input |> head
            four  = filter ((==4) . length) input |> head
            seven = filter ((==3) . length) input |> head
            a = seven |> dropWhile ((flip elem) one) |> head

--Finds all letters that occur a specific amount of times
filterAmount :: [String] -> Int -> [Char]
filterAmount input n = candidates |> filter (\l-> (count (elem l) input) == n)

--Translates a string to a digit using a wires dictionary
translate:: Wires -> String -> Char
translate wires input = map (translateChar wires) input |> sort |> \letters -> filter ((==letters) . fst) numbers |> head |> snd

translateChar :: Wires -> Char -> Char
translateChar wires c = filter (\(x,y)-> y==c) wires |> head |> fst

processLine :: String -> Int
processLine line = map (translate (assign segments)) output |> read  where
    segments = splitOn '|' line |> head |> splitOn ' ' |> init
    output   = splitOn '|' line |> last |> splitOn ' ' |> tail

part2 :: [String] -> Int
part2 input = map processLine input |> sum


test= ["be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe", "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc", "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg", "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb", "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea", "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb", "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe", "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef", "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb", "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"]