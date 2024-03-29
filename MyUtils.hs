module MyUtils (runOnFile,runOnFile2,runOnFile4,(|>),split,count,count2,freq,exists,separate,(!!?),unique,unique',combinations, combinations3, combinationsSelf,
      rotateMatrix,splitOn,joinWith,valueBetween, differences, tupleMap, repeatF, repeatUntil, indexes, zipWithIndexes, zip2, zipF, repeat2, repeat3,
      map2, map3, setElement, setElement2, setElement3, changeElement, changeElement2, changeElement3, empty2, empty3, flattenMaybe, removeNothing,
      t2toList, t3toList, t4toList, t5toList, t2fromList, t3fromList, t4fromList, t5fromList, 
      divF, mean, meanI, sign, pair, pairS, mapFst, mapSnd, mapBoth, fst3, snd3, thd3, maximumOn, minimumOn, set) where
import Control.Monad
import Data.List
import Data.Maybe
import System.IO

(|>) :: a -> (a->b) -> b
a |> f = f a

--Takes a file path and a function, runs that function on the file's contents, and prints the function's output. Trims the last line of the file if it's an empty line
runOnFile :: Show a => String -> ([String]->a) -> IO ()
runOnFile input start = do
   handle <- openFile input ReadMode
   contents <- hGetContents handle
   let lines = splitOn '\n' contents
   let linesTrimmed = if last lines == "" then init lines else lines
   print $ start linesTrimmed
   hClose handle      

runOnFile2 :: String -> ([String]->IO()) -> IO ()
runOnFile2 input start = do
   handle <- openFile input ReadMode
   contents <- hGetContents handle
   let lines = splitOn '\n' contents
   let linesTrimmed = if last lines == "" then init lines else lines
   start linesTrimmed
   hClose handle    

--Takes a file path and a function, runs that function on the file's contents, and prints the function's output. Maps 'read' over the file contents. Trims the last line of the file if it's an empty line
runOnFile3 :: Show a => Read b  => String -> ([b]->a) -> IO ()
runOnFile3 input start = do
   handle <- openFile input ReadMode
   contents <- hGetContents handle
   let lines = splitOn '\n' contents
   let linesTrimmed = if last lines == "" then init lines else lines
   let linesRead = map read linesTrimmed
   print $ start linesRead
   hClose handle
   
runOnFile4 :: ([String]->String) -> String -> IO ()
runOnFile4 start input = do
   handle <- openFile input ReadMode
   contents <- hGetContents handle
   let lines = split (=='\n') contents
   putStrLn $ start lines
   hClose handle
   
split     :: (a -> Bool) -> [a] -> [[a]]
split p s =  case dropWhile p s of
                      [] -> []
                      s' -> w : split p s''
                            where (w, s'') = break p s'

count :: (a->Bool) -> [a] -> Int
count p = length . (filter p)

count2 :: (a->Bool) -> [[a]] -> Int
count2 p as = map (count p) as |> sum

freq :: Eq a => [a] -> a -> Int
freq [] _     = 0
freq (x:xs) a = (if x==a then 1 else 0) + (freq xs a)

exists :: (a->Bool) -> [a] -> Bool
exists p xs = isJust (find p xs) 

--Separates a list into elements that do and don'this fit a predicate
separate :: (a->Bool) -> [a] -> ([a],[a])
separate p as = separate' p (reverse as) ([],[])

separate' :: (a->Bool) -> [a] -> ([a],[a]) -> ([a],[a])
separate' p [] acc = acc
separate' p (a:as) (ts,fs) = separate' p as (if p a then (a:ts,fs) else (ts,a:fs))

(!!?) :: [a] -> Int -> Maybe a
list !!? index = if index<0 || index>=length list then Nothing else Just (list!!index)


unique :: Eq a  => [a] -> [a]
unique xs = xs |> unique' [] |> reverse

unique' :: Eq a => [a] -> [a] -> [a]
unique' res []     = res
unique' res (x:xs) = if elem x res then unique' res xs else unique' (x:res) xs

rotateMatrix :: [[a]] -> [[a]]
rotateMatrix (x:xs) = foldr largerZip (map (\a->[a]) x) (reverse xs) |> map reverse

largerZip :: [a] -> [[a]] -> [[a]]
largerZip []     []       = []
largerZip (x:xs) (ys:yss) = (x:ys):(largerZip xs yss)

combinations :: [a] -> [b] -> [(a,b)]
combinations as bs = map (\a-> map (\b-> (a,b)) bs) as |> concat

combinations3 :: [a] -> [b] -> [c] -> [(a,b,c)]
combinations3 as bs cs = map (\a-> map (\b-> map (\c-> (a,b,c)) cs) bs) as |> concat |> concat

--Produces all combinations of two elements from an array, without symmetric of equal ones. [x,y,z] produces [(x,y),(x,z),(y,z)], but not (y,x) or (x,x)
combinationsSelf :: [a] -> [(a,a)]
combinationsSelf [] = []
combinationsSelf (x:xs) = (map (pair x) xs) ++ (combinationsSelf xs)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn a xs = splitOn' a xs []

splitOn' :: Eq a => a -> [a] -> [a]-> [[a]]
splitOn' a [] op     = [reverse op]
splitOn' a (x:xs) op = if a==x then (reverse op):(splitOn' a xs []) else splitOn' a xs (x:op)

joinWith :: [a] -> [[a]] -> [a]
joinWith a [] = []
joinWith a [x] = x
joinWith a (x:xs) = (x++a)++(joinWith a xs)

valueBetween :: Ord a => (a,a) -> a -> Bool
valueBetween (low,high) x = x >= low && x <= high

differences :: Num a => [a] -> [a]
differences [] = []
differences a = zip (tail a) (init a) |>  tupleMap (-)

tupleMap :: (a->b->c) -> [(a,b)] -> [c]
tupleMap f = map (\(a,b) -> f a b)

repeatF :: Int -> (a->a) -> a -> a
repeatF 0 _ x = x
repeatF n f x = repeatF (n-1) f (f x)

repeatUntil :: (a->Bool) -> (a->a) -> a -> a
repeatUntil p f a = if p a then a else repeatUntil p f (f a)

indexes :: [a] -> [Int]
indexes [] = []
indexes a = [0..(length a)-1]

zipWithIndexes :: [a] -> [(a,Int)]
zipWithIndexes a = zip a (indexes a)

zip2 :: [[a]] -> [[b]] -> [[(a,b)]]
zip2 ass bss = zip ass bss |> map (\(as,bs)-> zip as bs)

zipF :: (a->b) -> [a] -> [(a,b)]
zipF f as = zip as (map f as)

repeat2 :: a -> [[a]]
repeat2 a = repeat a |> repeat

repeat3 :: a -> [[[a]]]
repeat3 a = repeat a |> repeat |> repeat

map2 :: (a->b) -> [[a]] -> [[b]]
map2 f = map (map f)

map3 :: (a->b) -> [[[a]]] -> [[[b]]]
map3 f = map (map (map f))

empty2 :: Eq a => [[a]] -> Bool
empty2 xs = not $ exists (/=[]) xs

empty3 :: Eq a => [[[a]]] -> Bool
empty3 xss = (map (\xs->not $ exists (/=[]) xs) xss |> and)

setElement :: Int -> a -> [a] -> [a]
setElement i x xs = (take i xs)++[x]++(drop (i+1) xs)

setElement2 :: Int -> Int -> a -> [[a]] -> [[a]]
setElement2 i j x xs = (take j xs)++[setElement i x (xs!!j)]++(drop (j+1) xs)

setElement3 :: Int -> Int -> Int -> a -> [[[a]]] -> [[[a]]]
setElement3 i j k x xs = (take k xs)++[setElement2 i j x (xs!!k)]++(drop (k+1) xs)

changeElement :: Int -> (a->a) -> [a] -> [a]
changeElement i f xs = (take i xs)++[f (xs!!i)]++(drop (i+1) xs)

changeElement2 :: Int -> Int -> (a->a) -> [[a]] -> [[a]]
changeElement2 i j f xs = (take j xs)++[changeElement i f (xs!!j)]++(drop (j+1) xs)

changeElement3 :: Int -> Int -> Int -> (a->a) -> [[[a]]] -> [[[a]]]
changeElement3 i j k f xs = (take k xs)++[changeElement2 i j f (xs!!k)]++(drop (k+1) xs)

flattenMaybe :: Maybe (Maybe a) -> Maybe a
flattenMaybe Nothing = Nothing
flattenMaybe (Just a) = a

removeNothing :: [Maybe a] -> [a]
removeNothing [] = []
removeNothing (Nothing:xs) = removeNothing xs
removeNothing ((Just a):xs) = a:(removeNothing xs)

divF :: Int -> Int -> Float
divF x y = (fromIntegral x) / (fromIntegral y)

mean :: Fractional a => [a] -> a
mean as = (sum as) / (length as |> fromIntegral)

meanI ::  Integral a => [a] -> Float
meanI as = (sum as |> fromIntegral) / (length as |> fromIntegral)

sign :: Int -> Int
sign x
      | x > 0  = 1
      | x == 0 = 0
      | x < 0  = -1 

pair :: a -> b -> (a, b)
pair a b = (a,b)

pairS :: a -> b -> (b, a)
pairS a b = (b, a)

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a,b) = (f a, b)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a,b) = (a, f b)

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (x,y) = (f x, f y)

fst3 :: (a,b,c) -> a
fst3 (a,b,c) = a
snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b
thd3 :: (a,b,c) -> c
thd3 (a,b,c) = c

t2toList :: (a,a) -> [a]
t2toList (x,y) = [x,y]
t3toList :: (a,a,a) -> [a]
t3toList (x,y,z) = [x,y,z]
t4toList :: (a,a,a,a) -> [a]
t4toList (x1,x2,x3,x4) = [x1,x2,x3,x4]
t5toList :: (a,a,a,a,a) -> [a]
t5toList (x1,x2,x3,x4,x5) = [x1,x2,x3,x4,x5]

t2fromList :: [a] -> (a,a)
t2fromList [x,y] = (x,y)
t3fromList :: [a] -> (a,a,a)
t3fromList [x,y,z] = (x,y,z)
t4fromList :: [a] -> (a,a,a,a)
t4fromList [x1,x2,x3,x4] = (x1,x2,x3,x4)
t5fromList :: [a] -> (a,a,a,a,a)
t5fromList [x1,x2,x3,x4,x5] = (x1,x2,x3,x4,x5)

maximumOn :: Ord b => (a->b) -> [a] -> a
maximumOn f [] = error "MaximumOn can only be called on non-empty lists"
maximumOn f (a:as) = extremeOn (>) f as (a, (f a))

minimumOn :: Ord b => (a->b) -> [a] -> a
minimumOn f [] = error "MinimumOn can only be called on non-empty lists"
minimumOn f (a:as) = extremeOn (<) f as (a, (f a))

extremeOn :: Ord b => (b->b->Bool) -> (a->b) -> [a] -> (a,b) -> a
extremeOn op f []     (best, _)         = best
extremeOn op f (a:as) (best, bestValue) = extremeOn op f as (if f a `op` bestValue then (a, f a) else (best, bestValue))

set :: a -> (b->a)
set a = \_ -> a




