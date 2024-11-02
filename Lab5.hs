
-- Partial Application

plus_ten = (+) 10 

is_twenty = (==) 20

three_power = (**) 3

power_three = (**3)

take_four = take 4


-- Anonymous functions

-- a) (\x -> x-1)
-- b) (\x y -> x+y)
-- c) (\(x,y) -> (y,x))


-- Map

triple :: (Num a) => [a] -> [a]
triple list = map (*3) list

list_to_str :: (Show a) => [a] -> [String]
list_to_str list = map show list

second_char :: [[a]] -> [a]
second_char list = map (!!1) list

add_pairs :: (Num a) => [(a,a)] -> [a]
add_pairs list = map (\(x,y) -> x+y) list

triple_list_list :: (Num a) => [[a]] -> [[a]]
triple_list_list l = map (map (*3)) l

-- Filter
only_odds :: (Integral a) => [a] -> [a]
only_odds list = filter (\x -> x `mod` 2 /= 0) list

vowels :: [Char] -> [Char]
vowels list = filter (\x -> x `elem` "aeiou") list

between :: Int -> Int -> [Int] -> [Int]
between a b list = filter (\x -> x > a && x < b) list

ordered :: [(Int, Int)] -> [(Int, Int)]
ordered list = filter (\(x,y) -> x > y) list

singletons :: [[a]] -> [[a]]
singletons list = filter (\x -> length x == 1) list

only_odds_list :: [[Int]] -> [[Int]]
only_odds_list l = map(filter(\x -> x`mod`2/=0))l


-- Function Composition

-- a) head . head $ [[1]]
-- b) (+1) . (*2) $ 4
-- c) sum . tail . tail $ [1,2,3,4]
-- d) filter (>10) . map (*2) $ [1..10]

-- Function types
--
