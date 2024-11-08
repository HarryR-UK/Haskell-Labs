-- Folds and Scan
list_product :: (Num a) => [a] -> a
list_product list = foldr (*) 1 list


list_any :: [Bool] -> Bool
list_any list = foldr (||) False list

product_of_evens :: (Integral a) => [a] -> a
product_of_evens list = foldr (\x acc ->if even x then acc * x else acc) 1 list

lt10 list = foldr (\x acc -> if x < 10 then acc + 1 else acc) 0 list

smalls list = foldr (\x acc -> if x `elem` ['a'..'z'] then x : acc else acc) "" list


sum_evens_odds list = foldr (\x (a,b) -> if even x then (a+x, b) else (a,b+x)) (0,0) list


-- When using the Scanr function, unlike the foldr functions, this will return each step that the accumulator 
-- goes through, starting from right to left
--

leading_caps list = takeWhile (\x -> not(x `elem` ['a'..'z'])) list

drop_caps list = dropWhile (\x -> x `elem` ['A'..'Z']) list


split_on c string = (before, after)
  where before = takeWhile (\x -> x /= c) string
        after = dropWhile (\x -> x == c) (dropWhile (\x -> x /= c) string)


from_csv [] = []
from_csv string = fst(split) : from_csv (snd split)
  where split = split_on ',' string


-- zipWith
mul_lists list1 list2= zipWith (*) list1 list2

and_lists list1 list2 = zipWith (&&) list1 list2

keep_or_zero l1 l2 = zipWith(\x y -> if y == False then 0 else x) l1 l2


-- reverse the string, compare each letter of the string with zipWith, and if original string == new string then palindrome

is_palindrome_rev string = reverse string

is_palindrome string = and newList
  where rev = is_palindrome_rev string
        newList = zipWith(\x y -> if x == y then True else False) string rev
