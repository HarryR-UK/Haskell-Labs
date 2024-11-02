
-- TUPLES
square_and_cube x = (x*x, x*x*x)

add_tuple (a, b) = a + b

first (a, b) = a

second(a, b) = b

swap (a, b) = (b, a)

two_to_three (a,b) c = (a, b, c)

-- LISTS
head_squared list = (head list)^2

third list = list !! 2

second_tail list = tail(tail list)

third_head list = head (tail (tail list))

first_plus_last list = if length list < 2 then 0 else (head list + last list)

prepend_two list a b = a : b : list


-- List functions
two_lengths list1 list2 = length list1 + length list2 

make_palindrome list = list ++ reverse list

sum_and_product list = (sum list, product list)

four_through_six list = take 3 (drop 3 list)

both_in list x y = if x `elem` list && y `elem` list then True else False



-- list ranges
-- 1) [101..200]
-- 2) [1000,1050]
-- 3) [20,19..1]
-- 4) [x | x <- [999..], x `mod` 3 == 0]


-- List comprehensions

-- (a) [2^x | x <- [1..10]]

only_odds list = [x | x<-list, x `mod` 2 /= 0]

between a b list = [x | x <- list, x > a && x < b]

number_of_es string = length ([x | x<-string, x == 'e']) 

proper_fizzbuzz = [if x `mod` 3 == 0 && x `mod` 5 == 0 then "fizzbuzz" else (if x`mod`3==0 then "fizz" else (if x`mod`5==0 then "buzz" else show x)) | x <- [1..]]
