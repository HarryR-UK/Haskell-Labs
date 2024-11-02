-- basic recursion

mult13 n
    | n == 0 = 0
    | otherwise = 13 + mult13(n-1)


pow3 n
    | n == 0 = 1
    | otherwise = 3 * pow3(n-1)

odd_sum n
    | n == 0 = 0
    | n `mod` 2 == 0 = odd_sum (n-1)
    | otherwise = n + odd_sum (n-1)


lucas n
    | n == 0 = 2
    | n == 1 = 1
    | n > 1 = lucas(n-1) + lucas(n-2)


-- recursion on lists

half_sum [] = 0
half_sum (x:xs) = (x / 2) + half_sum xs


mult2 [] = []
mult2 (x:xs) = 2*x : mult2 xs

drop_evens [] = []
drop_evens (x:xs) = if x `mod` 2 /= 0 then x : drop else drop
    where drop = drop_evens(xs)

triple [] = []
triple (x:xs) = x : x : x : triple(xs)

mult_adjacent [] = []
mult_adjacent [x] = [x]
mult_adjacent(x:y:xs) = x * y : mult_adjacent(xs)

get_ele _ [] = -1
get_ele i (x:xs)
    | i == 0 = x
    | otherwise = get_ele (i-1) xs


drop_ele _ [] = []
drop_ele i (x:xs) 
    | i == 0 = drop
    | otherwise = x : drop
    where drop = drop_ele (i-1) xs


-- more complex recursion on lists
div_list [] [] = []
div_list _ [] = []
div_list [] _ = []
div_list (x:xs) (y:ys) = (x/y) : div_list xs ys

longer [] _ = False
longer _ [] = True
longer (x:xs) (y:ys) = longer xs ys

div3_and_not [] = ([], [])
div3_and_not (x:xs)
   | x `mod` 3 == 0 = (x:mod_three, not_mod_three)
   | otherwise = (mod_three, x:not_mod_three)
   where (mod_three, not_mod_three) = div3_and_not xs

vowels_and_consonants [] = ([], [])
vowels_and_consonants (x:xs)
    | x `elem` ['a','e','i','o','u'] = (x:vowels, consonants)
    | otherwise = (vowels, x:consonants)
    where (vowels, consonants) = vowels_and_consonants xs


fast_lucas_help 1 = [1,2]
fast_lucas_help n = x+y : (x:y:xs)
     where (x:y:xs) = fast_lucas_help (n-1)

fast_lucas n = head (fast_lucas_help n)

mult_by_pos_help [] = []
mult_by_pos_help ((x,y):xs) = x * y : mult_by_pos_help xs
 
mult_by_pos [] = []
mult_by_pos list = mult_by_pos_help (zip list [0..(length list) - 1])


-- The Collatz Problem

-- actual collatz algorithm, returning a list of all characters from n

collatz 1 = [1]
collatz n
    | n`mod`2 == 0 = n : n_even
    | otherwise = n : n_odd
    where n_even = collatz(n`div`2)
          n_odd = collatz(3 * n + 1)



collatz_data 0 = [(0,0)]
collatz_data n = (length(collatz n), n) : collatz_data (n-1)

collatz_lengths [] = []
collatz_lengths ((x,y):xs) = x : collatz_lengths xs

collatz_max_length [] _ _ = 0
collatz_max_length ((x,y):xs) n max
    | x == max = y
    | otherwise = collatz_max_length xs n max

longest_collatz 0 = 0
longest_collatz n = collatz_max_length(collatz_data n) n (maximum(collatz_lengths(collatz_data(n))))

-- second attempt is now more optimised!
