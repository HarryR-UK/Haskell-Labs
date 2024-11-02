-- PART A

char_to_int :: Char -> Integer
-- optimise later
char_to_int '0' = 0
char_to_int '1' = 1
char_to_int '2' = 2
char_to_int '3' = 3
char_to_int '4' = 4
char_to_int '5' = 5
char_to_int '6' = 6
char_to_int '7' = 7
char_to_int '8' = 8
char_to_int '9' = 9

repeat_char :: Char -> Integer -> [Char]
repeat_char c n
    | n == 0 = []
    | otherwise = c : repeat_char c (n-1)


decode :: [Char] -> [Char]
decode [] = []
decode (x:y:xs) = repeat_char x (char_to_int y) ++ (decode xs)

-- PART B
int_to_char :: Integer -> Char
-- also optimise this later
int_to_char 0 = '0'
int_to_char 1 = '1'
int_to_char 2 = '2'
int_to_char 3 = '3'
int_to_char 4 = '4'
int_to_char 5 = '5'
int_to_char 6 = '6'
int_to_char 7 = '7'
int_to_char 8 = '8'
int_to_char 9 = '9'


length_char :: Char -> [Char] -> Integer
length_char _ [] = 0
length_char c (x:xs)
     | c /= x = 0
     | otherwise = 1 + length_char x xs




    {-
This required a helper function which may not have been as effcient. 
Come back later to attempt to do this with one function
       -}

drop_char_help :: Integer -> [Char] -> [Char]
drop_char_help _ [] = []
drop_char_help n (x:xs)
  | n == 0 = x : drop_char_help n xs
  | otherwise = drop_char_help (n-1) xs

drop_char :: Char -> [Char] -> [Char]
drop_char _ [] = []
drop_char c string = drop_char_help (length_char c string) string


-- get the length of the number of characters, use drop char to remove the character, add the character and the length then move on to the next one
-- split the string up into the different letters, then add get the encode and add it to the rest of the string


encode :: [Char] -> [Char]
encode [] = []
encode [x] = [x,'1']
encode (x:xs) = x : len : enc
   where len = int_to_char(length_char x (x:xs))
         enc = encode(drop_char x xs)



-- PART C
--
-- split up each section of the string into the characters. get the length of the 

-- converts digit int to a list of ints
    {-
    This whole section will take an integer, then will return 
    a list of the integers. This will allow us to use the 
    int_to_char function on EACH element without needing an infinite set of possible
    integers which can be converted into chars
       -}
int_to_list_reverse :: [Integer] -> [Integer]
int_to_list_reverse [] = []
int_to_list_reverse (x:xs) = int_to_list_reverse(xs) ++ [x]

int_to_list_help :: Integer -> [Integer]
int_to_list_help 0 = []
int_to_list_help n = fromIntegral(n `mod` 10) : int_to_list_help(n `div` 10)

int_to_list :: Integer -> [Integer] 
int_to_list n
  | n == 0 = []
  | otherwise = int_to_list_reverse(int_to_list_help n)


get_list_of_chars :: [Char] -> [Integer]
get_list_of_chars [] = []
get_list_of_chars (x:xs) = int_to_list(length_char x (x:xs))

int_list_to_char_list :: [Integer] -> [Char]
int_list_to_char_list [] = []
int_list_to_char_list (x:xs) = int_to_char(x) : int_list_to_char_list xs

complex_encode :: [Char] -> [Char]
complex_encode [] = []
complex_encode [x] = [x]
complex_encode (x:xs) = x : len ++ complex_encode(drop_char x xs)
   where len = int_list_to_char_list(get_list_of_chars(x:xs))



-- complex decoding
-- thought process


-- theory could be: get the letter, get the number of integers after the letter
-- then, times the number of letters depending on the position of the 
-- element and the length of the list
-- for example, if the list of integers contains [2,1] (for the number 21) 
-- and the character is 'a'
-- then print 'a' * (2*10) and then 'a' * (1*1) depending on the position in denary

-- BUT how can we detect how many digits the number after the character goes to?
-- this is the only thing im stuck on with this problem. once I figure this out
-- i have build the rest of the functions so that the rest of the problem is solved




-- this passes the list of integers after the character and gets a list 
-- of integers (the reverse of what we used with the complex_encode
char_list_to_int_list :: [Char] -> [Integer]
char_list_to_int_list [] = []
char_list_to_int_list (x:xs) = char_to_int x : char_list_to_int_list xs

-- we can also use the repeat_char from earlier

-- simply takes the list spoken about before like [2,1] becoming the number 21
get_list_length :: [a] -> Integer
get_list_length [] = 0
get_list_length (x:xs) = 1 + get_list_length xs


-- by doing 10^length of the list, we then get the 100s 10s 1s etc (this was
-- using my a level computer science knowledge that binary simply uses 
-- base 2, in which each bit correlates to 2^0 then 2^1 then 2^2 etc) 
--
-- this number can then be used n conjunction with repeat_char to properly decode the character
int_list_to_final_number :: [Integer] -> Integer
int_list_to_final_number [] = 0
int_list_to_final_number (x:xs) = x * 10^((get_list_length(x:xs)-1)) + int_list_to_final_number xs

-- determining if the character is a digit
is_char_digit :: Integer -> Bool
is_char_digit n
  | n <= '9' && n >= '0' = True
  | otherwise = False

count_digits :: [Char] -> Integer
count_digits [] = []
count_digits (x:xs)
  | is_char_digit x = x : count_digits xs
  | otherwise = []

complex_decode :: [Char] -> [Char]
complex_decode [] = []
complex_decode (x:xs)
  | 

