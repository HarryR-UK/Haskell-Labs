import System.Environment


echo :: IO ()
echo = 
    do
        str <- getLine
        putStrLn str


double_echo :: IO ()
double_echo =
    do
        str <- getLine
        putStrLn str
        putStrLn str


put_two_strs :: String -> String -> IO ()
put_two_strs x y = 
    do
        putStrLn x
        putStrLn y


times_two :: IO () 
times_two = do
    n <- getLine
    let
      n2 = 2 * (read n :: Int)
    putStrLn . show $ n2


add :: IO () 
add = do
    x <- getLine
    y <- getLine
    let n = (read x :: Int) + (read y :: Int)
    putStrLn . show $ n


io_reverse :: IO ()
io_reverse = do
    n <- getLine 
    let
      x = reverse n
    putStrLn  x


guess_42 :: IO ()
guess_42 = do
    input <- getLine
    let n = read input :: Int
    if n == 42
    then putStrLn "correct"
    else putStrLn "wrong"

-- Returning Values
get_bool :: IO Bool
get_bool = do
    x <- getLine
    let n = read x :: Bool
    return n



get_two_and_add :: IO Int
get_two_and_add = do
    num1 <- getLine
    num2 <- getLine
    let add = (read num1 :: Int) + (read num2 :: Int)
    return add

gt10 :: IO Bool
gt10 = do
    x <- getLine
    let n = (read x :: Int)
    if n > 10
    then return True
    else return False

get_two_strings :: IO (String, String)
get_two_strings = do
    x <- getLine
    y <- getLine
    let (a, b) = (x , y)
    return (a,b)
    

add_one_forever :: IO ()
add_one_forever = do
    x <- getLine
    let n = (read x :: Int) + 1
    putStrLn . show $ n
    add_one_forever



echo_until_quit :: IO ()
echo_until_quit = do
    x <- getLine
    if x == "quit"
    then return ()
    else do
        putStrLn x
        echo_until_quit


print_numbers_between :: Int -> Int -> IO ()
print_numbers_between a b =
  if a <= b then do
    putStrLn . show $ a
    print_numbers_between (a+1) b
  else
    return ()

-- File handling
print_file :: String -> IO () 
print_file name = do
    x <- readFile name
    putStrLn x


first_line :: String -> IO ()
first_line name = do
    x <- readFile name
    let n = head . lines $ x
    putStrLn n

get_lines :: String -> IO [String]
get_lines name = do
    x <- readFile name
    let n = lines $ x
    return n

write_to :: String -> Int -> IO () 
write_to name n = do
    writeFile name (show n)

copy_file :: String -> String -> IO()
copy_file a b = do
    x <- readFile a
    writeFile b x

write_lines :: String -> [String] -> IO () 
write_lines a list = do
    let n = unlines $ list
    writeFile a n 


one_arg = do
    args <- getArgs
    putStrLn . unlines $ args

sum_two = do
    args <- getArgs
    let n = read(head args) :: Int
        x = read(args !! 1) :: Int
    putStrLn . show $ (n + x)


read_file_and_print = do
    args <- getArgs
    let file = args !! 0
    x <- readFile file
    putStrLn $ x

copy = do
    args <- getArgs
    let arg1 = args !! 0
        arg2 = args !! 1
    copy_file arg1 arg2
