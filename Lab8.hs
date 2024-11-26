data Direction = North | East | South | West deriving(Show, Eq, Read, Ord)



-- North == North    True
-- North /= South    True
-- read("North") :: Direction     North

-- North < East    True
-- max South West    West

is_north :: Direction -> Bool
is_north North = True
is_north _ = False

dir_to_int :: Direction -> Int
dir_to_int North = 1
dir_to_int East = 2
dir_to_int South = 3
dir_to_int West = 4


data Point = Point Int Int deriving(Show, Read)

same :: Int -> Point
same x = Point x x

is_zero :: Point -> Bool
is_zero (Point x y) = if x == 0 && y == 0 then True else False

mult_point :: Point -> Int
mult_point (Point x y) = x * y

up_two :: Point -> Point
up_two (Point x y) = Point x (y+2)

add_points :: Point -> Point -> Point
add_points (Point x1 y1) (Point x2 y2) = Point(x1+x2) (y1 + y2)

not_nothing :: (Eq a) => Maybe a -> Bool
not_nothing Nothing =False
not_nothing x = True

safe_tail :: [a] -> Maybe [a]
safe_tail [] = Nothing
safe_tail list = Just (tail list)


mult_maybe :: Maybe Int -> Maybe Int -> Maybe Int 
mult_maybe _ Nothing = Nothing
mult_maybe Nothing _ = Nothing
mult_maybe (Just x) (Just y) = Just (y*x)


return_two :: Int -> Either Bool Char
return_two n = if n == 1 then Left True else Right 'a'


show_right :: Either String Int -> String
show_right (Left x) = x
show_right (Right y) = show y


data List a = Cons a (List a) | Empty deriving (Show, Read)
-- (b) Cons 1 . Cons 2 . Cons 3 $ Cons 4 Empty
is_empty :: List a -> Bool
is_empty Empty = True
is_empty _ = False

is_single :: List a -> Bool
is_single (Cons x Empty) = True
is_single Empty = False
is_single (Cons _ _) = False

mult :: List Int -> Int
mult Empty = 1
mult (Cons x xs) = x * mult xs

our_map :: (a -> b) -> List a -> List b
our_map _ (Empty) = Empty
our_map f (Cons x xs) = f x `Cons` our_map f (xs)

data DTree a = Leaf a | Branch a (DTree a) (DTree a) deriving (Show, Read)

-- (b):  Branch 2 (Leaf 3) (Branch 1 (Leaf 9) (Leaf 5))


tree_mult :: DTree Int -> Int
tree_mult (Leaf a) = a
tree_mult (Branch x l r) = x * tree_mult l * tree_mult r

sum_leafs :: DTree Int -> Int
sum_leafs (Leaf a) = a
sum_leafs (Branch x l r) = sum_leafs l + sum_leafs r

count_threes :: DTree Int -> Int
count_threes (Leaf x) = if x == 3 then 1 else 0
count_threes (Branch x l r) = if x == 3 then 1 + lr else lr
    where lr = count_threes l + count_threes r

get_leafs :: DTree Int -> [Int]
get_leafs (Leaf x) = [x]
get_leafs (Branch x l r) = get_leafs l ++ get_leafs r
