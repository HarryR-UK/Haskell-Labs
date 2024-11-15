-- For Parts A and B

type Transaction = (Char, Int, Int, String, Int) 

test_log :: [Transaction]
test_log = [('B', 100, 1104,  "VTI",  1),
            ('B', 200,   36, "ONEQ",  3),
            ('B',  50, 1223,  "VTI",  5),
            ('S', 150, 1240,  "VTI",  9),
            ('B', 100,  229, "IWRD", 10),
            ('S', 200,   32, "ONEQ", 11), 
            ('S', 100,  210, "IWRD", 12)
            ]


-- For Part C

test_str_log = "BUY 100 VTI 1\nBUY 200 ONEQ 3\nBUY 50 VTI 5\nSELL 150 VTI 9\nBUY 100 IWRD 10\nSELL 200 ONEQ 11\nSELL 100 IWRD 12\n"

type Prices = [(String, [Int])]

test_prices :: Prices
test_prices = [
                ("VTI", [1689, 1785, 177, 1765, 1739, 1725, 1615, 1683, 1655, 1725, 1703, 1726, 1725, 1742, 1707, 1688, 1697, 1688, 1675]),
                ("ONEQ", [201, 203, 199, 199, 193, 189, 189, 183, 185, 190, 186, 182, 186, 182, 182, 186, 183, 179, 178]),
                ("IWRD", [207, 211, 213, 221, 221, 222, 221, 218, 226, 234, 229, 229, 228, 222, 218, 223, 222, 218, 214])
              ]


-- PART A
transaction_to_string :: Transaction -> String
transaction_to_string (typ, units, ppu, stock, day) = 
    let 
      bought = "Bought " ++ show units ++ " units of " ++ stock ++ " for " ++ show ppu ++ " pounds each on day " ++ show day
      sold = "Sold " ++ show units ++ " units of " ++ stock ++ " for " ++ show ppu ++ " pounds each on day " ++ show day
    in
      if typ == 'B' then bought else sold


trade_report_list :: [Transaction] -> [String]
trade_report_list log = map (transaction_to_string) log

stock_test :: String -> Transaction -> Bool
stock_test stock (_,_,_,stock_compare,_) = if stock == stock_compare then True else False

get_trades :: String -> [Transaction] -> [Transaction]
get_trades stock log = filter (\(x,y,z,l,p) -> l == stock) log


trade_report :: String -> [Transaction] -> String
trade_report stock log = unlines . map (\x -> transaction_to_string x) $  get_trades stock log


-- PART B
update_money :: Transaction -> Int -> Int
update_money (action, units, ppu, stock, day) money 
  | action == 'B' = money - diff
  | otherwise = money + diff
  where diff = (units * ppu)


-- profit :: [Transaction] -> String -> Int
--profit log stock = foldr (\(x,y,z,l,p) acc -> if l == stock then (update_money((x,y,z,l,p) acc)) else acc) 0 log


profit :: [Transaction] -> String -> Int
profit log stock_name = foldr (\x acc -> update_money x acc) 0 $ get_trades stock_name log

profit_report :: [String] -> [Transaction] -> String
profit_report stocks log = unlines $ zipWith (\x y -> x ++ ": " ++ show (profit log x)) stocks log


-- Part C
-- take and drop while

complex_profit_update_money trade money p =
  let
    action = takeWhile(/= ' ') trade
    after_action = dropWhile(/= ' ') trade
    units = read (takeWhile (/= ' ') $ dropWhile (== ' ')after_action) :: Int
    after_units = dropWhile(/= ' ') $ dropWhile (== ' ') after_action
    stock = takeWhile(/= ' ') $ dropWhile (==' ') after_units
    after_stock = dropWhile(/= ' ') $ dropWhile(==' ') after_units
    day = takeWhile(/=' ') $ dropWhile(==' ') after_stock
    stock_price = (snd . head $ filter(\x -> fst x == stock) p) !! ((read(day)::Int) - 1) 
  in
    if action == "BUY" then (money - (units * stock_price)) else (money + (units * stock_price))

complex_profit_get_name trade = takeWhile(/=' ') . dropWhile(==' ') . dropWhile(/=' ') . dropWhile(==' ')  $ dropWhile(/=' ')trade

complex_profit_get_trades stock_name log = filter(\x -> complex_profit_get_name x == stock_name) log

complex_profit_get_stock_names ps = foldr (\(x,y) acc -> x : acc) [] ps

complex_profit log stock_name price_log = foldr(\x acc -> complex_profit_update_money x acc price_log) 0 $ complex_profit_get_trades stock_name log

complex_profit_report :: String -> Prices -> String
complex_profit_report log p = unlines $ map(\ x -> x ++ ": " ++ show(complex_profit (lines log) x p)) (complex_profit_get_stock_names p)
