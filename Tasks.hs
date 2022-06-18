
-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

-- ==================================================

module Tasks where

import Dataset
import Data.List
import Text.Printf
import Data.Array

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]
type ColumnName = String

-- Prerequisities
split_by :: Char -> String -> [String]
split_by x = foldr op [""]
  where op char acc
            | char == x = "":acc
            | otherwise = (char:head(acc)):tail(acc)

read_csv :: CSV -> Table
read_csv = (map (split_by ',')) . (split_by '\n')

write_csv :: Table -> CSV
write_csv = (foldr (++) []).
            (intersperse "\n").
            (map (foldr (++) [])).
            (map (intersperse ","))


-- Functie de afisat numere
floatToStr :: Float -> String
floatToStr = printf "%.2f"

-- Transformare de la Int la Float
intToFloat :: Int -> Float
intToFloat n = fromInteger (toInteger n)

toFloat :: Value -> Float
toFloat x = read x :: Float

toInt :: Value -> Int
toInt x = read x :: Int

toString :: Value -> Value
toString x = read x :: [Char]

{-
    TASK SET 1
-}


-- Task 1
compute_average_steps :: Table -> Table
compute_average_steps [] = []
compute_average_steps (("Name":value):xs) = ("Name":["Average Number of Steps"]) : compute_average_steps xs
compute_average_steps ((name:values):xs) = (name : [get_avg values]) : compute_average_steps xs
compute_average_steps _ = []

get_avg :: [Value] -> Value
get_avg x = floatToStr (foldr ((+).toFloat) 0 x/8)

-- Task 2

-- Number of people who have achieved their goal:
get_passed_people_num :: Table -> Int
get_passed_people_num x = calc_passed_people x 0

calc_passed_people :: Table -> Int -> Int
calc_passed_people [] nr = nr
calc_passed_people ((name:values):xs) nr
  | (foldr ((+).toFloat) 0 values/1) >= 1000 = calc_passed_people xs (nr+1)
  | otherwise  = calc_passed_people xs nr
calc_passed_people _ nr = nr


-- Percentage of people who have achieved their:
get_passed_people_percentage :: Table -> Float
get_passed_people_percentage xs = intToFloat (get_passed_people_num xs) / intToFloat (length xs-1)


-- Average number of daily steps
get_steps_avg :: Table -> Float
get_steps_avg xs = calc_avg_all xs 0

calc_avg_all :: Table -> Float -> Float 
calc_avg_all [] nr = nr/132
calc_avg_all (("Name":values):xs) nr = calc_avg_all xs nr
calc_avg_all ((name:values):xs) nr = calc_avg_all xs (calc_sum values + nr)
calc_avg_all _ nr = nr

calc_sum :: Row -> Float 
calc_sum xs =  (foldr ((+).toFloat) 0 xs/1)

-- Task 3

get_avg_steps_per_h :: Table -> Table
get_avg_steps_per_h [] = []
get_avg_steps_per_h ([]:xs) = get_avg_steps_per_h xs
get_avg_steps_per_h (("Name":value):xs) = ((["H10","H11","H12","H13","H14","H15","H16","H17"]): (get_avg_column_per_h (transpose_table xs)))
get_avg_steps_per_h _ = []

get_avg_column_per_h :: Table -> Table
get_avg_column_per_h (("Olivia Noah":names):xs) = get_avg_column_per_h xs
get_avg_column_per_h xs = formTable [] (calcAvgSteps (xs))
  where
      formTable :: Row -> Table -> Table
      formTable row [] = [row]
      formTable row ((x:value):xs) = formTable (row ++ [x]) xs
      formTable row xs = formTable row xs

transpose_table :: Table -> Table
transpose_table ([]:_) = []
transpose_table m = (map head m):(transpose_table (map tail m))

calcAvgSteps :: Table -> Table
calcAvgSteps [] = []
calcAvgSteps ([]:xs) = calcAvgSteps xs
calcAvgSteps (value:xs) = ([sumAvg value]):(calcAvgSteps xs)

sumAvg :: [Value] -> Value
sumAvg x = printf "%.2f" (foldr ((+) . toFloat) 0 x/132)


-- Task 4

prima_linie = ["column", "range1", "range2", "range3"]

get_activ_summary :: Table -> Table
get_activ_summary [] = []
get_activ_summary (("Name":value):xs) = ["column","range1","range2","range3"] : formTable [["VeryActiveMinutes"],["FairlyActiveMinutes"],["LightlyActiveMinutes"]] (tail(tail(tail(transpose xs))))
get_activ_summary (x:xs) = xs

formTable :: Table -> Table -> Table
formTable (["VeryActiveMinutes"]:xs) (values:ys) = ("VeryActiveMinutes" : very_active values 0 0 0) : formTable xs ys
formTable (["FairlyActiveMinutes"]:xs) (values:ys) = ("FairlyActiveMinutes" : very_active values 0 0 0) : formTable xs ys
formTable (["LightlyActiveMinutes"]:xs) (values:ys) = ("LightlyActiveMinutes" : very_active values 0 0 0) : formTable xs ys
formTable _ _ = []

very_active :: Row -> Int -> Int -> Int -> Row
very_active [] a b c = append_values a b c
very_active (x:xs) a b c
  | ((toInt x) < 50) = very_active xs (a+1) b c
  | ((toInt x) < 100) = very_active xs a (b+1) c
  | ((toInt x) < 500) = very_active xs a b (c+1)
very_active _ _ _ _ = []

append_values :: Int -> Int -> Int -> Row
append_values a b c = [(show a), (show b), (show c)]

-- Task 5

get_ranking :: Table -> Table
get_ranking [] = []
get_ranking (("Name":value):xs) = ["Name", "Total Steps"] : bubbleSort (sort_table1 (map init (map init (map init (map init xs)))))
get_ranking (x:xs) = []

sort_table1 :: Table -> Table
sort_table1 [] = []
sort_table1 xs = x : sort_table1 (delete x xs)
  where x = minimum' xs

minimum' :: Table -> Row
minimum' ([a,b]:xs) = foldl' (\ [a,b] [c,d] -> if (toInt b) <= (toInt d) then [a,b] else [c,d]) [a,b] xs
minimum' _ = []

bubbleSortImpl :: Int -> Table -> Table
bubbleSortImpl 0 xs = xs
bubbleSortImpl n xs = bubbleSortImpl (n - 1) (bubble xs)
  where
    bubble :: Table -> Table
    bubble [] = []
    bubble ([a,b] : []) = [a,b] : []
    bubble ([a,b] : [c,d] : ys) = if a <= c && b == d
    then [a,b] : (bubble ([c,d] : ys))
    else [c,d] : (bubble ([a,b] : ys))
    bubble _ = []
bubbleSort :: Table -> Table
bubbleSort xs = let n = length xs
  in bubbleSortImpl n xs

-- Task 6

get_steps_diff_table :: Table -> Table
get_steps_diff_table [] = []
get_steps_diff_table (("Name":value):xs) = ["Name","Average first 4h","Average last 4h","Difference"]:bubbleSort' (sort_by_difference(get_difference (get_average xs)))
get_steps_diff_table (x:xs) = []

get_average :: Table -> Table
get_average [] = []
get_average ((name:a:b:c:d:e:f:g:h:ys):xs) = [name,calc_average ([a,b,c,d]),calc_average ([e,f,g,h])]:get_average xs
get_average _ = []

calc_average :: Row -> Value
calc_average (a:b:c:d:xs) = floatToStr (((toFloat a) + (toFloat b) + (toFloat c) + (toFloat d))/4)
calc_average _ = []

get_difference :: Table -> Table
get_difference ((name:x:y:ys):xs) = [name,x,y,calc_difference x y] : get_difference xs
get_difference _ = []

calc_difference :: Value -> Value -> Value
calc_difference x y 
  | (toFloat x) >= (toFloat y) = floatToStr((toFloat x) - (toFloat y))
  | (toFloat x) < (toFloat y) = floatToStr((toFloat y) - (toFloat x))
calc_difference _ _ = []

sort_by_difference :: Table -> Table
sort_by_difference [] = []
sort_by_difference xs = x : sort_by_difference (delete x xs)
  where x = minimum'' xs

minimum'' :: Table -> Row
minimum'' ([name,a,b,c]:xs) = foldl' (\ [name,a,b,c] [name2,d,e,f] -> if (toFloat c) <= (toFloat f) then [name,a,b,c] else [name2,d,e,f]) [name,a,b,c] xs
minimum'' _ = []

bubbleSortImpl' :: Int -> Table -> Table
bubbleSortImpl' 0 xs = xs
bubbleSortImpl' n xs = bubbleSortImpl' (n - 1) (bubble xs)
  where
    bubble :: Table -> Table
    bubble [] = []
    bubble ([name,a,b,c] : []) = [name,a,b,c] : []
    bubble ([name,a,b,c] : [name2,d,e,f] : ys) = if name <= name2 && toFloat(c) == toFloat(f)
    then [name,a,b,c] : (bubble ([name2,d,e,f] : ys))
    else [name2,d,e,f] : (bubble ([name,a,b,c] : ys))
    bubble _ = []
bubbleSort' :: Table -> Table
bubbleSort' xs = let n = length xs
  in bubbleSortImpl' n xs



-- Task 7

-- Applies the given function to all the values
vmap :: (Value -> Value) -> Table -> Table
vmap = map.map


-- Task 8

-- Applies the given function to all the entries
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap get_sleep_total row xs = map get_sleep_total xs


get_sleep_total :: Row -> Row
get_sleep_total (name:xs) = [name,floatToStr((foldr ((+) . toFloat) 0 xs/1))]
get_sleep_total _ = []


{-
    TASK SET 2
-}

-- Task 1

tsort :: ColumnName -> Table -> Table
tsort name xs = find_column name (transpose_table xs) (transpose_table xs)

find_column :: ColumnName -> Table -> Table -> Table
find_column name table_copy [] = []
find_column name table_copy ((x:values):xs) = if x == name 
  then sort_by_name name [] (transpose_table ((x:values):table_copy)) 
  else find_column name table_copy xs  
find_column _ _ _ = []

sort_by_name :: ColumnName -> Row -> Table -> Table
sort_by_name c_name x ((name:names):xs) 
  | c_name == name = sort_by_name c_name names xs
sort_by_name c_name x xs = x : map tail (sortBy cmp_function xs)

cmp_function :: Row -> Row -> Ordering 
cmp_function (a:c:values1) (b:d:values2) 
  | toInt(a) < toInt(b) = LT 
  | toInt(a) > toInt(b) = GT
  | toInt(a) == toInt(b) = if c < d then LT else GT
cmp_function a b = EQ

-- Task 2

vunion :: Table -> Table -> Table
vunion (names1:xs) (names2:ys) 
  | is_same_names names1 names2 == True = (names1:xs) ++ ys
  | otherwise = (names1:xs)
vunion _ _ = []

is_same_names :: Row -> Row -> Bool 
is_same_names [] [] = True 
is_same_names (x:names1) (y:names2) 
  | x == y = is_same_names names1 names2
  | otherwise = False 
is_same_names _ _ = False 

-- Task 3

hunion :: Table -> Table -> Table
hunion [] [] = []
hunion (row1:xs) (row2:[]) = (row1 ++ row2) : merge_blank xs (create_row row2)
hunion (row1:[]) (row2:ys) = (row1 ++ row2) : merge_blank ys (create_row row1)
hunion (row1:xs) (row2:ys) = (row1 ++ row2) : hunion xs ys 
hunion _ _ = []

merge_blank :: Table -> Row -> Table
merge_blank [] blank = []
merge_blank (row:xs) blank = (row ++ blank) : merge_blank xs blank 

create_row :: Row -> Row
create_row [] = []
create_row (x:xs) = "" : create_row xs

-- Task 4

tjoin :: ColumnName -> Table -> Table -> Table
tjoin key_column [] [] = []
tjoin key_column t1 t2 
  | check_key key_column t1 == check_key key_column t2 = 
    map tail (merge_tables (hunion (extract_column key_column t1) t1) (hunion (extract_column key_column t2) t2)) 
  | otherwise = t1

check_key :: ColumnName  -> Table -> Bool
check_key name [] = False
check_key name ((x:values):xs) = if x == name 
  then True 
  else check_key name xs  
check_key _ _ = False 

merge_tables :: Table -> Table -> Table
merge_tables ((name:values):xs) ys 
  | find_column_key name ys == [] = merge_tables xs ys 
  | otherwise = ((name:values) ++ (find_column_key name ys)) : merge_tables xs ys
merge_tables _ _ = []

find_column_key :: Value -> Table -> Row
find_column_key key [] = []
find_column_key key ((name:rem:values):ys)
  | key == name = values
  | otherwise = find_column_key key ys
find_column_key _ _ = []

-- Task 5

cartesian :: (Row -> Row -> Row) -> [ColumnName] -> Table -> Table -> Table
cartesian function column_names [] ys = []
cartesian function column_names (row1:xs) (row2:ys) 
  | (function row1 row2) == column_names = column_names : cartesian function column_names xs ys
  | otherwise = create_table_from_entry function row1 (row2:ys) ++ cartesian function column_names xs (row2:ys)
cartesian _ _ _ _ = []

create_table_from_entry :: (Row -> Row -> Row) -> Row -> Table -> Table
create_table_from_entry function row [] = []
create_table_from_entry function row (row1:xs) = (function row row1) : create_table_from_entry function row xs

-- Task 6

projection :: [ColumnName] -> Table -> Table
projection names xs = projection' names xs [[]]

projection' :: [ColumnName] -> Table -> Table -> Table
projection' [] xs ys = ys
projection' (name:rest) xs ys = projection' rest xs (hunion ys (extract_column name xs)) 

extract_column :: ColumnName -> Table -> Table
extract_column column_name ((name:rest):xs) 
  | column_name == name = form_as_table ((name:rest):xs) 
  | otherwise = extract_column column_name (map tail ((name:rest):xs))
extract_column _ _ = []

form_as_table :: Table -> Table
form_as_table [] = []
form_as_table ((name:rest):xs) = [name] : form_as_table xs
form_as_table _ = []

-- Task 7

filterTable :: (Value -> Bool) -> ColumnName -> Table -> Table
filterTable condition key_column t = 
  map tail (delete_columns condition key_column (copy_column key_column (transpose_table t) (transpose_table t)))

copy_column :: ColumnName -> Table -> Table -> Table
copy_column name table_copy [] = []
copy_column name table_copy ((x:values):xs) = if x == name 
  then transpose_table ((x:values):table_copy)
  else copy_column name table_copy xs  
copy_column _ _ _ = []

delete_columns :: (Value -> Bool) -> ColumnName -> Table -> Table
delete_columns condition column_name [] = []
delete_columns condition column_name ((el:values):xs) 
  | column_name == el = (el:values) : delete_columns condition column_name xs
delete_columns condition column_name ((el:values):xs) 
  | condition el == True = (el:values) : delete_columns condition column_name xs
  | condition el == False = delete_columns condition column_name xs
delete_columns _ _ _ = []
