mod10 = flip mod 10
div10 = flip div 10

intToDigits :: Int -> [Int]
intToDigits x
  | (xDiv10 == 0) = [x]
  | otherwise = intToDigits (xDiv10) ++ [mod10 x]
  where xDiv10 = div10 x

doubleFromRight :: [Int] -> [Int]
doubleFromRight list =
  fst $ foldr (\x (acc, bool) ->
    ((if bool then 2 * x else x) : acc, not bool)) ([], False) list


sumDigits :: [Int] -> Int
sumDigits list = foldr (+) 0 $ concat $ map intToDigits list

validate :: Int -> Bool
validate num = mod10 (sumDigits (doubleFromRight  $ intToDigits num)) == 0

-- validate
