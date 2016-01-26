intToDigits :: Int -> [Int]
intToDigits x =
  if x `div` 10 == 0 then [x] else  intToDigits (div x 10) ++ [mod x 10]

doubleFromRight :: [Int] -> [Int]
doubleFromRight list =
  fst $ foldr (\x (acc, bool) ->
    ((if bool then 2 * x else x) : acc, not bool)) ([], False) list

sumDigits :: [Int] -> Int
sumDigits list = foldr (+) 0 (concat (map intToDigits list))

validate :: Int -> Bool
validate num = sumDigits (doubleFromRight (intToDigits num)) `mod` 10 == 0
