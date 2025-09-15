
cantDigitos :: Integer -> Integer
cantDigitos x
    | x == 0 = 1
    | x < 10 = 1
    | otherwise = 1 + cantDigitos(div x 10) 