import Data.Bits (Bits(xor))
import Control.Concurrent (yield)
import Control.Arrow (ArrowZero(zeroArrow))
f :: Integer -> Integer
f x
    | x == 1 = 8
    | x == 4 = 131
    | x == 16 = 16

g :: Integer -> Integer
g x
    | x == 8 = 18
    | x == 16 = 4
    | x == 131 = 1

fog :: Integer -> Integer
fog x = f (g x)

gof :: Integer -> Integer
gof x = g (f x)

absoluto :: Integer -> Integer
absoluto x
    | x > 0  = x
    | x < 0 = -x

maximoAbsoluto :: Integer -> Integer -> Integer
maximoAbsoluto x y
    |absoluto x > absoluto y = absoluto x
    |absoluto y > absoluto x = absoluto y

maximo :: Integer -> Integer -> Integer -> Integer 
maximo x y z
    | x > z && x > y = x
    | y > x && y > z = y
    | z > x && z > y = z

algunoEsCero :: Integer -> Integer -> Integer
algunoEsCero x y
    | x == 0 = x
    | y == 0 = y
    | x == 0 && y == 0 = x 

ambosSonCero :: Integer -> Integer -> Integer 
ambosSonCero x y
    | x == 0 && y == 0 = 0

sumaDistintos :: Integer -> Integer -> Integer -> Integer
sumaDistintos x y z 
    | x /= y && x /= z && y /= z = x+y+z
    | x /= y && x == z = x+y
    | x == y && x /=z = x+z
    | x == y && x == z && y /= z = z+y 

