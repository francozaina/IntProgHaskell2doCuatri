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

esMulti:: Integer -> Integer -> Bool
esMulti x y 
    | mod x y == 0 = True
    | otherwise = False

digitoUnidades :: Integer -> Integer
digitoUnidades n = abs n `mod` 10

digitoDecenas :: Integer -> Integer
digitoDecenas n = abs n `mod` 100

estanRelacionados :: Integer -> Integer -> Bool
estanRelacionados a b = a /= 0 && b /= 0 && (-a) `mod` b == 0 && (-a) `div` b /= 0

productoInterno :: (Integer, Integer) -> (Integer, Integer) -> Integer
productoInterno (a,b) (x,y) = a * x + b * y

esParMenor :: (Integer, Integer) -> (Integer, Integer) -> (Bool, Bool)
esParMenor (a,b) (x,y)
    | a > x && b > y = (False, False) 
    | a > x && b < y = (False, True)
    | a < x && b > y = (True, False)
    | a < x && b < y  = (True,True)

distancia :: (Float, Float) -> Float
distancia (a,b) = sqrt(a*a + b*b)

sumaTerna :: (Integer,Integer,Integer) -> Integer
sumaTerna (a,b,c) = a + b + c

sumarSoloMultiplos :: (Integer,Integer,Integer) -> Integer -> Integer
sumarSoloMultiplos (a,b,c) x
    | esMulti a x == True && esMulti b x == True && esMulti c x == True = a+b+c
    | esMulti a x == False && esMulti b x == False && esMulti c x == False = 0
    | esMulti a x == True && esMulti b x == False && esMulti c x == False = a
    | esMulti a x == False && esMulti b x == True && esMulti c x == False = b
    | esMulti a x == False && esMulti b x == False && esMulti c x == True = c
    | esMulti a x == True && esMulti b x == True && esMulti c x == False = a + b
    | esMulti a x == True && esMulti b x == False && esMulti c x == True = a + c
    | esMulti a x == False && esMulti b x == True && esMulti c x == True = b + c

posPrimerPar:: (Integer,Integer,Integer) -> Integer
posPrimerPar (a,b,c)
    | mod a 2 == 0 = 0
    | mod b 2 == 0 = 1
    | mod c 2 == 0 = 2

crearPar :: Integer -> Integer -> (Integer,Integer)
crearPar a b = (a,b)

invertir :: (Integer,Integer) -> (Integer, Integer)
invertir (a,b) = (b,a)

--Ejercicio 5
funcionf :: Integer -> Integer
funcionf x
    | x <= 7 = x*x
    | x > 7 = 2*x -1

funciong :: Integer -> Integer
funciong x 
    | mod x 2 == 0 = div x 2
    | otherwise = 3*x+1

todosMenores :: (Integer,Integer,Integer) -> Bool
todosMenores (a,b,c)
    | funcionf a > funciong a && funcionf b > funciong b && funcionf c > funciong c = True
    | otherwise = False
