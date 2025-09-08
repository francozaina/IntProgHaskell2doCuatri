-- Guia 2
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Data.Bits (Bits(xor))
{-# HLINT ignore "Use map" #-}
doubleMe:: Integer -> Integer
doubleMe x = x + x

raizCuadrada :: Integer -> Integer
raizCuadrada x = x*x

enteroMasCercano :: Integer -> Integer
enteroMasCercano x = x+1

raicesCuadradasUno :: [Integer] -> [Integer]
raicesCuadradasUno [] = [] 
raicesCuadradasUno (x:xs) = raizCuadrada x : raicesCuadradasUno xs

problemaOrdenar :: [Integer] -> [Integer]
problemaOrdenar [] = []
problemaOrdenar [x] = [x]
problemaOrdenar (x:y:xs)
  | x > y     = y : problemaOrdenar (x:xs)
  | otherwise = x : problemaOrdenar (y:xs)

duplicarTodos :: [Integer] -> [Integer]
duplicarTodos [] = []
duplicarTodos (x:xs) = doubleMe x : duplicarTodos xs

pares :: [Integer]-> [Integer]
pares [] = []
pares (x:xs) 
    | mod x 2 == 0 = x :  pares xs
    | otherwise = pares xs

absoluto :: Integer -> Integer
absoluto x
    | x > 0 = x
    | x < 0 = -x 

sumarAbsMayorA5 :: [Integer] -> Integer 
sumarAbsMayorA5 [] = 0
sumarAbsMayorA5 (x:xs)
    | absoluto x > 5 = absoluto x + sumarAbsMayorA5 xs
    |otherwise = sumarAbsMayorA5 xs    