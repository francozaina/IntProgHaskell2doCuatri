import GHC.IO.Handle.Types (Handle__)
import Data.Bits (Xor)
longitud :: [t] -> Integer
longitud [] = 0
longitud [x] = 1 
longitud (x:xs) = 1 + longitud xs

ultimo :: [t] -> t  
ultimo [x] = x
ultimo (x:xs) = ultimo xs

principioAux :: [t] -> Integer -> [t]
principioAux [] _ = []
principioAux (x:xs) y
    | y > 0 = x : principioAux xs (y - 1)
    | otherwise = []

principio :: [t] -> [t]
principio xs = principioAux xs (longitud xs - 1)

reverso :: [t] -> [t]
reverso [] = []
reverso [x] = [x]
reverso (x:xs) = reverso xs ++ [x]

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece n [] = False
pertenece n (x:xs) 
    | n == x = True
    | otherwise = pertenece n xs


todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = False
todosIguales [x] = True
todosIguales (x:y:xs) | x == y = todosIguales (y:xs)
    |otherwise = False

todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [] = True 
todosDistintos [x] = True
todosDistintos (x:y:xs) | pertenece x (y:xs) = False
                        | otherwise = todosDistintos (y:xs)

quitar :: (Eq t) => t -> [t] -> [t]
quitar n [] = []
quitar n (x:xs) | n == x = xs 
                | otherwise = x : quitar n xs

quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos n (x:xs) | n == x = quitarTodos n xs
                     | otherwise = x : quitarTodos n xs

eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = x : eliminarRepetidos (quitarTodos x xs)
