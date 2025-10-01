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

mismosElementosAux :: (Eq t) => [t] -> [t] -> Bool
mismosElementosAux [] _ = True
mismosElementosAux (x:xs) y
    | pertenece x y == True = mismosElementosAux xs y
    | pertenece x y == False = False

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos x y
    | mismosElementosAux x y && mismosElementosAux y x = True
    | otherwise = False

iesimaPosicion :: (Eq t) => Integer -> [t] -> t
iesimaPosicion x (y:ys)
    | x > 0 = iesimaPosicion (x-1) (ys)
    | x == 0 = y

--Elimina primero y ultimo eleemento

capicuaAux :: (Eq t) => [t] -> [t]
capicuaAux x = quitar (iesimaPosicion 0 x) x

capicuaAux2 :: (Eq t) => [t] -> [t]
capicuaAux2 x = quitar (iesimaPosicion ((longitud x)-1) x) x

capicua :: (Eq t) => [t] -> Bool 
capicua [] = True
capicua [x] = True
capicua x 
    | head x == ultimo x = capicua (capicuaAux (capicuaAux2 x))
    | otherwise = False

sumatoria ::  [Integer] -> Integer
sumatoria [] = 0
sumatoria [x] = x
sumatoria (x:xs) = x + sumatoria xs

productoria :: [Integer] -> Integer
productoria [] = 0
productoria [x] = x
productoria (x:xs) = x * productoria xs

maximo :: [Integer] -> Integer 
maximo [] = 0
maximo [x] = x
maximo (x:y:xs)
    | x > y = maximo (x:xs)
    | x < y = maximo (y:xs)

sumarNAux :: Integer -> Integer -> [Integer] -> [Integer]
sumarNAux x y (z:zs)
    | y > 0 = sumarNAux x (y-1) (zs ++ [x+z])
    | y == 0 = z:zs

sumarN :: Integer -> [Integer] -> [Integer]
sumarN x y = sumarNAux x (longitud y) y

sumarElPrimero ::  [Integer] -> [Integer]
sumarElPrimero x = sumarN (iesimaPosicion 0 x) x

sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo x = sumarN (ultimo x) x

paresAux ::[Integer] -> Integer -> [Integer]
paresAux (x:xs) y  
    | y > 0 && mod x 2 == 0 = paresAux (xs ++ [x]) (y-1)
    | y > 0 && mod x 2 /= 0 = paresAux xs (y-1)
    | y == 0 = x:xs

pares ::  [Integer] -> [Integer]
pares x = paresAux x (longitud x)

multiplosDeNAux:: [Integer] -> Integer -> Integer -> [Integer]
multiplosDeNAux (x:xs) l n
    | l > 0 && mod x n == 0 = multiplosDeNAux (xs++[x]) (l-1) n 
    | l > 0 && mod x n /= 0 = multiplosDeNAux xs (l-1) n
    | l == 0 = (x:xs)

multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN x y = multiplosDeNAux y (longitud y) x 


ordenarAux :: [Integer] -> Integer -> [Integer]
ordenarAux x l
    |l > 0 = maximo x : ordenarAux (quitarTodos (maximo x) x) (l-1)
    |l == 0 = x

ordenar :: [Integer] -> [Integer]
ordenar x = reverso (ordenarAux x (longitud x))

sacarBlancosRepetidos :: [Char] -> [Char]
sacarBlancosRepetidos [] = []
sacarBlancosRepetidos [x] = [x]
sacarBlancosRepetidos (x:y:xs)
    | x == ' ' && y == ' ' = sacarBlancosRepetidos (x:xs)
    | otherwise = x : sacarBlancosRepetidos (y:xs)


contarPalabras :: [Char] -> Integer
contarPalabras xs = contarAux xs 0
  where    
    contarAux [] acc = acc
    contarAux (x:xs) acc
      | x == ' '  = contarAux (sacarEspacios xs) acc
      | otherwise = contarAux (sacarEspacios xs) (acc + 1)

    sacarEspacios [] = []
    sacarEspacios (x:xs)
      | x == ' '  = sacarEspacios xs
      | otherwise = (x:xs)

--listaContactos

type Texto = [Char]
type Nombre = Texto
type Telefono = Texto
type Contacto = (Nombre, Telefono)
type ContactosTel = [Contacto]

misContactos :: ContactosTel
misContactos =
  [ ("Juan", "1234")
  , ("Ana",  "5678")
  , ("Pedro","9999")
  ]

enLosContactos :: Nombre -> ContactosTel -> Bool
enLosContactos _ [] = False
enLosContactos nombre ((n,t):xs)
  | nombre == n = True
  | otherwise   = enLosContactos nombre xs

agregarContactoAux :: Contacto -> ContactosTel -> ContactosTel
agregarContactoAux _ [] = []
agregarContactoAux (x,z) ((n,t):xs)
  | x == n = xs ++ [(x,z)]
  | otherwise   = agregarContactoAux (x,z) xs

agregarContacto ::  Contacto -> ContactosTel -> ContactosTel
agregarContacto (x,z) c
    | enLosContactos x c == False = c ++ [(x,z)] 
    | otherwise = agregarContactoAux (x,z) c

eliminarContactoAux:: Nombre -> ContactosTel -> Integer -> ContactosTel
eliminarContactoAux x ((n,t):xs) l
    | l > 0 && x == n = xs
    | l > 0 && x /= n = eliminarContactoAux x (xs ++ [(n,t)]) (l-1)
    | l == 0 = ((n,t):xs)

eliminarContacto :: Nombre -> ContactosTel -> ContactosTel
eliminarContacto x c = eliminarContactoAux x c (longitud c)

--mapaDeLockers
type Identificacion = Integer
type Ubicacion = Texto
type Estado = (Disponibilidad, Ubicacion)
type Locker = (Identificacion, Estado)
type MapaDeLockers = [Locker]
type Disponibilidad = Bool

lockersDePrueba :: MapaDeLockers
lockersDePrueba =
  [ (100,(False,"ZD39I")),
    (101,(True,"JAH3I")),
    (103,(True,"IQSA9")),
    (105,(True,"QOTSA")),
    (109,(False,"893JJ")),
    (110,(False,"99292"))
  ]

existeElLocker :: Identificacion -> MapaDeLockers -> Bool
existeElLocker i [] = False
existeElLocker i ((n,d):xs)
    | i == n = True
    | i /= n = existeElLocker i xs

ubicacionDelLocker :: Identificacion -> MapaDeLockers -> Ubicacion
ubicacionDelLocker i ((n,(v,d)):xs)
    | i == n = d
    | i /= n = ubicacionDelLocker i xs    

estaDisponibleElLocker :: Identificacion -> MapaDeLockers -> Bool
estaDisponibleElLocker i ((n,(v,d)):xs)
    | i == n && v == True = True
    | i == n && v == False = False
    | i /= n = estaDisponibleElLocker i xs

type Matriz = [[Integer]]

matriz :: Matriz
matriz =    
    [ 
        [1,2,3],
        [4,5,6],
        [7,8,9]
    ]

sumaFila :: [Integer] -> Integer
sumaFila [] = 0
sumaFila (x:xs) = x + sumaFila xs

sumaTotal :: [[Integer]] -> Integer
sumaTotal [] = 0
sumaTotal (fila:resto) = sumaFila fila + sumaTotal resto

cuentaFila :: Integer -> [Integer] -> Integer
cuentaFila _ [] = 0
cuentaFila n (x:xs)
    | n == x = 1 + cuentaFila n xs
    | otherwise = cuentaFila n xs

cantidadDeApariciones :: Integer -> [[Integer]] -> Integer
cantidadDeApariciones _ [] = 0
cantidadDeApariciones n (fila:filas) = cuentaFila n fila + cantidadDeApariciones n filas

multiN :: Integer -> Integer -> [Integer] -> [Integer]
multiN x y (z:zs)
    | y > 0 = multiN x (y-1) (zs ++ [x*z])
    | y == 0 = z:zs

multiplicarN :: Integer -> [Integer] -> [Integer]
multiplicarN x y = multiN x (longitud y) y

multiplicarPorEscalarAux :: Integer -> [[Integer]] -> Integer -> [[Integer]]
multiplicarPorEscalarAux _ [] _ = []
multiplicarPorEscalarAux e (fila:filas) n
    | n > 0 = [multiplicarN e fila] ++ multiplicarPorEscalarAux e filas (n-1)
    | n == 0 = fila:filas

multiplicarPorEscalar :: Integer -> [[Integer]] -> [[Integer]]
multiplicarPorEscalar x y = multiplicarPorEscalarAux x y (longitud y)

concatenarListas :: [String] -> [String] -> [String]
concatenarListas x y = x ++ y

concatenarFilas :: [[String]] ->[String]
concatenarFilas [] = []
concatenarFilas [[x]] = [x]
concatenarFilas (x:y:xs) = concatenarListas x y ++ concatenarFilas xs


iesimaFila :: Integer -> [[a]] -> [a]
iesimaFila i (fila:filas)
    | i == 0 = fila
    | otherwise = iesimaFila (i-1) filas

iesimaPosicion2 :: Integer -> [a] -> a
iesimaPosicion2 x (y:ys)
    | x > 0 = iesimaPosicion2 (x-1) (ys)
    | x == 0 = y

iesimaColumnaAux :: Integer -> Integer -> [[a]] -> [a]
iesimaColumnaAux _ _ [] = []
iesimaColumnaAux l i (fila:filas)
    | l > 0    = iesimaPosicion2 i fila : iesimaColumnaAux (l-1) i filas
    | l == 0   = []

iesimaColumna :: Integer -> [[a]] -> [a]
iesimaColumna x y = iesimaColumnaAux (longitud y) x y
