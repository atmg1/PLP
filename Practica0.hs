import Data.List
import Data.Maybe
import Data.Either

--Ej 1:
{-
-null :: Foldable t => t a -> Bool
toma una "lista" y retorna true si esta vacia false en caso contrario

-head :: GHC.Has.Stack.Types.HasCallStack => [a] -> a
toma una "lista" y devuelve el primer elemento

-tail :: GHC.Has.Stack.Types.HasCallStack => [a] -> [a]
toma una "lista" y devuelve la sublista excluyendo el primer elemento

-init :: GHC.Has.Stack.Types.HasCallStack => [a] -> [a]
toma una "lista" y devuelve la sublista excluyendo el ultimo elemento

-last :: GHC.Has.Stack.Types.HasCallStack => [a] -> a
toma una "lista" y devuelve el ultimo elemento

-take :: Int -> [a] -> [a]
toma un entero n una lista y devuelve la sublista con los primeros n elementos

-drop :: Int -> [a] -> [a]
toma un entero n una lista y devuelve la sublista sin los primeros n elementos

-(++) :: [a] -> [a] -> [a]
toma 2 listas y las concatena

-concat :: Foldable t => t [a] -> [a]
toma una "lista" de listas y las concatena

-reverse :: [a] -> [a]
toma una lista y la da vuelta

-elem :: (Foldable t, Eq a) => a -> t a -> Bool
toma un valor de tipo a, una "lista" de valores de tipo a y devuelve si esta o no en la lista
-}

--Ej 2:
--a
valorAbsoluto :: Float -> Float
valorAbsoluto x | x >= 0 = x
                | otherwise = -x

--b
bisiesto :: Int -> Bool
bisiesto x | mod x 100 == 0 && mod x 400 /= 0 = False
           | mod x 4 == 0 = True
           | otherwise = False

--c
factorial :: Int -> Int
factorial 1 = 1
factorial x = x * factorial (x-1)

--d
cantDivisoresDesde :: Int -> Int -> Int
cantDivisoresDesde x y | x > y = 0
                       | (mod y x) == 0 = 1 + cantDivisoresDesde (x+1) y
                       | otherwise = cantDivisoresDesde (x+1) y

cantDivisores :: Int -> Int
cantDivisores 1 = 1
cantDivisores x = cantDivisoresDesde 1 x

esPrimo :: Int -> Bool
esPrimo x = cantDivisores x == 2

cantDivisoresPrimosAux :: Int -> Int -> Int
cantDivisoresPrimosAux x y | x > y = 0
                           | (mod y x == 0) && (esPrimo x) = 1 + cantDivisoresPrimosAux (x+1) y
                           | otherwise = cantDivisoresPrimosAux (x+1) y

cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos x = cantDivisoresPrimosAux 1 x

--Ej 3:
--a
inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso x = Just (1/x)

--b
aEntero :: Either Int Bool -> Int
aEntero (Right y) | y = 1
                  | otherwise = 0
aEntero (Left x) = x

--Ej 4:
--a
limpiarLetra :: Char -> String -> String
limpiarLetra _ [] = []
limpiarLetra a [x] | a == x = []
                   | otherwise = [x]
limpiarLetra a (x:y:xs) | a == x = limpiarLetra a (y:xs)
                      | a == y = limpiarLetra a (x:xs)
                      | otherwise = [x,y] ++ (limpiarLetra a xs)

limpiar :: String -> String -> String
limpiar [] x = x
limpiar (a:xs) x = limpiar xs (limpiarLetra a x)

--b
longitud :: [Float] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

suma :: [Float] -> Float
suma [] = 0
suma (x:xs) = x + suma xs

promedio :: [Float] -> Float
promedio [] = 0
promedio xs = (suma xs) / fromIntegral(longitud xs)

arregloDiferencia :: [Float] -> Float -> [Float]
arregloDiferencia [] _ = []
arregloDiferencia (x:xs) y = (x-y):(arregloDiferencia xs y)

difPromedio :: [Float] -> [Float]
difPromedio [] = []
difPromedio xs = arregloDiferencia xs (promedio xs)

--c
todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales [x] = True
todosIguales (x:y:xs) = x == y && todosIguales (y:xs)

--Ej 5:
data AB a = Nil | Bin (AB a) a (AB a) deriving (Show)
--a
vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _ = False

--b
negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin i n d) = Bin (negacionAB i) (not n) (negacionAB d)

--c
productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin i n d) = n * productoAB i * productoAB d