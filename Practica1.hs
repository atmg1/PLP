--2)
{-
curry :: ((a,b) -> c) -> a -> b -> c
curry f x y = f (x,y)

uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f (x,y) = f x y
-}

--CurryN no se puede definir con lo que vimos hasta ahora

--3)
--I)
sumMio :: (Num a) => [a] -> a
sumMio = foldr (+) 0

elemMio :: (Eq a) => a -> [a] -> Bool
elemMio e = foldr (\x rec -> x == e || rec) False

masmasMio :: [a] -> [a] -> [a]
--masmasMio xs ys = foldr (:) ys xs
masmasMio = flip (foldr (:))

filterMio :: (a -> Bool) -> [a] -> [a]
filterMio p = foldr (\x rec -> if p x then x:rec else rec) []

mapMio :: (a -> b) -> [a] -> [b]
mapMio f = foldr (\x rec -> f x : rec) []

--III)

{-
f :: (Num a) => a -> [a] -> [a]
f x xs = x:(map (+x) xs) 
f x = (:) x . map (+x)

sumasParcialesEx :: (Num a) => [a] -> [a]
sumasParcialesEx [] = []
sumasParcialesEx (x:xs) = f x (sumasParcialesEx xs)
-}

sumasParciales :: (Num a) => [a] -> [a]
sumasParciales = foldr (\x rec -> x:(map (+x) rec)) []
--sumasParciales = foldr (\x -> (:) x . map (+x)) []

--IV)

--f :: (Num a) => a -> a -> a
--f x sa = x - sa
--sumaAlt :: (Num a) => [a] -> a
--sumaAlt [] = 0
--sumaAlt (x:xs) = f x (sumaAlt xs)

sumaAlt :: (Num a) => [a] -> a
sumaAlt = foldr (-) 0

--V)
{-
f :: (Num a) => a -> [a] -> a -> a
f x xs rec = (-1)^(length xs) * x + rec

sumaAltInverso :: (Num a) => [a] -> a
sumaAltInverso [] = 0
sumaAltInverso (x:xs) = f x xs (sumaAltInverso xs)

Dado
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f z [] = z
recr f z (x:xs) = f x xs (recr f z xs)

sumaAltInverso :: (Num a) => [a] -> a
sumaAltInverso = recr (\x xs rec -> (-1)^(length xs) * x + rec) 0
-}

--f :: (Num a) => a -> a -> a
--f ac x = x - ac

--iterative :: (Num a) => a -> [a] -> a
--iterative ac [] = ac
--iterative ac (x:xs) = iterative (f ac x) xs

iterative ac xs = foldl (\ac x -> x - ac) ac xs

sumaAltInverso :: (Num a) => [a] -> a
sumaAltInverso xs = iterative 0 xs

--4)
--I)

permutarHasta :: Int -> a -> [a] -> [[a]]
permutarHasta n e xs = concatMap (\x -> [take x xs ++ [e] ++ drop x xs]) [0..n]

permutarUnaLista :: a -> [a] -> [[a]]
permutarUnaLista x xs = permutarHasta (length xs) x xs

fRecEstruct1 :: a -> [[a]] -> [[a]]
fRecEstruct1 x rec = concatMap (permutarUnaLista x) rec

--permutaciones :: [a] -> [[a]]
--permutaciones [] = [[]]
--permutaciones (x:xs) = f x (permutaciones xs)

permutaciones :: [a] -> [[a]]
permutaciones = foldr fRecEstruct1 [[]]

--II)

--fRecEstruct2 :: a -> [[a]] -> [[a]]
--fRecEstruct2 x rec = (map ((:) x) rec) ++ rec 

--partes :: [a] -> [[a]]
--partes [] = [[]]
--partes (x:xs) = fRecEstruct2 x (partes xs)

partes :: [a] -> [[a]]
partes = foldr (\x rec -> (map ((:) x) rec) ++ rec) [[]]

--III)

--fRecEstruct3 :: a -> [[a]] -> [[a]]
--fRecEstruct3 x rec = []:(map ((:) x) rec)

--prefijos :: [a] -> [[a]]
--prefijos [] = [[]]
--prefijos (x:xs) =  fRecEstruct3 x (prefijos xs)

prefijos :: [a] -> [[a]]
prefijos = foldr (\x rec -> []:(map ((:) x) rec)) [[]]

--IV)

sublistas :: [a] -> [[a]]
sublistas [] = [[]]
sublistas (x:xs) = (tail (prefijos (x:xs))) ++ sublistas xs

--5)
{-
Considerar las funciones
I)
elementosEnPosicionesPares [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs
                                    then [x]
else x : elementosEnPosicionesPares (tail xs)

La recursion no es estructural, el valor del caso recursivo esta en no esta en funcion de x y el llamado recursivo con la cola, sino que es una funcion de xs y el llamado recursivo con tail xs

II)
entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys -> if null ys
                           then x:entrelazar xs []
                           else x:head ys:entrelazar xs (tail ys)

Si es estructural, aca esta con foldr
-}

entrelazar :: [a] -> [a] -> [a]
entrelazar = foldr (\x rec -> \ys -> 
                    if null ys
                    then x:(rec [])
                    else x:head ys:(rec (tail ys))
                    ) id

--6)
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x:xs) = f x xs (recr f z xs)

--a)
sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e = recr (\x xs rec -> if e == x then xs else x : rec) []

{-b) Nose, consultar
sacarUna _ [] = []
sacarUna e (x:xs) = if e == x then xs else x : sacarUna e xs
-}

--c)
insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado e = recr (\x xs rec -> if x > e then e:x:xs else x : rec) [e]

--7) Definir las siguientes funciones para trabajar sobre listas y dar su tipo. Todas ellas deben poder aplicarse a listas finitas e infinitas

--I) mapPares, una version de map que toma una funcion currificada de dos argumentos y una lista de pares de valores y devuelve la lista de aplicaciones de la funcion a cada par. Pista: recordar curry y uncurry
mapPares :: (a -> b -> c) -> [(a,b)] -> [c]
mapPares f xs = map (uncurry f) xs

--II) armarPares, que dadas dos listas arma una lista de pares que contiene en cada posicion, el elemento correspondiente a esa posicion en cada una de las listas. Si una de las listas es mas larga que la otra, ignorar los elementos que sobran (el resultado tendra la longitud de la lista mas corta). Esta funcion en Hakell se llama zip. Pista: aprovechar la currificacion y utilizar evaluacion parcial

armarPares :: [a] -> ([b] -> [(a,b)])
--armarPares [] = const []
--armarPares (x:xs) = (\ys -> if null ys then [] else (x, head ys): armarPares xs (tail ys))
armarPares = foldr (\x rec -> 
                    \ys -> 
                    if null ys then [] 
                    else (x, head ys):rec (tail ys)) (const [])

{- 
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f z [] = z
recr f z (x:xs) = f x xs (recr f z xs)

Redefinir foldr en terminos de recr:

--redef :: (a -> b -> b) -> (a -> [a] -> b -> b)
--redef f x xs rec = f x rec 

--foldr f = recr (redef f)

Redefinir recr en terminos de foldr:
-}
