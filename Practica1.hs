--2)
curry :: ((a,b) -> c) -> a -> b -> c
curry f x y = f (x,y)

uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f (x,y) = f x y

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


