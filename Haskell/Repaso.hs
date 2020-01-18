module Repaso where

import           Data.Char
import           Data.Maybe

-- 1. Orden superior
----------------------------------------------------------------

-- |
-- >>> twice (+1) 5
-- 7
--
-- >>> twice (*2) 3
-- 12

twice :: (a -> a) -> a -> a
twice f x = f $ f x

-- |
-- >>> mapTuple (+1) (*2) (3, 4)
-- (4,8)
--
-- >>> mapTuple ord (==5) ('A', 3 + 1)
-- (65,False)

mapTuple :: (a->c) -> (b->d) -> (a,b) -> (c,d)
mapTuple f g (x,y) = (f x,g y)

-- >>> devuelvaFuncion 5
-- una funcion que suma 5
devuelvaFuncion :: Int -> (Int -> Int)
devuelvaFuncion n = \x -> x + n

-- map, filter
-- lambda expresiones
-- secciones

-- |
-- >>> aprobadoGeneral [1..10]
-- [5.0,5.0,5.0,5.0,5.0,6.0,7.0,8.0,9.0,10.0]
--
-- >>> aprobadoGeneral [4.7, 2.5, 7, 10, 8.7]
-- [5.0,5.0,7.0,10.0,8.7]

aprobadoGeneral :: [Double] -> [Double]
aprobadoGeneral xs = map (max 5) xs
--aprobadoGeneral xs = map (\x -> max x 5) xs
--aprobadoGeneral xs = map (\x -> if x < 5 then 5 else x) xs

-- >>> aprobados [4.7, 2.5, 7, 10, 8.7]
-- [7.0,10.0,8.7]
aprobados :: [Double] -> [Double]
--aprobados xs = filter (>=5) xs
aprobados xs = filter (5<=) xs

-- 2. Plegado de listas
----------------------------------------------------------------

-- revisión de la recursión sobre listas

-- |
-- >>> suma [1..10]
-- 55
--
-- >>> suma [7]
-- 7
--
-- >>> suma []
-- 0

suma :: Num a => [a] -> a
suma []     = 0
--suma (x:xs) = x + suma xs
suma (x:xs) = (+) x (suma xs)

-- |
-- >>> longitud "hola mundo"
-- 10
--
-- >>> longitud [True]
-- 1
--
-- >>> longitud []
-- 0

longitud :: [a] -> Integer
longitud []     = 0
--longitud (_:xs) = 1 + longitud xs
longitud (x:xs) = f x (longitud xs)
	where
		f _ solCol = 1 + solCol

-- |
-- >>> conjunción [1 == 1, 'a' < 'b', null []]
-- True
--
-- >>> conjunción [1 == 1, 'a' < 'b', null [[]]]
-- False
--
-- >>> conjunción []
-- True

conjunción :: [Bool] -> Bool
conjunción [] = True
--conjunción (x:xs) = x && conjunción xs
conjunción (x:xs) =  (&&) x (conjunción xs)

-- |
-- >>> esPalabra "haskell"
-- True
--
-- >>> esPalabra "haskell 2017"
-- False
--
-- >>> esPalabra "h"
-- True
--
-- >>> esPalabra ""
-- True

esPalabra :: String -> Bool
esPalabra "" = True
--esPalabra (x:xs) = isLetter x && esPalabra xs
esPalabra (x:xs) = f x (esPalabra xs)
	where
		f cabeza solCola = isLetter cabeza && solCola

-- |
-- >>> todasMayúsculas "WHILE"
-- True
--
-- >>> todasMayúsculas "While"
-- False
--
-- >>> todasMayúsculas ""
-- True

todasMayúsculas :: String -> Bool
todasMayúsculas "" = True
--todasMayúsculas (x:xs) = isUpper x && todasMayúsculas xs
todasMayúsculas (x:xs) = f x (todasMayúsculas xs)
	where 
		f cabeza solCola = isUpper cabeza && solCola

-- |
-- >>> máximo "hola mundo"
-- 'u'
--
-- >>> máximo [7, -8, 56, 17, 34, 12]
-- 56
--
-- >>> máximo [-8]
-- -8

máximo :: Ord a => [a] -> a
máximo [x] = x
máximo (x:xs) = max x $ máximo xs

-- |
-- >>> mínimoYmáximo "hola mundo"
-- (' ','u')
--
-- >>> mínimoYmáximo [7, -8, 56, 17, 34, 12]
-- (-8,56)
--
-- >>> mínimoYmáximo [1]
-- (1,1)

mínimoYmáximo :: Ord a => [a] -> (a,a)
mínimoYmáximo xs = undefined

-- |
-- >>> aplana [[1,2], [3,4,5], [], [6]]
-- [1,2,3,4,5,6]
--
-- >>> aplana [[1,2]]
-- [1,2]
--
-- >>> aplana []
-- []

aplana :: [[a]] -> [a]
aplana [] = []
--aplana (xs:xss) = xs ++ aplana xss
aplana (xs:xss) = (++) xs $ aplana xss


-- deducir el patrón de foldr
		
		--		f			base 					
--recLista :: (a -> b -> b) -> b -> [a] -> b
--recLista f base [] = base
--recLista f base (x:xs) = f x (recLista base xs)



-- resolver las anteriores funciones con foldr

-- |
-- >>> sumaR [1..10]
-- 55
--
-- >>> sumaR [7]
-- 7
--
-- >>> sumaR []
-- 0

sumaR :: Num a => [a] -> a
--sumaR xs = foldr (\cabeza solCola -> cabeza + solCola) 0 xs
sumaR xs = foldr (+) 0 xs

-- |
-- >>> longitudR "hola mundo"
-- 10
--
-- >>> longitudR [True]
-- 1
--
-- >>> longitudR []
-- 0

longitudR :: [a] -> Integer
longitudR xs = foldr (\_ solCola -> 1 + solCola) 0 xs

-- |
-- >>> conjunciónR [1 == 1, 'a' < 'b', null []]
-- True
--
-- >>> conjunciónR [1 == 1, 'a' < 'b', null [[]]]
-- False
--
-- >>> conjunciónR []
-- True

conjunciónR :: [Bool] -> Bool
conjunciónR xs = foldr (&&) True xs

-- |
-- >>> esPalabraR "haskell"
-- True
--
-- >>> esPalabraR "haskell 2017"
-- False
--
-- >>> esPalabraR "h"
-- True
--
-- >>> esPalabraR ""
-- True

esPalabraR :: String -> Bool
esPalabraR xs = foldr (\cabeza -> (&&) $ isLetter cabeza) True xs

-- |
-- >>> todasMayúsculasR "WHILE"
-- True
--
-- >>> todasMayúsculasR "While"
-- False
--
-- >>> todasMayúsculasR ""
-- True

todasMayúsculasR :: String -> Bool
todasMayúsculasR xs = undefined

-- |
-- >>> máximoR "hola mundo"
-- 'u'
--
-- >>> máximoR [7, -8, 56, 17, 34, 12]
-- 56
--
-- >>> máximoR [-8]
-- -8

-- Maybe y fromMaybe
máximoR :: Ord a => [a] -> a
--máximoR (x:xs) = foldr (max) x xs
máximoR xs = foldr1 max xs

-- |
-- >>> mínimoYmáximoR "hola mundo"
-- (' ','u')
--
-- >>> mínimoYmáximoR [7, -8, 56, 17, 34, 12]
-- (-8,56)
--
-- >>> mínimoYmáximoR [1]
-- (1,1)

mínimoYmáximoR :: Ord a => [a] -> (a,a)
mínimoYmáximoR xs = undefined

-- |
-- >>> aplanaR [[1,2], [3,4,5], [], [6]]
-- [1,2,3,4,5,6]
--
-- >>> aplanaR [[1,2]]
-- [1,2]
--
-- >>> aplanaR []
-- []

aplanaR :: [[a]] -> [a]
aplanaR xs = foldr (++) [] xs

-- otros ejercicios de foldr

-- |
-- >>> mapR (2^) [0..10]
-- [1,2,4,8,16,32,64,128,256,512,1024]
--
-- >>> mapR undefined []
-- []
--
-- >>> mapR ord  "A"
-- [65]

mapR :: (a -> b) -> [a] -> [b]
mapR f xs = undefined

-- |
-- >>> filter even [1..20]
-- [2,4,6,8,10,12,14,16,18,20]
--
-- >>> filter undefined []
-- []
--
-- >>> filter even [5]
-- []

filterR :: (a -> Bool) -> [a] -> [a]
filterR p xs = undefined

-- |
-- >>> apariciones 'a' "casa"
-- 2
-- >>> apariciones 'u' "casa"
-- 0

apariciones :: Eq a => a -> [a] -> Integer
apariciones x xs = undefined

-- |
-- >>> purgar "abracadabra"
-- "cdbra"
--
-- >>> purgar [1,2,3]
-- [1,2,3]
--
-- >>> purgar "aaaaaaaaaa"
-- "a"

purgar :: Eq a => [a] -> [a]
purgar xs = undefined

-- |
-- >>> agrupa "mississippi"
-- ["m","i","ss","i","ss","i","pp","i"]
--
-- >>> agrupa [1,2,2,3,3,3,4,4,4,4]
-- [[1],[2,2],[3,3,3],[4,4,4,4]]
--
-- >>> agrupa []
-- []

agrupa :: Eq a => [a] -> [[a]]
agrupa xs = undefined

-- 3. Plegado de tipos algebraicos recursivos
----------------------------------------------------------------

data Tree a = Empty
            | Leaf a
            | Node a (Tree a) (Tree a)
            deriving Show

treeI :: Tree Integer
treeI = Node 1
             (Node 2 (Leaf 4) (Leaf 5))
             (Node 3 Empty (Leaf 6))

treeC :: Tree Char
treeC = Node 'z'
          (Node 't' (Node 's' Empty (Leaf 'a')) (Leaf 'g'))
          (Node 'w' (Leaf 'h') (Node 'p' (Leaf 'f') (Leaf 'n')))

-- |
-- >>> treeSize treeI
-- 6
--
-- >>> treeSize treeC
-- 10

treeSize :: Tree a -> Integer
treeSize Empty = 0
treeSize (Leaf a) = 1
treeSize (Node n l r) = 1 + treeSize l + treeSize r

-- |
-- >>> treeHeight treeI
-- 3
-- >>> treeHeight treeC
-- 4

treeHeight :: Tree a -> Integer
treeHeight Empty = 0
treeHeight (Leaf a) = 1
treeHeight (Node n l r) = 1 + max (treeHeight l) (treeHeight r)

-- |
-- >>> treeSum treeI
-- 21

treeSum :: Num a => Tree a -> a
treeSum Empty = 0
treeSum (Leaf a) = id a
treeSum (Node n l r) = f n (treeSum l) (treeSum r)
	where
		f x sl sr = x + sl + sr

-- |
-- >>> treeProduct treeI
-- 720

treeProduct :: Num a => Tree a -> a
treeProduct = undefined

-- |
-- >>> treeElem 5 treeI
-- True
--
-- >>> treeElem 48 treeI
-- False
--
-- >> treeElem 'w' treeC
-- True
--
-- >>> treeElem '*' treeC
-- False

treeElem :: Eq a => a -> Tree a -> Bool
treeElem x Empty = False
treeElem x (Leaf y) = x == y
treeElem x (Node n l r) = f x (treeElem x l) (treeElem x r)
	where 
		f y sl sr = x == y || sl || sr
-- |
-- >>> treeToList treeI
-- [4,2,5,1,3,6]
--
-- >>> treeToList treeC
-- "satgzhwfpn"

treeToList :: Tree a -> [a]
treeToList Empty = []
treeToList (Leaf h) = [h]
treeToList (Node n l r) = treeToList l ++ n : treeToList r

-- |
-- >>> treeBorder treeI
-- [4,5,6]
--
-- >>> treeBorder treeC
-- "aghfn"

treeBorder :: Tree a -> [a]
treeBorder Empty = []
treeBorder (Leaf h) = [h]
treeBorder (Node n l r) = treeBorder l ++ treeBorder r

-- introducir el plegado del tipo Tree a

foldTree :: (a -> b -> b -> b) -> (a -> b) -> b -> Tree a -> b
foldTree f t b Empty = b
--Empty :: Tree a  --- Empty x e --- e :: b
foldTree f t b (Leaf h) = t h
--Leaf a :: a -> Tree a --- Leaf x l --- l :: a -> b
foldTree f t b (Node n l r) = f n (foldTree f t b l) (foldTree f t b r)
{-
Node a 	(Tree a) ::	a -> Tree a ------ f :: a -> b
		(Tree a)	a -> Tree a				  -> b 
					a -> Tree a				  -> b
-}

-- resolver los ejercicios anteriores con foldTree

-- |
-- >>> treeSize' treeI
-- 6
--
-- >>> treeSize' treeC
-- 10

treeSize' :: Tree a -> Integer
treeSize' = undefined

-- |
-- >>> treeHeight' treeI
-- 3
-- >>> treeHeight' treeC
-- 4

treeHeight' :: Tree a -> Integer
treeHeight' = undefined

-- |
-- >>> treeSum' treeI
-- 21

treeSum' :: Num a => Tree a -> a
treeSum' = foldTree f (id) 0
	where 
		f n sl sr = n + sl + sr

-- |
-- >>> treeProduct' treeI
-- 720

treeProduct' :: Num a => Tree a -> a
treeProduct' = foldTree f (id) 0
	where 
		f n sl sr = n * sl * sr

-- |
-- >>> treeElem' 5 treeI
-- True
--
-- >>> treeElem' 48 treeI
-- False
--
-- >> treeElem' 'w' treeC
-- True
--
-- >>> treeElem' '*' treeC
-- False

treeElem' :: Eq a => a -> Tree a -> Bool
treeElem' x t = foldTree f (x==) False t
	where
		f n sl sr = (n == x) || sl || sr

-- |
-- >>> treeToList' treeI
-- [4,2,5,1,3,6]
--
-- >>> treeToList' treeC
-- "satgzhwfpn"

treeToList' :: Tree a -> [a]
treeToList' = undefined

-- |
-- >>> treeBorder' treeI
-- [4,5,6]
--
-- >>> treeBorder' treeC
-- "aghfn"

treeBorder' :: Tree a -> [a]
treeBorder' = foldTree (const (++)) (:[]) []
--

treeMaximum :: Ord a => Tree a -> a
treeMaximum = undefined

treeMaximum' :: Ord a => Tree a -> a
treeMaximum' = undefined
