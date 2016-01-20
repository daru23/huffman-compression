-- Daniela Ruiz 06-40273
-- Diego Millan 03-36193

module Huffman
(Huffman, codificar, decodificar, reconstruirHuffman, generarAsociaciones)

where

--import 
import HeapBinary
import Data.List
------------------- Estructura de Datos Arboles de Huffman ---------------------
data Huffman a = Hoja Integer a 
                 | Rama Integer (Huffman a) (Huffman a)
                 deriving (Show, Eq, Ord)
------------------------------- FUNCIONES --------------------------------------

-- Devuelve la frecuencua acumulada que se encuentra en la raiz del arbol
obtenerFrecuencia :: Huffman a -> Integer
obtenerFrecuencia h = frecuencia h

-- Crea un nuevo arbol de Huffman a partir de dos arboles de Huffman
crearRama :: Huffman a -> Huffman a -> Huffman a
crearRama a1 a2 = Rama n a1 a2
                  where n = frecuencia a1 + frecuencia a2


-- Crea un arbol de Huffma para los elementos de la lista
crearHuffman :: (Ord a) => [(a, Integer)] -> Huffman a
crearHuffman x = (crearFinal . crearHuffman_aux. huffman_aux2) x

-- Devuelve una lista de tuplas con cada simbolo ocurrente en el arbol junto 
-- con su codificacion binaria asociada 
generarAsociaciones :: Huffman a -> [(a,[Bool])]
generarAsociaciones x = generar_aux [] x

-- Devuelve un Arbol de Huffman en base a la lista de tuplas las cuales 
-- tiene un simbolo y una codificacion binaria asociada
-- reconstruirHuffman :: [(a,[Bool])] -> Huffman a
reconstruirHuffman :: (Eq a) => [(a,[Bool])] -> Huffman a
reconstruirHuffman [(el,[True])] = Hoja 0 el
reconstruirHuffman l             = Rama 0 (reconstruirHuffman (conseguirIzq l)) (reconstruirHuffman (conseguirDer l))
                                
-- Dada una lista devuelve una tupla correspondiente al Arbol de Huffman asociado 
-- y a la codificacino de la entrada 
codificar :: (Eq a, Ord a) => [a] -> (Huffman a, [Bool])
codificar simb = 
                ( arb , (lista_asoc . secuencias simb . generarAsociaciones) arb)
                where 
                     arb = (crearHuffman . contar_frecuencias) simb

-- Dado un Arbol de Huffman y una lista de Booleanos devuelve la lista de 
-- simbolos que corresponde a la codificacion de dicha informacion 
decodificar :: Huffman a -> [Bool] -> [a]
decodificar h [] = []
decodificar h l  = fst cam : decodificar h (drop (snd cam + 1) l)
                         where
                             cam = camino l h 0

--------------------------- FUNCIONES AUXILIARES -------------------------------
-- Extrae la frecuencia asociada a cada Hoja o Rama 
frecuencia :: Huffman a -> Integer
frecuencia (Hoja n x  )  = n
frecuencia (Rama n x y) = n

-- Funcion auxiliar que recibe una tupla y devuelve una Hoja
crearHojas :: (a, Integer) -> Huffman a
crearHojas (a, n) = Hoja n a

-- Funcion auxiliar que crea las hojas correspondientes a cada tupla
huffman_aux :: [(a, Integer)] -> [Huffman a]
huffman_aux x = map crearHojas x

-- Funcion auxiliar que recibe una lista de tuplas y devuelve una arbol de huffma
-- utilizando el modulo HeapBinary 
huffman_aux2 :: (Ord a) => [(a, Integer)] -> [Huffman a]
huffman_aux2 x = (toList . fromList . huffman_aux) x

-- Funcion auxiliar que recibe una lista de arboles y devuelve una lista con 
-- un solo argol
crearHuffman_aux :: (Ord a) => [Huffman a] -> [Huffman a]
crearHuffman_aux [] = []
crearHuffman_aux x  
  | (length x)==1 = x  
  | otherwise = crearHuffman_aux ((toList . fromList) (crearRama (x !! 0) (x !! 1) : (drop 2 x)))

-- Funcion auxiliar que recive una lista de un elemento Arbol Huffman y devuelve 
-- el Arbol Huffman. Estilo flatten
crearFinal :: [Huffman a] -> Huffman a
crearFinal [x] = x 

-- Funcion auxiliar que recibe una lista de Booleanos y un Arbol de Huffman y devuelve 
-- una lista con tuplas donde el primer elemento es el valor de una hoja y el 
-- segundo elemento es la codificacion de elemento correspondiente en el Arbol 
generar_aux :: [Bool] -> Huffman a -> [(a,[Bool])]
generar_aux b (Hoja n a)       = [(a, b++[True])]
generar_aux b (Rama n izq der) = generar_aux (b++[False]) izq ++ generar_aux (b++[True]) der

-- Funcion auxiliar que recibe una lista de tuplas (elemento, codificacion) y 
-- una lista de tuplas (elemento, codificacion)
conseguirIzq :: [(a,[Bool])] -> [(a,[Bool])] 
conseguirIzq = fst . descomponer

-- Funcion auxiliar que recibe una lista de tuplas (elemento, codificacion) y 
-- una lista de tuplas (elemento, codificacion)
conseguirDer :: [(a,[Bool])] -> [(a,[Bool])] 
conseguirDer = snd . descomponer

-- Funcion auxiliar que recibe una lista de tuplas (elemento, codificacion) y 
-- consigue la asociacion correspondiente entre dos tuplas, es decir, consigue
-- si dos tuplas son hermanos
descomponer :: [(a,[Bool])] -> ([(a,[Bool])],[(a,[Bool])])
descomponer l = foldl' desc_aux ([],[]) l

-- Funcion auxiliar que dada una tupla de tuplas (elemento, codificacion) y 
-- una una tupla (elemento, codificacion) devuelve la asociacion correspondiente
-- es decir, si son hermanos
desc_aux :: ([(a,[Bool])],[(a,[Bool])]) -> (a,[Bool]) -> ([(a,[Bool])],[(a,[Bool])])
desc_aux (lIzq, lDer) (el,direccion:tail) 
                        | direccion = (lIzq,(el,tail):lDer)
                        | otherwise = ((el,tail):lIzq,lDer)

-- Funcion auxiliar que dada una lista devuelve una lista con tuplas de la forma
-- caracter, frecuencia
contar_frecuencias :: (Eq a) => [a] -> [(a,Integer)]
contar_frecuencias []     = []
contar_frecuencias (x:xs) = buscar 1 x xs : contar_frecuencias (filter (/= x) (x:xs))

-- Busca las ocurrencias del parametro dos en la la lista recibida y devuelve
-- una tupla con el elemento y el total de ocurrencias 
buscar :: (Eq a) => Integer -> a -> [a] -> (a,Integer)
buscar n x []  = (x,n)
buscar n x l
               | x == (head l)    = buscar (n+1) x (tail l)
               | otherwise        = buscar n x (tail l)  

-- Dado una lista devuelve la secuencia de codificaciones correspondientes 
-- al arbol de huffman
secuencias :: (Eq a) => [a] -> [(a,[Bool])] -> [(a,[Bool])]
secuencias [] l = []
secuencias x l  = secuencia (head x) l : secuencias (tail x) l 

-- Funcion auxiliar que dado un caracter y una lista de tuplas (elemento, codificacion),
-- devuelve la secuencia asociada al caracter
secuencia :: (Eq a) => a -> [(a,[Bool])] -> (a,[Bool])
secuencia x b
             | x == (fst(head b)) = (head b)
             | otherwise     = secuencia x (tail b)
    
-- Funcion auxiliar que dada una lista de (elemento, codificacion) deveulve una lista
-- donde concatena todas las codificaciones asociadas
lista_asoc :: [(a,[Bool])] -> [Bool]
lista_asoc [] = []
lista_asoc l  = concatMap snd l

-- Funcion auxiliar que recibe una lista de codificacion y una Arbol de Huffman
-- y un contador y devuelve la letra asociada al priemr camino o codificacion
-- que tiene la lista
camino :: [Bool] -> Huffman a -> Int -> (a,Int)
camino b (Hoja x a)   n  =  (a,n)                            
camino b (Rama x izq der)  n 
                             | (head b) == False = camino (tail b) izq (n+1)
                             | otherwise         = camino (tail b) der (n+1)
--------------------------------------------------------------------------------
