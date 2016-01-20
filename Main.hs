-- Daniela Ruiz 06-40273
-- Diego Millan 03-36193


module Main (main) where


import Data.List
import System
import System.IO 
import CompresorDeArchivos

--main :: [Char]
main =
    do 
        arg <- getArgs
        if (length arg) == 2 then leerDeArchivo (arg !! 0)
                                    --print m
--                let n = imprimirArchivo (arg !! 0) m 
            else print "ERROR: faltan argumentos"

