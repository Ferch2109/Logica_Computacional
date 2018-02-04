{-
- Pract 1: Introducción a Haskell
- El cifrado de César.
- Este programa implementa el cifrado de César,
- se recibe una palabra y se devuelve cifrada.
- Profesora: Dra. Lourdes del Carmen González Huesca
- Ayudante: Roberto Monroy Argumedo
- Laboratorio: Fernando Abigail Galicia Mendoza
- Integrante: González Chávez María Fernanda
-			  313036367
-}
import Data.Char

abc = ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z']
-- | alf. Función que recibe una letra mayúscula del alfabeto latino.
--		  Devuelve el entero correspondiente a su posición en el 
--		  alfabeto, en caso de que la letra sea minúscula devuelve
--		  error.
alf :: Char -> Int
alf x = if isUpper x then indice x 0 else error "Sólo mayúsculas."


-- | indice. Función que recibe una letra mayúscula del alfabeto
--			 latino <x> y un indice <n>. Devuelve el entero 
--			 correspondiente a la posición de la letra en el alfabeto.
indice :: Char -> Int -> Int
indice x n = if x == abc!!n then 65+n else indice x n+1

-- | iCesar. Función que recibe un entero que representa la posición
--			 de una letra de acuerdo al alfabeto latino.
--			 Devuelve la posición de acuerdo a la función hash 
--			 descrita por Suetonio.
iCesar :: Int -> Int
iCesar i = if i-3 < 65 then i+25-3 else i-3


-- | <Principal>
-- | cesar. Función que recibe una palabra <l>.
--  		Devuelve el cifrado César de la palabra. 
-- | chr. Función que recibe el número relacionado a una letra en el
--		  alfabeto latino.
--		  Devuelve dicha letra.
cesar :: String -> String
cesar [] = []
cesar l = [ chr( iCesar( alf( toUpper a ) ) ) | a <- l]

