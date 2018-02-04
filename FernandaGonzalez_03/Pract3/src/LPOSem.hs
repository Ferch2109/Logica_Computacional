{-
 -Logica Computacional 2017-2
 -Tema : Implementacion de la sustitución.
 -Profesor: Lourdes del Carmen Gonzaléz Huesca
 -Ayudante: Roberto Monroy Argumedo
 -Laboratorio: Fernando A. Galicia Mendoza
 -Integrantes:
 -				González Chávez María Fernanda
 -				Sanchez Pérez Juan Pedro Salvador
-}
module LPOSem where

import LPO

import Data.List

-- | IntF. Tipo que representa una interpretación de funciones.
type IntF a = Nombre -> [a] -> a

-- | IntR. Tipo que representa una interpretación de relaciones (predicados).
type IntR a = Nombre -> [a] -> Bool

-- | Estado. Tipo que representa el estado de una variable del universo.
type Estado a = Ind -> a

-- | Mundo. Tipo que representa un mundo.
type Mundo a = (Estado a, IntF a, IntR a)


-- | actEstados. Función que recibe un <Estado> a, una variable <Ind> x, y
--				 un elemento del universo.
--				 Devuelve la actualización de estados.
actEstados :: Estado a -> Ind -> a -> Estado a
actEstados e x v y
	| x == y = v
	| otherwise = e x

-- | iTerm. Función que recibe un término, un estado y una función de 
--			interpretación de términos.
--			Devuelve la interpretación del término respecto al estado dado.
iTerm :: Term -> Estado a -> IntF a -> a
iTerm (V x) e i = e x
iTerm (F c []) e i = i c []
iTerm (F f ts) e i = i f (aux e i ts) where
						aux _ _ [] = []
						aux e i (t : ts) = (iTerm t e i):(aux e i ts)

-- | iForm. Función que recibe una fórmula, un mundo y una función de
--			interpretación de fórmulas.
--			Devuelve la interpretación de la función respecto al mundo.
iForm :: Eq a => [a] -> Mundo a -> Form -> Bool
iForm v (e, iF, iR) TrueF = True
iForm v (e, iF, iR) FalseF = False
iForm v (e, iF, iR) (Pr p t) = and([ iForm v (e, iF, iR) (concat(varT t)) ])
iForm v (e, iF, iR) (Eq t1 t2) = ( (iTerm t1 e iF ) == (iTerm t2 e iF ) )
iForm v (e, iF, iR) (Neg f) = not(iForm v (e, iF, iR) f)
iForm v (e, iF, iR) (Conj f1 f2) = or ( [iForm v (e, iF, iR) f1, iForm v (e, iF, iR) f2] )
iForm v (e, iF, iR) (Imp f1 f2) = and( [iForm v (e, iF, iR) f1]++[iForm v (e, iF, iR) f2])

