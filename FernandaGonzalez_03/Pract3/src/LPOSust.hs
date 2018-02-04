{-
 -Logica Computacional 2017-2
 -Tema : Implementacion de la sustitución.
 -Profesor: Lourdes del Carmen Gonzaléz Huesca
 -Ayudante: Roberto Monroy Argumedo
 -Laboratorio: Fernando A. Galicia Mendoza
-}
module LPOSust where

import LPO
import Data.List

-- | Subst. Tipo que representa una sustitución de variables en términos.
type Subst = [(Ind,Term)]

-- | elimRep. Función que elimina los elementos repetidos de una lista.
elimRep :: Eq a => [a] -> [a]
elimRep l = eR_aux l [] where
  eR_aux [] c = c
  eR_aux (x:xs) c
    | elem x c = eR_aux xs c
    | otherwise = eR_aux xs (c++[x])

-- | verifSus. Función que verifica una sustitución.
verifSus :: Subst -> Bool
verifSus s = elimRep [i | (i,t) <- s] == [i | (i,t) <- s]

-- | apsubTaux. Función auxiliar que aplica una sustitución de variables en términos
-- en un término.
apsubTaux :: Term -> Subst -> Term
apsubTaux (V x) sus = case sus of
  [] -> V x
  (s:ss) -> if fst s == x then snd s else apsubTaux (V x) ss
apsubTaux (F c []) _ = F c []
apsubTaux (F f ts) s = F f (apsubTL_aux ts s) where
  apsubTL_aux [] s = []
  apsubTL_aux (t:ts) s = ((apsubTaux t s):apsubTL_aux ts s)
  
-- | apsubT. Función que aplica una sustitución de variables de variables en términos
-- en un término dado.
apsubT :: Term -> Subst -> Term
apsubT t s = if verifSus s then apsubTaux t s else error "Sustitución no legal."

--Ejemplos:

--Considerese los siguientes terminos.
t1 = F "f" [V 1,F "c" [],V 3]
t2 = F "g" [t1,V 2]
t3 = F "d" []
t4 = F "h" [V 6,V 1]

--Se realizan las siguientes sustituciones validas.
sust1 = apsubT t2 [(2,t3),(1,t4)]
sust2 = apsubT t4 [(1,t1)]
sust3 = apsubT t2 [(1,t2)]

--La siguiente sustitución es inválida.
sustInv1 = apsubT t2 [(2,t3),(2,t4)]
sustInv2 = apsubT t3 [(1,V 1),(2,V 3),(1,V 4)]

ultima = 0
-- | apsubF. Función que recibe una fórmula y una sustitución.
--           Devuelve una fórmula con la sustitución ya aplicada.
apsubF :: Form -> Subst -> Form
apsubF TrueF s = TrueF
apsubF FalseF s = FalseF
apsubF (Pr p t) s = Pr p [ apsubT z s | z <- t ]
apsubF (Eq t1 t2) s = ( Eq (apsubT t1 s) (apsubT t2 s) )
apsubF (Neg f) s = (Neg (apsubF f s))
apsubF (Conj f1 f2) s = ( Conj (apsubF f1 s) (apsubF f2 s) )
apsubF (Disy f1 f2) s = ( Disy (apsubF f1 s) (apsubF f2 s) )
apsubF (Imp f1 f2) s = ( Imp (apsubF f1 s) (apsubF f2 s) )
apsubF (Equi f1 f2) s = ( Equi (apsubF f1 s) (apsubF f2 s) )
apsubF (All x f) s = (All y (apsubF (renombrarF f x y ) s)) where y = (maximum ((varF f)++[ultima]) )+1 where ultima = if ultima == y then y+1 else ultima
apsubF (Ex x f ) s = (Ex y (apsubF (renombrarF f x y ) s)) where y = (maximum ((varF f)++[ultima]) )+1 where ultima = if ultima == y then y+1 else ultima

-- AUX --
-- | renombrar. Función que recibe una fórmula, una variable ligada y
--				la variable por la que se sustituirá.
--				Devuelve una fórmula con su variable ligada renombrada
--				para poder aplicar una sustitución válida.
renombrarF :: Form -> Ind -> Ind -> Form
renombrarF TrueF x y = TrueF
renombrarF FalseF x y = FalseF
renombrarF (Pr p t) x y = Pr p ([ renombrarT ti x y | ti <- t ])
renombrarF (Eq t1 t2) x y = ( Eq (renombrarT t1 x y) (renombrarT t2 x y) )
renombrarF (Neg f) x y = ( Neg (renombrarF f x y) )
renombrarF (Conj f1 f2) x y = ( Conj (renombrarF f1 x y) (renombrarF f2 x y) )
renombrarF (Disy f1 f2) x y = ( Disy (renombrarF f1 x y) (renombrarF f2 x y) )
renombrarF (Imp f1 f2) x y = ( Imp (renombrarF f1 x y) (renombrarF f2 x y) )
renombrarF (Equi f1 f2) x y = ( Equi (renombrarF f1 x y) (renombrarF f2 x y) )
renombrarF (All z f) x y = (All y (renombrarF f x y) )
renombrarF (Ex z f ) x y = (Ex y (renombrarF f x y) )

-- AUX --
-- | renombrarT. Función que recibe un término, una variable ligada y 
--				 la variable por la que se sustituirá.
--				 Devuelve el término renombrado.
renombrarT :: Term -> Ind -> Ind -> Term
renombrarT (V x) t y = if x == t then V y else V x
renombrarT (F f ts) x y = (F f (renombrarT_aux ts x y)) where

renombrarT_aux [] _ _ = []
renombrarT_aux (t:ti) x y = union [renombrarT t x y] (renombrarT_aux ti x y)