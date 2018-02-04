{-
 -Logica Computacional 2017-2
 -Tema : Expresiones aritméticas
 -Profesor: Lourdes del Carmen Gonzaléz Huesca
 -Ayudante: Roberto Monroy Argumedo
 -Laboratorio: Fernando A. Galicia Mendoza
 -Integrantes:
 				González Chávez María Fernanda
 				Sanchez Perez Juan Salvador Pedro
-}

module EA where

import Data.List

-- | Sust. Tipo que representa una sustitución de variables de expresiones.
type Sust = (String,EA)

-- | State. Tipo que representa el estado de una variable en la memoria de
--			de la máquina. (tipo función)
type State = String -> Int

-- | Ind. Tipo que representa las variables.
type Ind = String

-- | Numb. Tipo que representa los numeros.
type Numb = Int

-- | EA. Tipo que representa una expresión aritmética.
data EA  = N Numb | V Ind | Sum EA EA | Prod EA EA | Let EA EA EA deriving (Show)


-- | sust. Función que realiza una sustitución, recibe un elemento e1 tipo <EA> y
--		   una sustitución (x,e2).
--		   Devuelve e1[x:=e2]
sust :: EA -> Sust -> EA
sust e (s,ea) = if s `elem` (fv e) then ( sust_aux e (s, ea) )  
                else error "No es posible hacer la sustitución."

-- | eval. Función que realiza una evaluación dado una expresión artimética,
--		   y un estado.
--		   Devuelve el entero resultante de la evaluación.
eval :: EA -> State -> Int
eval (N n) _ = n
eval (V x) s = s(x)
eval (Sum e1 e2) s = (eval e1 s) + (eval e2 s)
eval (Prod e1 e2) s = (eval e1 s) * (eval e2 s)
eval (Let (V x) e1 e2) s = eval (elimLet (Let (V x) e1 e2) s) s


--AUXILIAR
-- | sust_aux. Función que recibe una expresión aritméica y una sustitución.
--             Devuelve la sustitución aplicada a la expresión aritmética.
sust_aux :: EA -> Sust -> EA
sust_aux (N n) _ = (N n)
sust_aux (V x) (s,e) = e
sust_aux (Sum e1 e2) s = Sum (sust_aux e1 s ) (sust_aux e2 s ) 
sust_aux (Prod e1 e2) s = Prod (sust_aux e1 s ) (sust_aux e2 s ) 
sust_aux (Let (V x) e1 e2) s = Let (V x) (sust_aux e1 s ) (sust_aux e2 s ) 

-- AUXILIAR
-- | elimLet. Función recibe una expresión aritmética y un estado.
--            Elimina la expresión "let" de tal forma que la expresión e dada
--            como parámetro sea equivalente a elimLet(e).
elimLet :: EA -> State -> EA
elimLet (Let (V x) e1 e2) s = let ev_e1 = (eval e1 s)
                                in sust e2 (x, N ev_e1)
-- AUXILIAR
-- | fv. Función que recibe una expresión aritmética.
--       Devuelve el conjunto de variables libres de dicha expresión.
fv :: EA -> [Ind]
fv (N n) = []
fv (V x) = [x]
fv (Sum e1 e2) = union (fv e1) (fv e2)
fv (Prod e1 e2) = union (fv e1) (fv e2)
fv (Let (V x) e1 e2) = union (fv e1) (fv e2)