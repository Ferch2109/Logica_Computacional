{-
- Pract 1: Introducción a Haskell
- Fórmula general de segundo grado.
- Este programa indica si las raices de
- un polinomio (en IR ) de segundo grado son reales <True>
- o no <False> a traves de la función --raizReal--.
- Profesora: Dra. Lourdes del Carmen González Huesca
- Ayudante: Roberto Monroy Argumedo
- Laboratorio: Fernando Abigail Galicia Mendoza
- Integrante: González Chávez María Fernanda
-			  313036367
-}

-- | C. Tipo de dato que representa los números complejos.
type C = ( Double, Double )

-- | P2. Tipo que representa los polinomios de segundo grado
--		 en el campo de los números reales:
--		 ( a, b, c ) representa ax²+bx+c.
type P2 = ( Double, Double, Double )

-- | R. Tipo que representa las 2 posibles raíces de un
--		polinomio de segundo grado.
type R = ( C, C )

-- | <Principal>
-- | raizReal. Función que recibe un polinomio de segundo
--			   grado <P2>. Devuelve <True> en caso de que sus
--			   2 raíces sean reales, <False> en otro caso.
-- | fst. Función que recibe una tupla. Devuelve el primer valor.
-- | snd. Función que recibe una tupla. Devuelve el segundo valor.
raizReal :: P2 -> Bool
raizReal ( a, b, c ) = if snd( fst( raices( a, b, c ) ) ) /= 0 || snd( snd( raices( a, b, c ) ) ) /= 0  then False else True

-- | raices. Función que recibe un polinomio de segundo grado
--			 <P2>. Devuelve las dos posibles raíces <R>
--			 utilizando la ecuación general de segundo grado.
raices :: P2 -> R
raices ( a, b, c ) = if  ( b^2 - 4*a*c ) < 0 then ( ( (-b)/2*a , raizPos( a, b, c ) ), ( (-b)/2*a , raizNeg( a, b, c ) ) ) else ( ( (-b)/2*a + raizPos( a, b, c ), 0 ), ( (-b)/2*a + raizNeg( a, b, c ) , 0 ) )

-- | raizPos. Función que recibe un polinomio de segundo grado
--			  <P2>. Devuelve la raiz <Double> positiva.
raizPos :: P2 -> Double
raizPos ( a, b, c ) = ( ( sqrt( b^2 - 4*a*c ) ) / 2*a )

-- | raizNeg. Función que recibe un polinomio de segundo grado
--			  <P2>. Devuelve la raiz <Double> negativa.
-- | sqrt. Función que recibe un número. Devuelve la raiz.
raizNeg :: P2 -> Double
raizNeg ( a, b, c ) = ( -( sqrt( b^2 - 4*a*c ) ) / 2*a )