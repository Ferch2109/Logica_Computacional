{-
- Pract 1: Introducción a Haskell
- Números binarios.
- Este programa brinda un tipo de datos que
- representa los números binarios.
- Define ciertos operadores sobre este tipo
- de datos.
- Profesora: Dra. Lourdes del Carmen González Huesca
- Ayudante: Roberto Monroy Argumedo
- Laboratorio: Fernando Abigail Galicia Mendoza
- Integrante: González Chávez María Fernanda
-			  313036367
-}


data BinarioPos = U | Cero BinarioPos | Uno BinarioPos

instance Show BinarioPos where
    show U = "1"
    show ( Cero x ) = ( show x ) ++ "0"
    show ( Uno x ) = ( show x ) ++ "1"

-- | sucesor. Función que recibe un tipo <BinarioPos>.
--  		  Devuelve el numero siguiente, i.e. el
--			  número mas 1.	
sucesor :: BinarioPos -> BinarioPos
sucesor U = Cero U
sucesor ( Cero x ) = Uno( sucesor x )
sucesor ( Uno x ) = Cero( suma x U )


-- | suma. Función que recibe dos tipos <BinarioPos>.
--  		  Devuelve la suma de ambos.
suma :: BinarioPos -> BinarioPos -> BinarioPos
suma U U = Cero U
suma U ( Cero x ) = Uno x
suma U ( Uno x ) = Cero( suma U x ) 
suma ( Cero x ) U = Uno x
suma ( Uno x ) U = Cero( suma U x )
suma ( Cero x ) ( Cero y ) = Cero( suma x y )
suma ( Uno x ) ( Uno y ) = Cero( suma ( suma U x ) y )
suma ( Cero x ) ( Uno y ) = Uno( suma x y )
suma ( Uno x ) ( Cero y ) = Uno( suma x y )

-- | producto. Función que recibe dos tipos <BinarioPos>.
--  		  Devuelve el producto de ambos.
producto :: BinarioPos -> BinarioPos -> BinarioPos
producto U U = U
producto U ( Cero x ) = Cero( producto U x )
producto U ( Uno x ) = Uno( producto U x )
producto ( Cero x ) U = Cero( producto U x )
producto ( Uno x ) U = Uno( producto U x )
producto ( Cero x ) ( Cero y ) = Cero( producto x (Cero y) )

producto ( Uno x ) ( Uno y ) = Uno( suma (producto U y ) (Cero( producto x y )) )

producto ( Cero x ) ( Uno y ) = Cero( producto x y )
producto ( Uno x ) ( Cero y ) = Cero( producto x y )
