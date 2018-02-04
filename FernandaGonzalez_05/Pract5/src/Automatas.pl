/**
  * Lógica computacional 2017-2
  * Tema : Operadores de corte.
  * Profesora: Lourdes del Carmen Gonzaléz Huesca
  * Ayudante: Roberto Monroy Argumedo
  * Laboratorio: Fernando A. Galicia Mendoza
  * Integrantes: 
    - Gonzalez Chavez Maria Fernanda
    - Sanchez Perez Pedro Juan salador
  **/

%%PROGRAMA	

%%Estado inicial S=q0
inicial(q0).

%%Estado final F={q3}
final(q3).


/**
  * @arg nat. Lista de simbolos.
  * Relación indica si X es una lista de símbolos aceptados 
  * por el autómata finito determinista.
  **/
afd(X) :- inicial(S), tExt(S,X).


/**
  * @arg nat. Estado origen de la trancisión
  * @arg nat. Estado siguiente de la trancisión.
  * Relación que determina si a partir del estado Q es posible
  * llegar al estado final (q3) a través de la cadena l.
  **/
tExt(Q, []) :- final(Q).
tExt(Q, [X|XS] ) :- delta(Q,X,QD), tExt(QD, XS).


/**
  * @arg nat. Estado de partida de la trancisión
  * @arg nat. Símbolo de entrada de la trancisión epsilon.
  * Relación que indica el estado QN del que se llega
  * a partir del estado Q con el simbolo X
  **/
delta(q0, a, q1).
delta(q1, b, q2).
delta(q2, c, q1).
delta(q2, d, q3).
delta(q3, e, q0).

/***********************************************************************************************/

/**
  * @arg nat. Lista de simbolos.
  * Relación indica si X es una lista de símbolos aceptados 
  * por el autómata finito no determinista.
  **/
afn(X) :- inicial(S),trans(S,X).


/**
  * @arg nat. Estado origen de la trancisión
  * @arg nat. Estado siguiente de la trancisión.
  * Relación que determina si a partir del estado Q es posible
  * llegar al estado final (q3) a través de la cadena l.
  **/
trans(Q, []) :- final(Q).
trans(Q, [X|XS] ) :- deltanfda(Q,X,QN), trans(QN, XS).


/**
  * @arg nat. Estado de partida de la trancisión
  * @arg nat. Símbolo de entrada de la trancisión epsilon.
  * Relación que indica el estado QN del que se llega
  * a partir del estado Q con el simbolo X
  **/
deltanfda(q0, 0, q0).
deltanfda(q0, 1, q0).
deltanfda(q0, 1, q1).
deltanfda(q1, 0, q2).
deltanfda(q2, 1, q3).
