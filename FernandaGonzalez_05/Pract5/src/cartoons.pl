/**
  * Lógica computacional 2017-2
  * Tema : Redes Semánticas.
  * Profesora: Lourdes del Carmen Gonzaléz Huesca
  * Ayudante: Roberto Monroy Argumedo
  * Laboratorio: Fernando A. Galicia Mendoza
  **/

/*Relación que indica los colores posibles*/
color(rojo).
color(verde).
color(amarillo).
color(morado).
color(azul).

/*Relación que indica los personajes posibles*/
personaje(bugsbunny).
personaje(steven).
personaje(star).
personaje(mickeymouse).
personaje(batman).

/*Relación que indica las armas posibles*/
arma(espada).
arma(pistola).
arma(sable).
arma(rayos).
arma(puños).

/*Relación que indica las bebidas posibles*/
bebida(cocacola).
bebida(whisky).
bebida(chocolate).
bebida(leche).
bebida(agua).

/*Relación que indica las mascotas posibles*/
mascota(leon).
mascota(caracoles).
mascota(zorro).
mascota(caballo).
mascota(perro).


/**Relación que indica que una casa es una tupla de cinco elementos
  *de la forma (color,personaje,arma,bebida,mascota)
  **/
%%casa( (_C,_P,_A,_B,_M) ).
/*casa(rojo,bugsbunny,_,_,_).
casa(verde,_,_,whisky,_).
casa(amarillo,_,pistola,_,_).

casa(_,star,_,chocolate,_).
casa(_,_,espada,_,caracoles).
casa(_,steven,_,_,leon).
casa(_,_,rayos,cocacola,_).
casa(_,batman,puños,_,_).  */


/*Relación que indica si la casa X esta alado de la casa Y*/
alado((verde,_,_,_,_),(morado,_,_,_,_)).
alado(X,Y) :-vive(P,X), atacacon(P,sable), vive(P2,Y), tiene(P2,zorro).
	

/*Relación que determina si X vive en la casa Y*/
vive(X,(amarillo,_,pistola,_,_)) :- atacacon(X,pistola).
vive(bugsbunny, (rojo,_,_,_,_)).
vive(mickeymouse,X) :- alado(X,(azul,_,_,_,_))-
/*vive(mickeymouse, primera(X)) :- casa(X).*/

/*Relación que determina si X tiene a Y como mascota.*/
tiene(X,caracoles) :- atacacon(X, espada).
tiene(steven,leon).

/*Relación que determina si X bebe Y*/
bebe(X,whisky) :- vive(X,(verde,_,_,_,_)).
bebe(star,chocolate).
bebe(X,cocacola) :- atacacon(X,rayos).
bebe(X, leche) :- not(vive(X,(verde,_,_,_,_))),
bebe(X,agua) :- not(bebe(X,whisky)),not(bebe(X,chocolate)),not(bebe(X,cocacola)), not(bebe(X,leche)).


/*Relación que determina si el personaje X ataca con el arma Y*/
atacacon(X,pistola) :- personaje(X), vive(X,Y), alado(Y,Z), tiene(Z,caballo).
atacacon(batman,puños).




