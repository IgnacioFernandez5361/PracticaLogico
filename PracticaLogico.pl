atiende(dodain,lunes,9,15).
atiende(dodain,miercoles,9,15).
atiende(dodain,viernes,9,15).
atiende(lucas,martes,10,20).
atiende(juanC,sabado,18,22).
atiende(juanC,domingo,18,22).
atiende(juanFdS,jueves,10,20).
atiende(juanFdS,viernes,12,20).
atiende(leoC,lunes,14,18).
atiende(leoC,miercoles,14,18).
atiende(martu,miercoles,23,24).

atiende(vale,Dia,HoraInicial,HoraFinal):-atiende(dodain,Dia,HoraInicial,HoraFinal).
atiende(vale,Dia,HoraInicial,HoraFinal):-atiende(juanC,Dia,HoraInicial,HoraFinal).

quienAtiende(Persona,Dia,Hora):-
    atiende(Persona,Dia,HoraInicial,HoraFinal),
    Hora >= HoraInicial, 
    Hora =< HoraFinal.

estaForeverAlone(Persona,Dia,Hora):-
    quienAtiende(Persona,Dia,Hora),
    not((quienAtiende(Otre, Dia, Hora), Persona \= Otre)).

posibilidadesAtencion(Dia, Personas):-
  findall(Quien, atiende(Quien, Dia, _, _), Quienes),
  combinar(Quienes, Personas).

combinar([], []).
combinar([Posible|Posibles], [Posible|Personas]):-combinar(Posibles, Personas).
combinar([_|Posibles], Personas):-combinar(Posibles, Personas).

ventas(dodain, lunes, 10, 8, [golosinas(1200), cigarrillos(jockey), golosinas(50)]).
ventas(dodain, miercoles, 12, 8, [bebidas(true, 8), bebida(false, 1)]).
ventas(martu, miercoles, 12, 8, [golosinas(1000), cigarrillos(chesterfield, colorado, parisiennes)]).
ventas(lucas, martes, 11, 8, [golosinas(600)]).
ventas(lucas, martes, 18, 8, [bebidas(false, 2), cigarrillos(derby)]).

esSuertudo(Vendedor):-
    vendedor(Vendedor),
    forall(ventas(Vendedor,_,_,_,[PrimeraVenta|_]),diaDeVentaSuertudo(PrimeraVenta)).

vendedor(Persona):-distinct(Persona, ventas(Persona, _, _, _, _)).

diaDeVentaSuertudo(golosinas(PrecioDeVenta)):- PrecioDeVenta > 100.
diaDeVentaSuertudo(cigarrillos(Marcas)):-
    length(Marcas,CantidadDeMarcas),
    CantidadDeMarcas > 2.
diaDeVentaSuertudo(bebidas(true,_)).
diaDeVentaSuertudo(bebidas(_,CantidadDeBebidas)):- CantidadDeBebidas > 5.
