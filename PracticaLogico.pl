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


