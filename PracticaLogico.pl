mago(harry,mestiza,[coraje,orgullo,inteligencia,amistad]).
mago(draco,pura,[inteligencia,orgullo]).
mago(hermione,impura,[inteligencia,orgullo,responsabilidad]).

odiariaEstarEn(harry,slytherin).
odiariaEstarEn(draco,hufflepuff).

casa(Casa):-importanteParaCasa(Casa,_).

importanteParaCasa(gryffindor,[coraje]).
importanteParaCasa(slytherin,[orgullo,inteligencia]).
importanteParaCasa(hufflepuff,[amistad]).
importanteParaCasa(ravenclaw,[inteligencia,responsabilidad]).

permiteEntrarAMago(Mago,slytherin):-
    mago(Mago,Sangre,_),
    Sangre \= impura.

permiteEntrarAMago(Mago,Casa):-
    mago(Mago,_,_),
    Casa \= slytherin.

caracterApropiadoParaLaCasa(Mago,Casa):-
    mago(Mago,_,Caracteristicas),
    importanteParaCasa(Casa,CaracterBuscado),
    forall(member(Caracteristica, CaracterBuscado),member(Caracteristica,Caracteristicas)).

puedeQuedarEn(hermione,gryffindor).
puedeQuedarEn(Mago,Casa):-
    caracterApropiadoParaLaCasa(Mago,Casa),
    permiteEntrarAMago(Mago,Casa),
    not(odiariaEstarEn(Mago,Casa)).

cadenaDeAmistades(Magos):-
    forall((member(Mago,Magos),mago(Mago,_,Caracteristicas)),member(amistad,Caracteristicas)),
    puedenRelacionarse(Magos).

puedenRelacionarse([_]).
puedenRelacionarse([Mago1,Mago2|OtrosMagos]):-
    puedenQuedarJuntos(Mago1,Mago2),
    puedenRelacionarse([Mago2|OtrosMagos]).

puedenQuedarJuntos(Mago1,Mago2):-
    puedeQuedarEn(Mago1,Casa),
    puedeQuedarEn(Mago2,Casa).