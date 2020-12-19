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

accionRealizada(harry,andarFueraDeLaCama,-50).
accionRealizada(harry,bosqueProhibido,-50).
accionRealizada(harry,tercerPiso,-75).
accionRealizada(hermione,tercerPiso,-75).
accionRealizada(hermione,seccionRestringidaDeLaBibilioteca,-10).
accionRealizada(ron,ganarAjedrezMagico,50).
accionRealizada(hermione,salvarASusAmigos,50).
accionRealizada(harry,ganarleAVoldemort,60).

accionRealizada(Mago,pregunta,PuntajeConseguido):- 
preguntaRespondida(Mago,_,Puntaje,Profesor),
puntajeSegunProfesor(Profesor,Puntaje,PuntajeConseguido).

puntajeSegunProfesor(snape,Puntaje,PuntajeConseguido):- PuntajeConseguido is Puntaje / 2.
puntajeSegunProfesor(Profesor,Puntaje,Puntaje):- Profesor \= snape.

esDe(hermione, gryffindor).
esDe(ron, gryffindor).
esDe(harry, gryffindor).
esDe(draco, slytherin).
esDe(luna, ravenclaw).

esBuenAlumno(Mago):-
    accionRealizada(Mago,_,PuntosConseguidos),
    PuntosConseguidos > 0,
    forall(accionRealizada(Mago,_,Puntos), Puntos > 0).

accionRecurrente(Accion):-
    accionRealizada(_,Accion,_),
    findall(Mago,accionRealizada(Mago,Accion,_),MagosQueLaRealizaron),
    length(MagosQueLaRealizaron,CantidadDeOcurrencias),
    CantidadDeOcurrencias > 1.

puntajeConseguidoDeUnAlumno(Alumno,PuntajeTotal):-
    findall(Puntaje,accionRealizada(Alumno,_,Puntaje),Puntajes),
    sum_list(Puntajes, PuntajeTotal).

puntajePorAlumnoDeCasa(Casa,Puntaje):-
    esDe(Mago,Casa),
    puntajeConseguidoDeUnAlumno(Mago,Puntaje).

puntajeDeLaCasa(Casa,Puntaje):-
    findall(PuntajeDeMago,puntajePorAlumnoDeCasa(Casa,PuntajeDeMago),PuntajePorMagoDeLaCasa),
    sum_list(PuntajePorMagoDeLaCasa,Puntaje).

casaGanadora(Casa):-
    casa(Casa),
    puntajeDeLaCasa(Casa,Puntaje),
    forall(puntajeDeLaCasa(_,OtroPuntaje),Puntaje > OtroPuntaje).

preguntaRespondida(hermione,locacionDeBezoar,20,snape).
preguntaRespondida(hermione,levitarPluma,25,flitwick).