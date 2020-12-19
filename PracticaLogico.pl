herramientasRequeridas(ordenarCuarto, [aspiradora(100), trapeador, plumero]).
herramientasRequeridas(limpiarTecho, [escoba, pala]).
herramientasRequeridas(cortarPasto, [bordedadora]).
herramientasRequeridas(limpiarBanio, [sopapa, trapeador]).
herramientasRequeridas(encerarPisos, [lustradpesora, cera, aspiradora(300)]).

tiene(egon,aspiradora(200)).
tiene(egon,trapeador).
tiene(peter,trapeador).
tiene(winston,varitaDeNeutrones).

tieneHerramientaRequerida(Integrante,Herramienta):-
    tiene(Integrante,Herramienta).

tieneHerramientaRequerida(Integrante,aspiradora(PotenciaNecesaria)):-
    tiene(Integrante,aspiradora(Potencia)),
    between(0,Potencia,PotenciaNecesaria).

