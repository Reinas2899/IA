:- include('client.pl').
:- include('encomenda.pl').
:- include('estafeta.pl').
:- include('mapa.pl').
:- include('transport.pl').
:- use_module(library(lists)).
:- use_module(library(aggregate)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  PARTE 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

geraLocais([],[]):- !.
geraLocais([Enc|T],[Local|L]):-
  encomenda(Enc,_,_,Cli,_,_,_,_,_,_,_),
  cliente(Cli,Local),
  geraLocais(T,L),
  !.

geraCircuitosAEAux(Locais,Distancia,Circuito):-
percorreTodoCircuito(Locais,gualtar, Distancia,Circuito).

geraCircuitosAE(Enc,Distancia,Circuito):-
  geraLocais(Enc,Locais),
  remove_duplicates(Locais,LocaisL),
  geraCircuitosAEAux(LocaisL,Distancia,Circuito).

geraCircuitosProfundidadeIterativaAux(Locais,Distancia,Circuito):-
  percorreCircuitoProfundidadeIterativa(Locais,gualtar, Distancia,Circuito).
    
geraCircuitosProfundidadeIterativa(Enc,Distancia,Circuito):-
  geraLocais(Enc,Locais),
  remove_duplicates(Locais,LocaisL),
  geraCircuitosProfundidadeIterativaAux(LocaisL,Distancia,Circuito).


geraCircuitosLarguraAux(Locais,Distancia,Circuito):-
  percorreCircuitoLargura(Locais,gualtar, Distancia,Circuito).
    
geraCircuitosLargura(Enc,Distancia,Circuito):-
  geraLocais(Enc,Locais),
  remove_duplicates(Locais,LocaisL),
  geraCircuitosLarguraAux(LocaisL,Distancia,Circuito).

geraCircuitosGulosaAux(Locais,Distancia,Circuito):-
  percorreCircuitoGulosa(Locais,gualtar, Distancia,Circuito).
    
geraCircuitosGulosa(Enc,Distancia,Circuito):-
  geraLocais(Enc,Locais),
  remove_duplicates(Locais,LocaisL),
  geraCircuitosGulosaAux(LocaisL,Distancia,Circuito).


remove_duplicates([], []).
remove_duplicates([Head | Tail], Result) :-
    member(Head, Tail), !,
    remove_duplicates(Tail, Result).
remove_duplicates([Head | Tail], [Head | Result]) :-
    remove_duplicates(Tail, Result).

pesoTotal([],Peso):-
    Peso is 0,
    !.
pesoTotal([Enc|T],Peso):-
  encomenda(Enc,P,_,_,_,_,_,_,_,_,_),
  pesoTotal(T,PesoAux),
  Peso is P + PesoAux.


medeVelocidade(Transporte,Peso,Velocidade):-
  (Transporte == 'bicicleta' -> Velocidade is 10 - 0.7*Peso;
    Transporte == 'moto' -> Velocidade is 35 - 0.5*Peso;
      Transporte == 'carro' -> Velocidade is 25-0.1*Peso;
      Velocidade is 0).


escolheTransportePeso(Peso,Transporte,Bool):-
  ( Peso =< 5 -> Transporte = 'bicicleta';
     Peso =< 20 -> Transporte = 'moto';
       Peso =< 100 -> Transporte = 'carro';
      Bool is 0).


calculaTempoAE(Enc,Transporte,Velocidade,Tempo,Distancia):-
  pesoTotal(Enc,Peso),
  escolheTransportePeso(Peso,Transporte,_),
  medeVelocidade(Transporte,Peso,Velocidade),
  geraCircuitosAE(Enc,Distancia,_),
  calculaTempoAux(Distancia,Velocidade,Tempo).

calculaTempoProfundidade(Enc,Transporte,Velocidade,Tempo,Distancia):-
  pesoTotal(Enc,Peso),
  escolheTransportePeso(Peso,Transporte,_),
  medeVelocidade(Transporte,Peso,Velocidade),
  geraCircuitosProfundidade(Enc,_,Distancia),
  calculaTempoAux(Distancia,Velocidade,Tempo).

calculaTempoProfundidadeIterativa(Enc,Transporte,Velocidade,Tempo,Distancia):-
  pesoTotal(Enc,Peso),
  escolheTransportePeso(Peso,Transporte,_),
  medeVelocidade(Transporte,Peso,Velocidade),
  geraCircuitosProfundidadeIterativa(Enc,_,Distancia),
  calculaTempoAux(Distancia,Velocidade,Tempo).

calculaTempoGulosa(Enc,Transporte,Velocidade,Tempo,Distancia):-
  pesoTotal(Enc,Peso),
  escolheTransportePeso(Peso,Transporte,_),
  medeVelocidade(Transporte,Peso,Velocidade),
  geraCircuitosGulosa(Enc,Distancia,_),
  calculaTempoAux(Distancia,Velocidade,Tempo).

calculaTempoLargura(Enc,Transporte,Velocidade,Tempo,Distancia):-
  pesoTotal(Enc,Peso),
  escolheTransportePeso(Peso,Transporte,_),
  medeVelocidade(Transporte,Peso,Velocidade),
  geraCircuitosLargura(Enc,_,Distancia),
  calculaTempoAux(Distancia,Velocidade,Tempo).


calculaTempoAux(Distancia,Velocidade,Tempo):-
  Tempo is Distancia/Velocidade.


entregaMaisEcologicaPossivel(Enc,Transporte,Tempo,Distancia):-
  encomenda(Enc,_,_,_,_,_,_,_,_,_,Prazo),
  pesoTotal([Enc],Peso),
  geraCircuitosAE([Enc],Distancia,_),
  escolheTransportePrazo(Peso,Distancia,Prazo,Tempo,Transporte).


  
  
escolheTransportePrazo(Peso,Distancia,Prazo,Tempo,Transporte):-
  ( Peso =< 5, medeVelocidade('bicicleta',Peso,Velocidade), calculaTempoAux(Distancia,Velocidade,Tempo), Tempo =< Prazo -> Transporte = 'bicicleta';
     Peso =< 20, medeVelocidade('moto',Peso,Velocidade), calculaTempoAux(Distancia,Velocidade,Tempo),Tempo =< Prazo -> Transporte = 'moto';
        Peso =< 100, medeVelocidade('carro',Peso,Velocidade), calculaTempoAux(Distancia,Velocidade,Tempo),Tempo =< Prazo -> Transporte = 'carro';
          Transporte = impossivel).
  

entregaMaisRapidaPossivel(Enc,Transporte,Tempo,Distancia):-
  encomenda(Enc,_,_,_,_,_,_,_,_,_,Prazo),
  pesoTotal([Enc],Peso),
  geraCircuitosAE([Enc],Distancia,_),
  escolheTransportePrazo(Peso,Distancia,Prazo,_,TransporteAux),
  (TransporteAux == 'bicicleta' -> Transporte = 'moto', medeVelocidade('moto',Peso,Velocidade), calculaTempoAux(Distancia,Velocidade,Tempo);
    TransporteAux == 'moto' -> Transporte = 'moto', medeVelocidade('moto',Peso,Velocidade), calculaTempoAux(Distancia,Velocidade,Tempo);
      Transporte = 'carro' , medeVelocidade('carro',Peso,Velocidade), calculaTempoAux(Distancia,Velocidade,Tempo)).




buscaTodasEnc(Est,EncL):-
  estafeta(Est,_,EncL).

calculaTodosCircuitosAE([],_,_):- !.
calculaTodosCircuitosAE(Enc,Circuitos,Distancia):-
  geraCircuitosAE(Enc,Circuitos,Distancia).


calculaAE(Est,C,D):-
  buscaTodasEnc(Est,EncL),
  calculaTodosCircuitosAE(EncL,C,D).

geraCircuitosProfundidadeAux(Locais,Circuito,Distancia):-
  percorreCircuitoProfundidade(Locais, gualtar, Circuito, Distancia).
  
geraCircuitosProfundidade(Enc,Circuito,Distancia):-
  geraLocais(Enc,Locais),
  remove_duplicates(Locais,LocaisL),
  geraCircuitosProfundidadeAux(LocaisL,Circuito,Distancia).


calculaTodosCircuitosProfundidade(Enc,Circuito,Distancia):-
  geraCircuitosProfundidade(Enc,Circuito,Distancia).


calculaProfundidade(Est,C,D):-
  buscaTodasEnc(Est,EncL),
  calculaTodosCircuitosProfundidade(EncL,C,D).




calculaTodosCircuitosProfundidadeIterativa([],_,_):- !.
calculaTodosCircuitosProfundidadeIterativa(Enc,Circuitos,Distancia):-
  geraCircuitosProfundidadeIterativa(Enc,Circuitos,Distancia).


calculaProfundidadeIterativa(Est,C,D):-
  buscaTodasEnc(Est,EncL),
  calculaTodosCircuitosProfundidadeIterativa(EncL,Caminho,D),
  append([gualtar],Caminho,C).




calculaTodosCircuitosLargura([],_,_):- !.
calculaTodosCircuitosLargura(Enc,Circuitos,Distancia):-
  geraCircuitosLargura(Enc,Circuitos,Distancia).


calculaLargura(Est,C,D):-
  buscaTodasEnc(Est,EncL),
  calculaTodosCircuitosLargura(EncL,Caminho,D),
  append([gualtar],Caminho,C).




calculaTodosCircuitosGulosa([],_,_):- !.
calculaTodosCircuitosGulosa(Enc,Circuitos,Distancia):-
  geraCircuitosGulosa(Enc,Circuitos,Distancia).


calculaGulosa(Est,C,D):-
  buscaTodasEnc(Est,EncL),
  calculaTodosCircuitosGulosa(EncL,C,D).


% MENU

menu :- repeat,
    write('                                     '),nl,
    write('   1. BFS                            '),nl,
    write('   2. DFS                            '),nl,
    write('   3. A*                             '),nl,
    write('   4. Greddy                         '),nl,
    write('   5. Iterative                      '),nl,
    write('   6. Calcula Tempo(Ecologico)       '),nl,
    write('   7. Calcula Mais Rápido            '),nl,
    write('   0. Sair                           '),nl,
    write('                                     '),nl,
    write('Selecione a Query: '),nl,
    read(Choice), Choice>=0, Choice =<7,
    doit(Choice), Choice=7, !.

doit(1):-
    write('Estafeta : '), nl,
    read(Est),
    calculaLargura(Est,C,D),
    write('Caminho = '), write(C), nl,
    write('Distancia = '), write(D),nl.
doit(2):-
    write('Estafeta : '), nl,
    read(Est),
    calculaProfundidade(Est,C,D),
    write('Caminho = '), write(C), nl,
    write('Distancia = '), write(D),nl.
doit(3):-
    write('Estafeta : '), nl,
    read(Est),
    calculaAE(Est,C,D),
    write('Caminho = '), write(C), nl,
    write('Distancia = '), write(D),nl.
doit(4):-
    write('Estafeta : '), nl,
    read(Est),
    calculaGulosa(Est,C,D),
    write('Caminho = '), write(C), nl,
    write('Distancia = '), write(D),nl.
doit(5):-
    write('Estafeta : '), nl,
    read(Est),
    calculaProfundidadeIterativa(Est,C,D),
    write('Caminho = '), write(C), nl,
    write('Distancia = '), write(D),nl.
  doit(6):-
    write('Encomenda : '), nl,
    read(Enc),
    entregaMaisEcologicaPossivel(Enc,C,D,Dis),
    write('Transporte = '), write(C), nl,
    write('Tempo = '), write(D),nl,
    write('Distancia = '), write(Dis),nl.
  doit(7):-
    write('Encomenda : '), nl,
    read(Enc),
    entregaMaisRapidaPossivel(Enc,C,D,Dis),
    write('Transporte = '), write(C), nl,
    write('Tempo = '), write(D),nl,
    write('Distancia = '), write(Dis),nl.
      
  
doit(0):- abort.
