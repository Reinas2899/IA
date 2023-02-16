:- include('client.pl').
:- include('encomenda.pl').
:- include('estafeta.pl').
:- include('mapa.pl').
:- include('transport.pl').
:- use_module(library(lists)).
:- use_module(library(aggregate)).


% 1. identificar o estafeta que utilizou mais vezes um meio de transporte mais ecológico;

maisVerde(Est):-
  encomenda(_,_,Est,_,_,_,_,_,_,_,'bicicleta',1).

listaVerde(L):- findall(E,maisVerde(E),L).

estafetaMaisVerde(Est,C):-
listaVerde(L),
conta_max_elemento(Est,C,L).

% 2. identificar que estafetas entregaram determinada(s) encomenda(s) a um
%    determinado cliente;

verEstafeta(Enc,Cli,Est):-
    encomenda(Enc,_,Est,Cli,_,_,_,_,_,_,_,1).

% 3. identificar os clientes servidos por um determinado estafeta;

clientesServidosEstafetas(Est,Cli):-
    encomenda(_,_,Est,Cli,_,_,_,_,_,_,_,1).

% 4. calcular o valor faturado pela Green Distribution num determinado dia;

calculaPrecoEncomenda(Enc,C):-
  encomenda(Enc,_,_,Cli,_,_,_,_,_,_,T,1),
  cliente(Cli,Local),
  shortest(gualtar,Local,_,Len),
  transport(T,_,_,_,CustoKm),
  C is CustoKm * Len.


calculaPrecoEncomendaporDia(C,AA,MM,DD):-
  encomenda(Enc,_,_,_,_,AA,MM,DD,_,_,_,1),          
  calculaPrecoEncomenda(Enc,C).
  
calculaPrecoTodasEncomendasporDia(C,[]):- C is 0.
calculaPrecoTodasEncomendasporDia(C,[H|T]):-
  encomenda(H,_,_,_,_,AA,MM,DD,_,_,_,1),
  calculaPrecoEncomendaporDia(C1,AA,MM,DD),
  calculaPrecoTodasEncomendasporDia(C2,T),
  C is C1+C2.



totalFaturadoPorDia(C,AA,MM,DD):-
  listaEncDia(L,AA,MM,DD),
  calculaPrecoTodasEncomendasporDia(C,L).

listaEncDia(L,AA,MM,DD):- findall(Enc,restringeData(Enc,AA,MM,DD),L).

restringeData(Enc,AA,MM,DD):-
  encomenda(Enc,_,_,_,_,AA,MM,DD,_,_,_,1).

% 5. identificar quais as zonas (e.g., rua ou freguesia) com maior volume de
%    entregas por parte da Green Distribution;

calculaVolumeEncomendaLocal(Local,V):-
  listaLocal(L),
  conta_max_elemento(Local,V,L).

listaLocal(L):- findall(Local,restringeLocal(Local),L).

restringeLocal(Local):-
  encomenda(_,_,_,Cli,_,_,_,_,_,_,_,1),
  cliente(Cli,Local).

conta_elemento(X,N,L) :-
  aggregate(count,member(X,L),N).
conta_max_elemento(X,N,L) :-
  aggregate(max(N1),X1,conta_elemento(X1,N1,L),N),
  member(X,L),
  conta_elemento(X,N,L),
  !.


% 6. calcular a classificação média de satisfação de cliente para um determinado estafeta;


classificaEstafeta(Est,C):-
  estafeta(Est,L,_),
  media(L,C).

media( L, Media ):-
    sumlist( L, Sum ),
    length( L, Length),
    (  Length > 0
    -> Media is Sum / Length
    ;  Media is 0
    ).


% 7. identificar o número total de entregas pelos diferentes meios de transporte, num determinado intervalo de tempo;

verificaIntervaloTempo(AI,MI,DI,HI,MinI,AF,MF,DF,HF,MinF,AA,MM,DD,HH,Min, Bool):-
  ((AA=:=AI , AA=:=AF), (MM=:=MI,MM=:=MF), (DD=:=DF,DD=:=DI), (HH=:=HI,HH=:=HF), (Min>=MinI, Min=<MinF)->  Bool is 1;
  (AA>AI , AA<AF) -> Bool is 1;
  (AA<AI , AA>AF) -> Bool is 0;
  (AA=:=AI , AA=:=AF), (MM>MI,MM<MF) ->  Bool is 1;
  (AA=:=AI ,AA=:=AF), (MM<MI;MM>MF) ->  Bool is 0;
  (AA=:=AI ,AA=:=AF), (MM=:=MI,MM<MF), (DD>DI) ->  Bool is 1;
  (AA=:=AI ,AA=:=AF), (MM=:=MI,MM<MF), (DD<DI) ->  Bool is 0;
  (AA=:=AI ,AA=:=AF), (MM=:=MI,MM<MF), (DD=:=DI), (HH<HI) ->  Bool is 0;
  (AA=:=AI ,AA=:=AF), (MM=:=MI,MM<MF), (DD=:=DI), (HH>HI) ->  Bool is 1;
  (AA=:=AI ,AA=:=AF), (MM=:=MI,MM<MF), (DD=:=DI), (HH=:=HI), (Min>=MinI) ->  Bool is 1;
  (AA=:=AI ,AA=:=AF), (MM=:=MI,MM<MF), (DD=:=DI), (HH=:=HI), (Min<MinI) ->  Bool is 0;
  (AA=:=AI ,AA=:=AF), (MM=:=MI,MM=:=MF), (DD<DI;DD>DF) ->  Bool is 0;
  (AA=:=AI ,AA=:=AF), (MM=:=MI,MM=:=MF), (DD=:=DF,DD=:=DI), (HH>HI,HH<HF) ->  Bool is 1;
  (AA=:=AI ,AA=:=AF), (MM=:=MI,MM=:=MF), (DD=:=DF), (HH<HF) ->  Bool is 1;
  (AA=:=AI ,AA=:=AF), (MM=:=MI,MM=:=MF), (DD=:=DF), (HH>HF) ->  Bool is 0;
  (AA=:=AI ,AA=:=AF), (MM=:=MI,MM=:=MF), (DD=:=DI), (HH>HI) ->  Bool is 1;
  (AA=:=AI ,AA=:=AF), (MM=:=MI,MM=:=MF), (DD=:=DI), (HH<HI) ->  Bool is 0;
  (AA=:=AI ,AA=:=AF), (MM=:=MI;MM=:=MF), (DD=:=DF), (HH=:=HF), (Min=<MinF) ->  Bool is 1;
  (AA=:=AI ,AA=:=AF), (MM=:=MI;MM=:=MF), (DD=:=DF), (HH=:=HF), (Min>MinF) ->  Bool is 0;
  (AA=:=AI ,AA=:=AF), (MM>MI,MM=:=MF), (DD<DF) ->  Bool is 1;
  (AA=:=AI ,AA=:=AF), (MM>MI,MM=:=MF), (DD>DF) ->  Bool is 0;
  (AA=:=AI ,AA=:=AF), (MM>MI,MM=:=MF), (DD=:=DF), (HH>HF) ->  Bool is 0;
  (AA=:=AI ,AA=:=AF), (MM>MI,MM=:=MF), (DD=:=DF), (HH<HF) ->  Bool is 1;
  (AA=:=AI ,AA=:=AF), (MM>MI,MM=:=MF), (DD=:=DF), (HH=:=HF), (Min=<MinF) ->  Bool is 1;
  (AA=:=AI ,AA=:=AF), (MM>MI,MM=:=MF), (DD=:=DF), (HH=:=HF), (Min>MinF) ->  Bool is 0;
  (AA>AI , AA=:=AF), (MM<MF) ->  Bool is 1;
  (AA>AI , AA=:=AF), (MM>MF) ->  Bool is 0;
  (AA>AI , AA=:=AF), (MM=:=MF), (DD<DF) ->  Bool is 1;
  (AA>AI , AA=:=AF), (MM=:=MF), (DD>DF) ->  Bool is 0;
  (AA>AI , AA=:=AF), (MM=:=MF), (DD=:=DF), (HH>HF) ->  Bool is 0;
  (AA>AI , AA=:=AF), (MM=:=MF), (DD=:=DF), (HH<HF) ->  Bool is 1;
  (AA>AI , AA=:=AF), (MM=:=MF), (DD=:=DF), (HH=:=HF), (Min=<MinF) ->  Bool is 1;
  (AA>AI , AA=:=AF), (MM=:=MF), (DD=:=DF), (HH=:=HF), (Min>MinF) ->  Bool is 0;
  (AA=:=AI , AA<AF), (MM>MI) ->  Bool is 1;
  (AA=:=AI , AA<AF), (MM<MI) ->  Bool is 0;
  (AA=:=AI , AA<AF), (MM=:=MI), (DD=:=DI), (HH>HI) ->  Bool is 1;
  (AA=:=AI ,AA<AF), (MM=:=MI), (DD=:=DI), (HH<HI) ->  Bool is 0;
  (AA=:=AI ,AA<AF), (MM=:=MI), (DD=:=DI), (HH=:=HI), (Min>=MinI) ->  Bool is 1;
  (AA=:=AI ,AA<AF), (MM=:=MI), (DD=:=DI), (HH=:=HI), (Min<MinI) ->  Bool is 0;
  (AA=:=AI ,AA<AF), (MM=:=MI), (DD>DI) ->  Bool is 1;
  Bool is 0).
  

listaTrans(T,AI,MI,DI,HI,MinI,AF,MF,DF,HF,MinF,L,C):- 
  findall(Enc,verificaEntregasTransporteTempo(Enc,T,AI,MI,DI,HI,MinI,AF,MF,DF,HF,MinF),L), length(L,C).

verificaEntregasTransporteTempo(Enc,T,AI,MI,DI,HI,MinI,AF,MF,DF,HF,MinF):-
  encomenda(Enc,_,_,_,_,AA,MM,DD,HH,Min,T,1),
  verificaIntervaloTempo(AI,MI,DI,HI,MinI,AF,MF,DF,HF,MinF,AA,MM,DD,HH,Min, Bool),
  Bool =:= 1.

totalEntregasTrans(T,AI,MI,DI,HI,MinI,AF,MF,DF,HF,MinF,C):-
  listaTrans(T,AI,MI,DI,HI,MinI,AF,MF,DF,HF,MinF,_,C).


% 8. identificar o número total de entregas pelos estafetas, num determinado intervalo de tempo


listaEstTempo(Est,AI,MI,DI,HI,MinI,AF,MF,DF,HF,MinF,L,C):- findall(Est,verificaEntregasEstafetaTempo(Est,AI,MI,DI,HI,MinI,AF,MF,DF,HF,MinF),L), length(L,C).

verificaEntregasEstafetaTempo(Est,AI,MI,DI,HI,MinI,AF,MF,DF,HF,MinF):-
  encomenda(_,_,Est,_,_,AA,MM,DD,HH,Min,_,1),
  verificaIntervaloTempo(AI,MI,DI,HI,MinI,AF,MF,DF,HF,MinF,AA,MM,DD,HH,Min, Bool),
  Bool =:= 1.

totalEntregasEstTempo(Est,AI,MI,DI,HI,MinI,AF,MF,DF,HF,MinF,C):-
  listaEstTempo(Est,AI,MI,DI,HI,MinI,AF,MF,DF,HF,MinF,_,C).


% 9. calcular o número de encomendas entregues e não entregues pela Green Distribution, num determinado período de tempo;


verificaEntregasRealizadasTempo(Enc,AI,MI,DI,HI,MinI,AF,MF,DF,HF,MinF):-
  encomenda(Enc,_,_,_,_,AA,MM,DD,HH,Min,_,1),
  verificaIntervaloTempo(AI,MI,DI,HI,MinI,AF,MF,DF,HF,MinF,AA,MM,DD,HH,Min, Bool),
  Bool =:= 1.

verificaEntregasNaoRealizadasTempo(Enc,AI,MI,DI,HI,MinI,AF,MF,DF,HF,MinF):-
  encomenda(Enc,_,_,_,_,AA,MM,DD,HH,Min,_,0),
  verificaIntervaloTempo(AI,MI,DI,HI,MinI,AF,MF,DF,HF,MinF,AA,MM,DD,HH,Min, Bool),
  Bool =:= 1.


procuraRealiazadas(AI,MI,DI,HI,MinI,AF,MF,DF,HF,MinF,L,C):- 
  findall(Enc,verificaEntregasRealizadasTempo(Enc,AI,MI,DI,HI,MinI,AF,MF,DF,HF,MinF),L), length(L,C).

procuraNaoRealiazadas(AI,MI,DI,HI,MinI,AF,MF,DF,HF,MinF,L,C):- 
  findall(Enc,verificaEntregasNaoRealizadasTempo(Enc,AI,MI,DI,HI,MinI,AF,MF,DF,HF,MinF),L), length(L,C).


totalEntreguesNEntregues(E,NE,AI,MI,DI,HI,MinI,AF,MF,DF,HF,MinF):-
procuraRealiazadas(AI,MI,DI,HI,MinI,AF,MF,DF,HF,MinF,_,E),
procuraNaoRealiazadas(AI,MI,DI,HI,MinI,AF,MF,DF,HF,MinF,_,NE).


% 10. calcular o peso total transportado por estafeta num determinado dia.

calculaPesoDiaEst(Est,P,AA,MM,DD):-
  listaEstDia(L,Est,AA,MM,DD),
  sumlist(L,P).

listaEstDia(L,Est,AA,MM,DD):- findall(P,restringeEstData(Est,AA,MM,DD,P),L).


restringeEstData(Est,AA,MM,DD,P):-
  encomenda(_,P,Est,_,_,AA,MM,DD,_,_,_,_).

% MENU

menu :- repeat,
    write('                          '),nl,
    write('   1. Query 1             '),nl,
    write('   2. Query 2             '),nl,
    write('   3. Query 3             '),nl,
    write('   4. Query 4             '),nl,
    write('   5. Query 5             '),nl,
    write('   6. Query 6             '),nl,
    write('   7. Query 7             '),nl,
    write('   8. Query 8             '),nl,
    write('   9. Query 9             '),nl,
    write('  10. Query 10             '),nl,
    write('   0. Sair             '),nl,
    write('                          '),nl,
    write('Selecione a Query: '),nl,
    read(Choice), Choice>=0, Choice =<10,
    doit(Choice), Choice=10, !.

doit(1):-
    estafetaMaisVerde(Est,C),
    write('Estafeta = '), write(Est), nl,
    write('Numero de vezes = '), write(C),nl.
doit(2):-
    write('Encomenda : '), nl,
    read(Enc),
    write('Cliente : '),nl,
    read(Cli),
    verEstafeta(Enc,Cli,Est),
write('Estafeta = '), write(Est),nl.

doit(3):-
  write('Estafeta : '), nl,
  read(Est),
  clientesServidosEstafetas(Est,Cli),
  write('Cliente = '), write(Cli),nl.

doit(4):-
  write('Ano : '), nl,
  read(AA),
  write('Mes : '), nl,
  read(MM),
  write('Dia : '), nl,
  read(DD),
  totalFaturadoPorDia(C,AA,MM,DD),
  write('Total Faturado nesse dia: '), write(C),nl.

doit(5):-
    calculaVolumeEncomendaLocal(Local,V),
    write('Freguesia = '), write(Local), nl,
    write('Volume = '), write(V),nl.

doit(6):-
    write('Estafeta : '), nl,
    read(Est),
    classificaEstafeta(Est,C),
    write('Classificassão = '), write(C), nl.

  doit(7):-
    write('Transporte : '), nl,
    read(T),
    write('Ano Inicial : '), nl,
    read(AI),
    write('Mes Inicial : '), nl,
    read(MI),
    write('Dia Inicial : '), nl,
    read(DI),
    write('Hora Inicial : '), nl,
    read(HI),
    write('Minutos Iniciais : '), nl,
    read(MinI),
    write('Ano Final : '), nl,
    read(AF),
    write('Mes Final : '), nl,
    read(MF),
    write('Dia Final : '), nl,
    read(DF),
    write('Hora Final : '), nl,
    read(HF),
    write('Minutos Finais : '), nl,
    read(MinF),
    totalEntregasTrans(T,AI,MI,DI,HI,MinI,AF,MF,DF,HF,MinF,C),
    write('Total = '), write(C), nl.

  doit(8):-
    write('Estafeta : '), nl,
    read(Est),
    write('Ano Inicial : '), nl,
    read(AI),
    write('Mes Inicial : '), nl,
    read(MI),
    write('Dia Inicial : '), nl,
    read(DI),
    write('Hora Inicial : '), nl,
    read(HI),
    write('Minutos Iniciais : '), nl,
    read(MinI),
    write('Ano Final : '), nl,
    read(AF),
    write('Mes Final : '), nl,
    read(MF),
    write('Dia Final : '), nl,
    read(DF),
    write('Hora Final : '), nl,
    read(HF),
    write('Minutos Finais : '), nl,
    read(MinF),
    totalEntregasEstTempo(Est,AI,MI,DI,HI,MinI,AF,MF,DF,HF,MinF,C),
    write('Total = '), write(C), nl.

  doit(9):-
    write('Ano Inicial : '), nl,
    read(AI),
    write('Mes Inicial : '), nl,
    read(MI),
    write('Dia Inicial : '), nl,
    read(DI),
    write('Hora Inicial : '), nl,
    read(HI),
    write('Minutos Iniciais : '), nl,
    read(MinI),
    write('Ano Final : '), nl,
    read(AF),
    write('Mes Final : '), nl,
    read(MF),
    write('Dia Final : '), nl,
    read(DF),
    write('Hora Final : '), nl,
    read(HF),
    write('Minutos Finais : '), nl,
    read(MinF),
    totalEntreguesNEntregues(E,NE,AI,MI,DI,HI,MinI,AF,MF,DF,HF,MinF),
    write('Total Entregues = '), write(E), nl,
    write('Total Nao Entregues = '), write(NE), nl.

  doit(10):-
    write('Estafeta : '), nl,
    read(Est),
    write('Ano : '), nl,
    read(AA),
    write('Mes : '), nl,
    read(MM),
    write('Dia : '), nl,
    read(DD),
    calculaPesoDiaEst(Est,P,AA,MM,DD),
    write('Peso Total = '), write(P), nl.
  
doit(0):- abort.
