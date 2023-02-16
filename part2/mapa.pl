move(gualtar,nogueiro,4).
move(gualtar,s_vitor,3).
move(gualtar,s_pedro,2).
move(s_vitor,s_vicente,4).
move(s_vitor,nogueiro,3).
move(s_vitor,nogueira,9).
move(s_vitor,s_lazaro,3).
move(s_pedro,nogueiro,3).
move(s_pedro,espinho,5).
move(espinho,nogueiro,5).
move(nogueiro,nogueira,10).
move(s_vicente,real,3).
move(s_vicente,maximinos,5).
move(s_vicente,s_lazaro,6).
move(real,maximinos,3).
move(real,ferreiros,3).
move(maximinos,s_lazaro,2).
move(maximinos,ferreiros,1).
move(maximinos,lomar,3).
move(ferreiros,lomar,3).
move(s_lazaro,lomar,3).
move(s_lazaro,nogueira,13).
move(lomar,nogueira,15).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Pesquisa A* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


path(A,B,Path,Len) :-
       travel(A,B,[A],Q,Len),
       reverse(Q,Path).

travel(A,B,P,[B|P],L) :-
       adjacente(A,B,L).
travel(A,B,Visited,Path,L) :-
       adjacente(A,C,D),
       C \== B,
       \+member(C,Visited),
       travel(C,B,[C|Visited],Path,L1),
       L is D+L1.

pesquisaA(A,B,Path,Length) :-
   setof([P,L],path(A,B,P,L),Set),
   Set = [_|_],
   minimal(Set,[Path,Length]).

minimal([F|R],M) :- min(R,F,M).


min([],M,M).
min([[P,L]|R],[_,M],Min) :- L < M, !, min(R,[P,L],Min).
min([_|R],M,Min) :- min(R,M,Min).


shortestAll(_, [], C, E,CustoMenor, EncomendaMProx):- 
       CustoMenor = C,
       EncomendaMProx = E,
       !.
shortestAll(Source, [H|L], Custo, Encomenda,CustoMenor, EncomendaMProx):-
       pesquisaA(Source,H,_,C1),
       ((C1 >= Custo) -> shortestAll(Source, L, Custo, Encomenda,CustoMenor, EncomendaMProx);
       shortestAll(Source, L, C1, H,CustoMenor, EncomendaMProx)).

percorreCircuito([],_, _, _) :- !.       
percorreCircuito([H|L],Source, CustoMenor, EncomendaMProx) :-
       pesquisaA(Source,H,_,Custo),
       shortestAll(Source, L, Custo,H,CustoMenor, EncomendaMProx).
     
percorreTodoCircuito([],Source, CustoTotal,P) :- 
       pesquisaA(Source,gualtar,P,L),
       CustoTotal is L,
       !.  
percorreTodoCircuito([H|L],Source, CustoTotal,[Source|Vs]) :-
       percorreCircuito([H|L],Source, CustoMenor, EncomendaMProx),
       remover(EncomendaMProx,[H|L],Lista),
       percorreTodoCircuito(Lista,EncomendaMProx, CustoAux,Vs),
       CustoTotal is CustoAux + CustoMenor,
       !.


remover( _, [], []).
remover( R, [R|T], T).
remover( R, [H|T], [H|T2]) :- H \= R, remover( R, T, T2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Profundidade %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resolve_pp_c(Nodo,Destino,Path, C) :-
       profundidadeprimeiro(Nodo,Destino, [Nodo], Caminho, C),
       reverse(Caminho,Path),
       !.

profundidadeprimeiro(Nodo,Destino,Historico, [Destino|Historico], C) :-
       adjacente(Nodo,Destino,C).
profundidadeprimeiro(Nodo,Destino, Historico, Caminho, C) :-
	adjacente(Nodo, ProxNodo, C1),
       ProxNodo \== Destino,
       \+ member(ProxNodo, Historico),
       profundidadeprimeiro(ProxNodo, Destino, [ProxNodo|Historico], Caminho, C2), 
       C is C1 + C2.	

adjacente(Nodo, ProxNodo, C) :- 
	move(Nodo, ProxNodo, C).
adjacente(Nodo, ProxNodo, C) :- 
	move(ProxNodo, Nodo, C).

percorreCircuitoProfundidade([],Source, Caminho, Distancia) :- 
       resolve_pp_c(Source,gualtar,[_|Caminho],Distancia), 
       !.       
percorreCircuitoProfundidade([H|L],Source, Circuito, DistanciaTotal) :-
       resolve_pp_c(Source,H,Caminho,Distancia),
       append(Caminho,CircuitoAux,Circuito),   
       percorreCircuitoProfundidade(L,H, [C|CircuitoAux], DistanciaAux),
       DistanciaTotal is Distancia + DistanciaAux.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% BFS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

percorreCircuitoLargura([],Source, Caminho, Distancia) :- 
       larguraPrimeiro([[Source]],gualtar,[_|Caminho],Distancia), 
       !.       
percorreCircuitoLargura([H|L],Source, Circuito, DistanciaTotal) :-
       larguraPrimeiro([[Source]],H,[_|Caminho],Distancia),
       append(Caminho,CircuitoAux,Circuito),   
       percorreCircuitoLargura(L,H, CircuitoAux, DistanciaAux),
       DistanciaTotal is Distancia + DistanciaAux.


percorreL([Node|Path],NewPaths) :-
       findall([NewNode,Node|Path],
               (adjacente(Node,NewNode,_), 
               \+ member(NewNode,Path)),
               NewPaths).
     
larguraPrimeiro(Queue,Goal,Path,Distancia) :-
       larguraPrimeiroAux(Queue,Goal,Caminho),
       reverse(Caminho,Path),
       calculaDistancia(Path,Distancia),
       !.
          
larguraPrimeiroAux([[Goal|Path]|_],Goal,[Goal|Path]).
larguraPrimeiroAux([Path|Queue],Goal,FinalPath) :-
    percorreL(Path,NewPaths), 
    append(Queue,NewPaths,NewQueue),
    larguraPrimeiroAux(NewQueue,Goal,FinalPath).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Gulosa %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

percorreCircuitoGulosa([],Source, Distancia,Circuito) :-
       resolve_gulosa(Source, gualtar, Caminho, Distancia),
       append(Caminho, [gualtar], Circuito),
       !.
percorreCircuitoGulosa([H|L], Source, Distancia, Circuito) :-
       resolve_gulosa(Source, H, Caminho, Custo),
       append(Caminho, CaminhoAux, Circuito),
       percorreCircuitoGulosa(L, H, CustoAux, CaminhoAux),
       Distancia is Custo + CustoAux.

resolve_gulosa(Nodo,Destino,Path, C) :-
       gulosa(Nodo,Destino, [Nodo], [_ | Caminho], C),
       reverse(Caminho,Path),
       !.

gulosa(Nodo,Destino,Historico, [Destino|Historico], C) :-
       adjacente(Nodo,Destino,C).
gulosa(Nodo,Destino, Historico, Caminho, C) :-
       nodosAdjacentes(Nodo,Next),
       naoMembro(Next, Historico, ProxNodos),
       proxMaisCurto(ProxNodos,ProxNodo,Custo),
       gulosa(ProxNodo, Destino, [ProxNodo|Historico], Caminho, C2), 
       C is Custo + C2.	

nodosAdjacentes(Source,ProxNodos):-
       findall((C,ProxNodo),adjacente(Source,ProxNodo,C),ProxNodos).

proxMaisCurto(Next,ProxNodo,Custo):-
       msort(Next,[(H,X)|_]),
       ProxNodo = X,
       Custo = H.

naoMembro([], _, []):-!.
naoMembro([(X,H)|L], Historico, ProxNodos) :-
       (member(H,Historico) -> naoMembro(L, Historico, ProxNodos);
       append(ProxNodosAux,[(X,H)],ProxNodos),
       append(Historico,[H],HistoricoAux),
       naoMembro(L, HistoricoAux, ProxNodosAux)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Busca iterativa limitada em profundidade  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


percorreCircuitoProfundidadeIterativa([],Source, Caminho, Distancia) :- 
       iterativa_profundidade([[Source]],gualtar,[_|Caminho],Distancia), 
       !.       
percorreCircuitoProfundidadeIterativa([H|L],Source, Circuito, DistanciaTotal) :-
       iterativa_profundidade([[Source]],H,[_|Caminho],Distancia),
       append(Caminho,CircuitoAux,Circuito),   
       percorreCircuitoProfundidadeIterativa(L,H, CircuitoAux, DistanciaAux),
       DistanciaTotal is Distancia + DistanciaAux.




iterativa_profundidade(Queue,Goal,Path,Distancia) :-
       iterativa_profundidadeAux(1,Queue,Goal,Caminho),
       reverse(Caminho,Path),
       calculaDistancia(Path,Distancia),
       !.
   
iterativa_profundidadeAux(Depth,Queue,Goal,Path) :- 
       profundidadeLimitada(Depth,Queue,Goal,Path).
iterativa_profundidadeAux(Depth,Queue,Goal,Path) :- 
       Depth1 is Depth+1,
       iterativa_profundidadeAux(Depth1,Queue,Goal,Path).


profundidadeLimitada(_,[[Goal|Path]|_],Goal,[Goal|Path]).
profundidadeLimitada(Depth,[Path|Queue],Goal,FinalPath) :-
    percorre(Depth,Path,NewPaths),
    append(NewPaths,Queue,NewQueue),
    profundidadeLimitada(Depth,NewQueue,Goal,FinalPath).

percorre(Depth,[Node|Path],NewPaths) :-
    length(Path,Len),
    Len < Depth-1, !,
    findall([NewNode,Node|Path],adjacente(Node,NewNode,_),NewPaths).
percorre(_,_,[]).

calculaDistancia([_],Distancia):-
       Distancia is 0,
       !.
calculaDistancia([H,Hs|T],Distancia):-
       Aux = Hs,
       adjacente(H,Hs,C),
       calculaDistancia([Aux|T],DistanciaAux),
       Distancia is DistanciaAux + C.

