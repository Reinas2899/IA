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




connected(X,Y,L) :- move(X,Y,L) ; move(Y,X,L).

path(A,B,Path,Len) :-
       travel(A,B,[A],Q,Len),
       reverse(Q,Path).

travel(A,B,P,[B|P],L) :-
       connected(A,B,L).
travel(A,B,Visited,Path,L) :-
       connected(A,C,D),
       C \== B,
       \+member(C,Visited),
       travel(C,B,[C|Visited],Path,L1),
       L is D+L1.

shortest(A,B,Path,Length) :-
   setof([P,L],path(A,B,P,L),Set),
   Set = [_|_], % fail if empty
   minimal(Set,[Path,Length]).

minimal([F|R],M) :- min(R,F,M).

% minimal path
min([],M,M).
min([[P,L]|R],[_,M],Min) :- L < M, !, min(R,[P,L],Min).
min([_|R],M,Min) :- min(R,M,Min).
