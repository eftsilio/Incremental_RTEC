
absoluteAngleDiff(A,B,C):- D is A-B ,D < -180,C is abs(D+360),!.
absoluteAngleDiff(A,B,C):- D is A-B ,D > 180,C is abs(D-360),!.

variance(L,X):-sumlist(L,S),length(L,Le),M is S/Le,variance(L,M,X).
variance([],_,0).
variance([H|T] ,M , VO):-
    variance(T,M,Y),
    VO is(Y + ((H-M)*(H-M))).

minT((DT1,T1),(DT2,T2),(DT1,T1)):- DT1 < DT2.
minT((DT1,T1),(DT2,T2),(DT2,T2)):- DT2 =< DT1.

minpair([H|T],Min):-
	minpair(T,H,Min).

minpair([],Min,Min).
minpair([H|T],MinC,Min):-
    minT(H,MinC,MinN),
    minpair(T, MinN, Min).
    
%
% Remove duplicates from list
%
rm-dups([],[]) :- !.
rm-dups([E],[E]) :- !.
rm-dups([Element|OtherElements],Singles) :-
    member(Element,OtherElements), !,
    rm-dups(OtherElements,Singles).
    
rm-dups([Element|OtherElements],[Element|Singles]) :-
    rm-dups(OtherElements,Singles).
