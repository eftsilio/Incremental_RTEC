minus(Diff, A, B) :- 
	Diff is A - B, !.

minus(inf, inf, _B) :- !.

minus(inf, _A, inf).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% examine if each interval's duration is greater than a value
intDurGreater([], [], _V, [], []) :- !.

intDurGreater([], OI, V, [], OnTime) :-
	!, intDurGreater_OnTime(OI, V, OnTime).

intDurGreater([(IA1,IA2),IB|Itail], OI, V, [(IA1,IA2) | Rest], OnTime) :-
	minus(Diff, IA2, IA1),
	gt(Diff, V),
	intDurGreater([IB|Itail], OI, V, Rest, OnTime), !.

intDurGreater([_IA,IB|Itail], OI, V, Rest, OnTime) :-
	intDurGreater([IB|Itail], OI, V, Rest, OnTime), !.

intDurGreater([(IA1,IA2)], [(IA2, IB2)| Tail], V, [(IA1,IA2)], [(IA2, IB2)| Rest]) :-
	minus(Diff, IB2, IA1),
	gt(Diff, V), !,
	intDurGreater_OnTime(Tail, V, Rest).

intDurGreater([(_IA1,_IA2)], [(_IA2, _IB2)| Tail], V, [], Rest) :-
	intDurGreater_OnTime(Tail, V, Rest), !.

intDurGreater([(IA1,IA2)], OI, V, [(IA1,IA2)], OnTime) :-
	minus(Diff, IA2, IA1),
	gt(Diff, V), !,
	intDurGreater_OnTime(OI, V, OnTime).

intDurGreater([(_IA1,_IA2)], OI, V, [], OnTime) :-
	intDurGreater_OnTime(OI, V, OnTime), !.

intDurGreater_OnTime([], _V, []) :- !.

intDurGreater_OnTime([(IA1,IA2)|Itail], V, [(IA1,IA2)| Rest]) :-
	minus(Diff, IA2, IA1),
	gt(Diff, V), !,
	intDurGreater_OnTime(Itail, V, Rest).

intDurGreater_OnTime([(_IA1,_IA2)|Itail], V, Rest) :-
	intDurGreater_OnTime(Itail, V, Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% examine if each interval's duration is less than a value
intDurLess([], [], _V, [], []) :- !.

intDurLess([], OI, V, [], OnTime) :-
	!, intDurLess_OnTime(OI, V, OnTime).

intDurLess([(IA1,IA2),IB|Itail], OI, V, [(IA1,IA2) | Rest], OnTime) :-
	minus(Diff, IA2, IA1),
	lt(Diff, V),
	intDurLess([IB|Itail], OI, V, Rest, OnTime), !.

intDurLess([_IA,IB|Itail], OI, V, Rest, OnTime) :-
	intDurLess([IB|Itail], OI, V, Rest, OnTime), !.

intDurLess([(IA1,IA2)], [(IA2, IB2)| Tail], V, [(IA1,IA2)], [(IA2, IB2)| Rest]) :-
	minus(Diff, IB2, IA1),
	lt(Diff, V), !,
	intDurLess_OnTime(Tail, V, Rest).

intDurLess([(_IA1,_IA2)], [(_IA2, _IB2)| Tail], V, [], Rest) :-
	intDurLess_OnTime(Tail, V, Rest), !.

intDurLess([(IA1,IA2)], OI, V, [(IA1,IA2)], OnTime) :-
	minus(Diff, IA2, IA1),
	lt(Diff, V), !,
	intDurLess_OnTime(OI, V, OnTime).

intDurLess([(_IA1,_IA2)], OI, V, [], OnTime) :-
	intDurLess_OnTime(OI, V, OnTime), !.

intDurLess_OnTime([], _V, []) :- !.

intDurLess_OnTime([(IA1,IA2)|Itail], V, [(IA1,IA2)| Rest]) :-
	minus(Diff, IA2, IA1),
	lt(Diff, V), !,
	intDurLess_OnTime(Itail, V, Rest).

intDurLess_OnTime([(_IA1,_IA2)|Itail], V, Rest) :-
	intDurLess_OnTime(Itail, V, Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Keep time-points that belong to the overlapping part of two consecutive windows

keepOverlappingPoints([], _InitTime, []) :- !.

keepOverlappingPoints([(S,L)|Rest], InitTime, [(S,L)|Rest]) :-
	geq(S, InitTime), !.

keepOverlappingPoints([(_S,_L)|Rest], InitTime, FinalList) :-
	keepOverlappingPoints2(Rest, InitTime, FinalList).
    

% takes the union of two multisets.
multiset_union([],Y,Y) :- !.
multiset_union(Y, [], Y) :- !.
%multiset_union([(H,R1)|T],[(H,R1)|T2],[(H,R1)|S]):- multiset_union(T,T2,S), !.
multiset_union([(H, R1)|T],[(H, R2)|T2],[(H,R)|S]):- R is R1+ R2, multiset_union(T,T2,S),!.
multiset_union([(H,R1)|T],[(H2,R2)|T2],[(H,R1)|S]):- H < H2, multiset_union(T,[(H2,R2)|T2],S), !.
multiset_union([(H,R1)|T],[(H2,R2)|T2],[(H2,R2)|S]):- multiset_union([(H,R1)|T],T2,S).
