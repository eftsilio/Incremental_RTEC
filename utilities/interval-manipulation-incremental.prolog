
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% seperate the overlapping and the non-overlapping intervals

seperate([], [], [], _Pq) :- !.

seperate([(S,E)|T], [(S,E)|Rest], OnTime, Pq) :-
	lt(E, Pq),
	!,
	seperate(T, Rest, OnTime, Pq), !.

seperate([(S,E)|T], [(S,Pq)], [(Pq,E)|T], Pq) :-
	gt(E, Pq), lt(S, Pq),
	!.

seperate([(S,E)|T], [(S,E)], T, Pq) :-
	E == Pq,
	!.

seperate([(S,E)|T], [], [(S,E)|T], Pq) :-
	geq(S, Pq),
	!.

%%%%%%%%%%%%%%%%%%%%%% Main predicate for symmetric difference %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

iset_symdifference( A, [], A, [] ) :- !.

iset_symdifference( [], B, [], B ) :- !.

iset_symdifference( [(A11,A12)|A2_n], [(B11,B12)|B2_n], [(A11,A12)|ABRest], BARest ) :-
    interval_is_less( [A11,A12], [B11,B12] ),
    iset_symdifference( A2_n, [(B11,B12)|B2_n], ABRest, BARest ), !.

iset_symdifference( [(A11,A12)|A2_n], [(B11,B12)|B2_n], ABRest, [(B11,B12)|BARest] ) :-
    interval_is_less( [B11,B12], [A11,A12] ),
    iset_symdifference( [(A11,A12)|A2_n], B2_n, ABRest, BARest ), !.

iset_symdifference( [(A1_low,A1_high)|A2_n], [(B1_low,B1_high)|B2_n], [(A1_low,B1_low) | ABRest ], BARest  ) :-
    % A1_low < B1_low,
    lt(A1_low, B1_low),
    !,
    %%% B1_low_less_1 is B1_low - 1,
    iset_symdifference( [ (B1_low,A1_high) | A2_n ], [ (B1_low,B1_high) | B2_n ], ABRest, BARest ), !.

iset_symdifference( [(A1_low,High)|A2_n], [(A1_low,High)|B2_n], ABRest, BARest ) :-
    iset_symdifference( A2_n, B2_n, ABRest, BARest ), !.

iset_symdifference( [(A1_low,High)|A2_n], [(B1_low,High)|B2_n], ABRest, [(B1_low,A1_low)|BARest] ) :-
    iset_symdifference( A2_n, B2_n, ABRest, BARest ), !.

iset_symdifference( [(A1_low,A1_high)|A2_n], [(A1_low,B1_high)|B2_n], ABRest, BARest ) :-
    %A1_high > B1_high,
    gt(A1_high, B1_high),
    !,
    %%% B1_high_add_1 is B1_high + 1,
    iset_symdifference( [ (B1_high,A1_high) | A2_n ], B2_n, ABRest, BARest ), !.

iset_symdifference( [(A1_low,A1_high)|A2_n], [(B1_low,B1_high)|B2_n], ABRest, [(B1_low,A1_low)|BARest] ) :-
    %A1_high > B1_high,
    gt(A1_high, B1_high),
    !,
    %%% B1_high_add_1 is B1_high + 1,
    iset_symdifference( [ (B1_high,A1_high) | A2_n ], B2_n, ABRest, BARest ), !.

iset_symdifference( [(A1_low,A1_high)|A2_n], [(A1_low,B1_high)|B2_n], ABRest, BARest ) :-
    %A1_high < B1_high,
    lt(A1_high, B1_high),
    !,
    %%% A1_high_add_1 is A1_high + 1,
    iset_symdifference( A2_n, [ (A1_high,B1_high) | B2_n ], ABRest, BARest ), !.

iset_symdifference( [(A1_low,A1_high)|A2_n], [(B1_low,B1_high)|B2_n], ABRest, [(B1_low,A1_low)|BARest] ) :-
    %%% A1_high_add_1 is A1_high + 1,
    iset_symdifference( A2_n, [ (A1_high,B1_high) | B2_n ], ABRest, BARest ), !.

%%%%%%%%%%%%%%%%%%%%%% Main predicate for incremental union %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

incremental_union_all_alt([O_A,O_B], [OnTime_A, OnTime_B], OverAorB, OnTimeAorB) :-
	getCPUTime(ST,yap),
	iset_union(O_A, O_B, OverAorB),
	iset_union(OnTime_A, OnTime_B, OnTimeAorB),
	getCPUTime(ET,yap),
	T is ET - ST, nb_getval(window_union, LT), FLT is LT + T, nb_setval(window_union, FLT).

incremental_union_all(OI, Delays, [OnTime_A, OnTime_B], Retractions, PrevAorB, OverAorB, OnTimeAorB) :-
	getCPUTime(ST,yap),
	incremental_union(OI, Delays, Retractions, PrevAorB, OverAorB),
	iset_union(OnTime_A, OnTime_B, OnTimeAorB),
	getCPUTime(ET,yap),
	T is ET - ST, nb_getval(window_union, LT), FLT is LT + T, nb_setval(window_union, FLT).

incremental_union([[],[]], _Delays, _Retractions, _PrevAorB, []) :- !.

incremental_union(_OI, [[],[]], [[],[]], OverAorB, OverAorB) :- !.

incremental_union([O_A,[]], _Delays, _Retractions, _PrevAorB, O_A) :- !.

incremental_union([[],O_B], _Delays, _Retractions, _PrevAorB, O_B) :- !.

incremental_union(_OI, [Delays_A,Delays_B], [[],[]], PrevAorB, OverAorB) :-
	iset_union(Delays_A, Delays_B, DUnion),
	iset_union(PrevAorB, DUnion, OverAorB), !.

incremental_union([O_A, O_B], _Delays, _Retractions, _PrevAorB, OverAorB) :-
	iset_union(O_A, O_B, OverAorB).

%%%%%%%%%%%%%%%%%%%%%% Main predicate for incremental intersection %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

incremental_intersect_all_alt([O_A,O_B], [OnTime_A, OnTime_B], OverAandB, OnTimeAandB) :-
	getCPUTime(ST,yap),
	iset_intersection(O_A, O_B, OverAandB),
	iset_intersection(OnTime_A, OnTime_B, OnTimeAandB),
	getCPUTime(ET,yap),
	T is ET - ST, nb_getval(window_inter, LT), FLT is LT + T, nb_setval(window_inter, FLT).

incremental_intersect_all(OI, Delays, [OnTime_A, OnTime_B], Retractions, PrevAandB, OverAandB, OnTimeAandB) :-
	getCPUTime(ST,yap),
	incremental_intersect(OI, Delays, Retractions, PrevAandB, OverAandB),
	iset_intersection(OnTime_A, OnTime_B, OnTimeAandB),
	getCPUTime(ET,yap),
	T is ET - ST, nb_getval(window_inter, LT), FLT is LT + T, nb_setval(window_inter, FLT).

incremental_intersect(O, _Delays, _Retractions, _PrevAandB, []) :- member([], O), !.

incremental_intersect(_OI, [[],[]], [[],[]], OverAandB, OverAandB) :- !.

incremental_intersect([O_A,O_B], [Delays_A,Delays_B], [[],[]], PrevAandB, OverAandB) :-
	
	%%%%%%%%%%%%%%%%% B --> A %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	iset_intersection(Delays_B, O_A, I_AB),

	%%%%%%%%%%%%%%%%% A --> B %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	iset_intersection(Delays_A, O_B, I_BA),

	%%%%%%%%%%%%%%%%% Final Intersection %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	union_all([PrevAandB, I_AB, I_BA], OverAandB), !.

incremental_intersect([O_A,O_B], _Delays, _Retractions, _PrevAandB, OverAandB) :-
	iset_intersection(O_A, O_B, OverAandB).


%%%%%%%%%%%%%%%%%%%%%% Main predicate for incremental complement %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

incremental_relative_complement_all(OI, Delays, [OnTime_A, OnTime_B], Retractions, PrevAnotB, OverAnotB, OnTimeAnotB) :-
	getCPUTime(ST,yap),
	incremental_relative_complement(OI, Delays, Retractions, PrevAnotB, OverAnotB),
	iset_difference(OnTime_A, OnTime_B, OnTimeAnotB),
	getCPUTime(ET,yap),
	T is ET - ST, nb_getval(window_comp, LT), FLT is LT + T, nb_setval(window_comp, FLT).

incremental_relative_complement([[],[]], _Delays, _Retractions, _AnotB, []) :- !.

incremental_relative_complement(_OI, [[],[]], [[],[]], OverAnotB, OverAnotB) :- !.

incremental_relative_complement([O_A,[]], _Delays, _Retractions, _PrevAnotB, O_A) :- !.

incremental_relative_complement([[],_O_B], _Delays, _Retractions, _PrevAnotB, []) :- !.

incremental_relative_complement([_O_A, O_B], [Delays_A,Delays_B], [[],[]], PrevAnotB, OverAnotB) :-
	
	%%%%%%%%%%%%%%%%% B --> A %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	iset_difference(PrevAnotB, Delays_B, TempA1),

	%%%%%%%%%%%%%%%%% A --> B %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	iset_difference(Delays_A, O_B, TempA2),

	%%%%%%%%%%%%%%%%% Final Intersection %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	iset_union(TempA1, TempA2, OverAnotB), !.

incremental_relative_complement([O_A,O_B], _Delays, _Retractions, _PrevAnotB, OverAnotB) :-
	iset_difference(O_A, O_B, OverAnotB).


