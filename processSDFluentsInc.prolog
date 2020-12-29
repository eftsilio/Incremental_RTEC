
% process statically determined fluents of type 1
processSDFluentInc(Index, F=V, InitTime, NewInitTime, PrevQueryTime) :-
    chechDependencySDF(Index, F=V, InitTime, NewInitTime, PrevQueryTime).

% in case the fluent is used in the body of a fluent of higher order
chechDependencySDF(Index, F=V, InitTime, NewInitTime, PrevQueryTime) :-
    % the predicate below is defined in the declarations file of a specific example
    itIsChild(F=V), !,
    processSDFluentInc2(Index, F=V, InitTime, NewInitTime, PrevQueryTime).

% in case the fluent is not a body literal
chechDependencySDF(Index, F=V, InitTime, NewInitTime, PrevQueryTime) :-
    processSDFluentInc3(Index, F=V, InitTime, NewInitTime, PrevQueryTime).
    
% this predicate deals with the case where intervals for F=V were computed at the previous query time
processSDFluentInc2(Index, F=V, InitTime, NewInitTime, PrevQueryTime) :-
    keyOfEvent(F=V, Key),
    recorded(Key, sdFPList(Index, F=V, RestrictedList, Extension, _, PrOnTime, _), R), !,
    erase(R),
    amalgamatePeriods(Extension, RestrictedList, Ltemp),
    connectLists(Ltemp, PrOnTime, ExtendedPList, PrevQueryTime),
    setTheSceneSDFluent(ExtendedPList, InitTime, BrokenPeriod, PrevPeriods), %CalcList),
    holdsForSDFluentInc(F=V, PrevPeriods, NewPeriods, OnTime),
    % take the symmetric difference in order to determine delayed and retracted intervals
    iset_symdifference(NewPeriods, PrevPeriods, Delays, Retractions),
    updatesdFPList1(Index, F=V, NewPeriods, BrokenPeriod, Delays, OnTime, Retractions).

% this predicate deals with the case where no intervals for F=V were computed at the previous query time
processSDFluentInc2(Index, F=V, _InitTime, _NewInitTime, _PrevQueryTime) :-
    holdsForSDFluentInc(F=V, [], NewPeriods, OnTime),
    updatesdFPList1(Index, F=V, NewPeriods, [], NewPeriods, OnTime, []).

% this predicate deals with the case where intervals for F=V were computed at the previous query time
processSDFluentInc3(Index, F=V, InitTime, NewInitTime, PrevQueryTime) :-
    keyOfEvent(F=V, Key),
    reocrded(Key, sdFPList(Index, F=V, RestrictedList, Extension, _, PrOnTime, _), R), !,
    erase(R),
    amalgamatePeriods(Extension, RestrictedList, Ltemp),
    connectLists(Ltemp, PrOnTime, ExtendedPList, PrevQueryTime),
    setTheSceneSDFluent(ExtendedPList, InitTime, BrokenPeriod, PrevPeriods), %CalcList),
    holdsForSDFluentInc(F=V, PrevPeriods, NewPeriods, OnTime),
    updatesdFPList1(Index, F=V, NewPeriods, BrokenPeriod, [], OnTime, []).

% this predicate deals with the case where no intervals for F=V were computed at the previous query time
processSDFluentInc3(Index, F=V, _InitTime, _NewInitTime, _PrevQueryTime) :-
    holdsForSDFluentInc(F=V, [], NewPeriods, OnTime),
    updatesdFPList1(Index, F=V, NewPeriods, [], [], OnTime, []).


% process statically determined fluents of type 2

% this predicate deals with the case where intervals for F=V were computed at the previous query time
processSDFluent2Inc(Index, F=V, InitTime, NewInitTime, PrevQueryTime) :-
	keyOfEvent(F=V, Key),
	recorded(Key, sdFPList2(Index, F=V, DepSavedPeriods, RestrictedList, Extension, _, PrOnTime, _), R), !,
	erase(R),
    % the predicate below is defined in the declarations file of a specific example
	dependency(F=V, Dependencies), !,
	processDependencies(InitTime, PrevQueryTime, Dependencies, DepSavedPeriods, NewDepSavedPeriods),
	amalgamatePeriods(Extension, RestrictedList, Ltemp),
	connectLists(Ltemp, PrOnTime, ExtendedPList, PrevQueryTime),
	setTheSceneSDFluent(ExtendedPList, InitTime, BrokenPeriod, PrevPeriods),
	holdsForSDFluentInc2(F=V, PrevPeriods, NewDepSavedPeriods, NewPeriods, OnTime),
    % if the fluent is a body literal in a rule take the symmetric difference else do not
	(itIsChild(F=V) ->
		iset_symdifference(NewPeriods, PrevPeriods, Delays, Retractions),
		updatesdFPList2(Index, F=V, NewDepSavedPeriods, NewPeriods, BrokenPeriod, Delays, OnTime, Retractions)
		;
		updatesdFPList2(Index, F=V, NewDepSavedPeriods, NewPeriods, BrokenPeriod, [], OnTime, [])
	).

% this predicate deals with the case where no intervals for F=V were computed at the previous query time
processSDFluent2Inc(Index, F=V, InitTime, NewInitTime, PrevQueryTime) :-
    % the predicate below is defined in the declarations file of a specific example
	dependency(F=V, Dependencies), !,
	processDependencies(InitTime, PrevQueryTime, Dependencies, [], NewDepSavedPeriods),
	holdsForSDFluentInc2(F=V, [], NewDepSavedPeriods, NewPeriods, OnTime),
	(itIsChild(F=V) ->
		updatesdFPList2(Index, F=V, NewDepSavedPeriods, NewPeriods, [], NewPeriods, OnTime, [])
		;
		updatesdFPList2(Index, F=V, NewDepSavedPeriods, NewPeriods, [], [], OnTime, [])
	).

% process the dependencies of a statically determined fluent of type 2 recursively
% if there are no other dependencies stop
processDependencies(_InitTime, _Pq, [], _DSP, _NDSP) :- !.

% process dependencies one by one
processDependencies(InitTime, PrevQueryTime, [(Id1, OE1)|RDep], DepSavedPeriods, NewDepSavedPeriods) :-
	getIntervals(OE1, DepSavedPeriods, Pr1, _, PrOn1, _),
	connectLists(Pr1, PrOn1, ExtendedPList, PrevQueryTime),
	setTheSceneSDFluent(ExtendedPList, InitTime, _BrokenPeriod, PrevPeriods),
	holdsForSDFluentInc2(OE1, PrevPeriods, NewDepSavedPeriods, NewPeriods, OnTime),
	(itIsChild(OE1) ->
		iset_symdifference(NewPeriods, PrevPeriods, Delays, Retractions),
		getIntervals2(OE1, NewDepSavedPeriods, NewPeriods, Delays, OnTime, Retractions),
		processDependencies(InitTime, PrevQueryTime, RDep, DepSavedPeriods, NewDepSavedPeriods)
		;
		getIntervals2(OE1, NewDepSavedPeriods, NewPeriods, [], OnTime, []),
		processDependencies(InitTime, PrevQueryTime, RDep, DepSavedPeriods, NewDepSavedPeriods)
	).

	



% deals with the interval, if any, that starts before or on Qi-WM and ends after Qi-WM
setTheSceneSDFluent([(Start,End)|Tail], InitTime, BrokenPeriod, CurrentList) :-
	% look for an interval starting before or on Qi-WM and ending after Qi-WM
	(gt(End,InitTime) ->
	(
		Start=<InitTime, nextTimePoint(InitTime,NewInitTime), BrokenPeriod=[(Start,NewInitTime)], 
		(End \== NewInitTime, CurrentList=[(NewInitTime,End)|Tail] ; CurrentList=Tail)
		;
		BrokenPeriod=[], CurrentList=[(Start,End)|Tail]
	)
	;
		setTheSceneSDFluent(Tail, InitTime, BrokenPeriod, CurrentList)
	), !.

% all intervals end before Qi-WM 
setTheSceneSDFluent(_EPList, _InitTime, [], []).

% get the intervals(overlapping, delayed, on time, retracted) of the dependencies of statically determined fluent of type 2
getIntervals(OE, [], [], [], [], []) :- !.

getIntervals(OE, Periods, Pr, D, O, R) :-
    % the predicate below is defined in the declarations file of a specific example
	mgraph(OE, Periods, Pr, D, O, R), !.

getIntervals2(OE, Periods, Pr, D, O, R) :-
	mgraph(OE, Periods, Pr, D, O, R), !.

% store the intervals of a statically determined fluent of type 1
updatesdFPList1(_Index, _U, [], [], [], [], []) :- !.

updatesdFPList1(Index, F=V, NewPeriods, BrokenPeriod, Delays, OnTime, Retractions) :-
	keyOfEvent(F=V, Key), !,
	recordz(Key, sdFPList(Index, F=V, NewPeriods, BrokenPeriod, Delays, OnTime, Retractions), R).

% store the intervals of a statically determined fluent of type 2
updatesdFPList2(_Index, _U, NDSP, [], [], [], [], []) :-
	checkEmptyDep(NDSP), !.

updatesdFPList2(Index, F=V, NewDepSavedPeriods, NewPeriods, BrokenPeriod, Delays, OnTime, Retractions) :-
	keyOfEvent(F=V, Key), !,
	recordz(Key, sdFPList2(Index, F=V, NewDepSavedPeriods, NewPeriods, BrokenPeriod, Delays, OnTime, Retractions), R).


% check if the intervals of the dependencies are empty
checkEmptyDep([]) :- !.

checkEmptyDep([(OE, [], _, [], _)|Rest]) :-
	checkEmptyDep(Rest).
	

% connect two lists of intervals (similar to amalgamate Periods)
connectLists([], On, NC, Pq) :-
	!, connectLists2(On, NC, Pq).

connectLists(Over, [], Over, _Pq) :- !.

connectLists([H|Tail], On, [H|URest], Pq) :-
	Tail \= [], !,
	connectLists(Tail, On, URest, Pq).

connectLists([(A,B)], [(B,inf)], [(A,Pq)], Pq) :- !.

connectLists([(A,B)], [(B,C)|Rest], [(A,C)|NRest], Pq) :-
	!, connectLists2(Rest, NRest, Pq). 

connectLists([(A,B)], On, [(A,B)|Rest], Pq) :-
	connectLists2(On, Rest, Pq).


connectLists2([], [], _Pq) :- !.

connectLists2([(A,B)|Tail], [(A,B)|Rest], Pq) :-
	Tail \= [], !,
	connectLists2(Tail, Rest, Pq).

connectLists2([(A,inf)], [(A,Pq)], Pq) :- A \= Pq, !.

connectLists2([(A,B)], [(A,B)], Pq) :- A \= Pq, !.

connectLists2([(Pq,inf)], [], Pq).
