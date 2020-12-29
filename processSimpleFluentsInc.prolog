% process simple fluents
processSimpleFluentInc(Index, F=V, InitTime, NewInitTime, PrevQueryTime) :-
    % the predicate below is defined in the dynamicGrounding file of a specific example
    % it checks if there are delayed events for entity Index
	(delayedEntity(Index) ->
			chechDependencySF(Index, F=V, InitTime, NewInitTime, PrevQueryTime)
			;
			processSimpleFluentInc4(Index, F=V, InitTime, NewInitTime, PrevQueryTime)
	), !.

% check if the fluent is a body literal in a rule or not
chechDependencySF(Index, F=V, InitTime, NewInitTime, PrevQueryTime) :-
	itIsChild(F=V), !,
	processSimpleFluentInc2(Index, F=V, InitTime, NewInitTime, PrevQueryTime).

chechDependencySF(Index, F=V, InitTime, NewInitTime, PrevQueryTime) :-
	processSimpleFluentInc3(Index, F=V, InitTime, NewInitTime, PrevQueryTime).

% process simple fluent that is used in a rule definition higher in the hierarchy
processSimpleFluentInc2(Index, F=V, InitTime,  NewInitTime, PrevQueryTime) :-
	isThereASimpleFPList(Index, F=V, PrevInitList, PrevTermList, ExtendedPList, NewInitTime, PrevQueryTime),
	setTheSceneSimpleFluent(ExtendedPList, F=V, InitTime, StPoint, NewCalcList),
    % collect initiation and termination rules for inserting and retracting time-points
    collectRules(F=V, IR, TR, ID, TD),
	% delete starting points within (Qi-WM,Qi] using the rules in IR
	deleteStartingPoints0(F=V, PrevInitList, SurvInitList, IR),
    % the predicate below is defined in the dynamicGrounding file of a specific example
    % it checks if there are events that arrived on time for entity Index
	onTimeEntity(Index, Oe),
    % compute the starting points within (Qi-WM,Qi] using the rules in ID
	computeStartingPoints(F=V, SurvInitList, InitList, ID, Oe),
	% append the starting point of the interval, if any, starting
	% before or on Qi-WM and ending after Qi-WM   
	% to the starting points computed at this stage
	addPoint(StPoint, InitList, CompleteInitList),
    % delete ending points within (Qi-WM,Qi] using the rules in TR
    deleteEndingPoints0(F=V, PrevTermList, SurvTermList, TR),
	% compute new intervals
	holdsForSimpleFluent(F=V, Oe, TD, NewIntervals, CompleteInitList, SurvTermList, TerminList, PrevQueryTime, OnTime),
	% update simpleFPList
	computesimpleFPList(NewIntervals, InitTime, OverlappingPeriods, Extension),
    % take the symmetric difference
	iset_symdifference(OverlappingPeriods, NewCalcList, Delays, Retractions),
	updatesimpleFPList(Index, F=V, InitList, TerminList, OverlappingPeriods, Extension, Delays, OnTime, Retractions).

% process simple fluent that is not used in a rule definition higher in the hierarchy
processSimpleFluentInc3(Index, F=V, InitTime, NewInitTime, PrevQueryTime) :-
	isThereASimpleFPList2(Index, F=V, PrevInitList, PrevTermList, ExtendedPList, NewInitTime),
	setTheSceneSimpleFluent(ExtendedPList, F=V, InitTime, StPoint, _CalcList),
    % collect initiation and termination rules for inserting and retracting time-points
    collectRules(F=V, IR, TR, ID, TD),
    % delete starting points within (Qi-WM,Qi] using the rules in IR
    deleteStartingPoints0(F=V, PrevInitList, SurvInitList, IR),
    % the predicate below is defined in the dynamicGrounding file of a specific example
    % it checks if there are events that arrived on time for entity Index
	onTimeEntity(Index, Oe),
    % compute the starting points within (Qi-WM,Qi] using the rules in ID
    computeStartingPoints(F=V, SurvInitList, InitList, ID, Oe),
	% append the starting point of the interval, if any, starting
	% before or on Qi-WM and ending after Qi-WM   
	% to the starting points computed at this stage
	addPoint(StPoint, InitList, CompleteInitList),
    % delete ending points within (Qi-WM,Qi] using the rules in TR
    deleteEndingPoints0(F=V, PrevTermList, SurvTermList, TR),
	% compute new intervals
	holdsForSimpleFluent(F=V, Oe, TD, NewIntervals, CompleteInitList, SurvTermList, TerminList, PrevQueryTime, OnTime),
	% update simpleFPList
	computesimpleFPList(NewIntervals, InitTime, OverlappingPeriods, Extension),
	updatesimpleFPList2(Index, F=V, InitList, TerminList, OverlappingPeriods, Extension, [], OnTime, []).

% process simple fluent for which there are no delayed entries
processSimpleFluentInc4(Index, F=V, InitTime, NewInitTime, PrevQueryTime) :-
	isThereASimpleFPList2(Index, F=V, SurvInitList, SurvTermList, ExtendedPList, NewInitTime),
	(onTimeEntity(Index, true) ->
		setTheSceneSimpleFluent(ExtendedPList, F=V, InitTime, StPoint, _CalcList),
		computeStartingPointsOnTime(F=V, SurvInitList, InitList),
		addPoint(StPoint, InitList, CompleteInitList),
		holdsForSimpleFluentOnTime(InitTime, F=V, NewIntervals, CompleteInitList, SurvTermList, TerminList, PrevQueryTime, OnTime),
		computesimpleFPList(NewIntervals, InitTime, OverlappingPeriods, Extension),
		updatesimpleFPList2(Index, F=V, InitList, TerminList, OverlappingPeriods, Extension, [], OnTime, [])

		;

		computesimpleFPList2(ExtendedPList, InitTime, PrevQueryTime, Extension, OverlappingPeriods, OnTime),
		updatesimpleFPList2(Index, F=V, SurvInitList, SurvTermList, OverlappingPeriods, Extension, [], OnTime, [])
	).


% retrieve from memory the starting-ending points and the intervals calculated at the previous query time
isThereASimpleFPList(Index, F=V, PrevInitList, PrevTermList, ExtendedPList, NewInitTime, Pq) :-
	keyOfEvent(F=V, Key),
	recorded(Key, simpleFPList(Index, F=V, I_Qi, T_Qi, RestrictedList, Extension, _, OnTime, _), R), !,
	erase(R),
	keepOverlappingPoints(I_Qi, NewInitTime, PrevInitList),
	keepOverlappingPoints(T_Qi, NewInitTime, PrevTermList),
	amalgamatePeriods(Extension, RestrictedList, Ltemp),
	connectLists(Ltemp, OnTime, ExtendedPList, Pq).

% this predicate deals with the case where no intervals for F=V were computed at the previous query time
isThereASimpleFPList(_Index, _U, [], [], [], _NewInitTime, _Pq).

isThereASimpleFPList2(Index, F=V, PrevInitList, PrevTermList, ExtendedPList, NewInitTime) :-
	keyOfEvent(F=V, Key),
	recorded(Key, simpleFPList(Index, F=V, I_Qi, T_Qi, RestrictedList, Extension, _, OnTime, _), R), !,
	erase(R),
	keepOverlappingPoints(I_Qi, NewInitTime, PrevInitList),
	keepOverlappingPoints(T_Qi, NewInitTime, PrevTermList),
	amalgamatePeriods(Extension, RestrictedList, Ltemp),
	iset_union(Ltemp, OnTime, ExtendedPList).

% this predicate deals with the case where no intervals for F=V were computed at the previous query time
isThereASimpleFPList2(_Index, _U, [], [], [], _NewInitTime).


addPoint([], L, L) :- !.
addPoint([P], L, [(P,1)|L]).


/************************************************************************************************************* 
   This predicate is similar to setTheSceneSDFluent. The main difference is that instead of breaking
   the interval, if any, that starts before or on Qi-Memory and ends after Qi-Memory, we delete it (the 
   interval) and keep the starting point. cachedHoldsFor will create the fluent intervals given this 
   starting point and other starting and ending points within (Qi-WM,WM]. 
 *************************************************************************************************************/

% there is no need to update starting points in this case
% if there were any starting points then the first argument would not have been empty
setTheSceneSimpleFluent([], _U, _InitTime, [], []) :- !.

% deals with the interval, if any, that starts before or on Qi-WM and ends after Qi-WM
setTheSceneSimpleFluent([(Start,End)|Tail], _U, InitTime, StPoint, CurrentList) :-
	% look for an interval starting before or on Qi-WM and ending after Qi-WM
	InitTimePlus1 is InitTime+1,
	(gt(End,InitTimePlus1) ->
		StartMinus1 is Start-1,
		(
			StartMinus1=<InitTime, StPoint=[StartMinus1], CurrentList=[(InitTimePlus1,End)|Tail]
			;
			StPoint=[], CurrentList=[(Start,End)|Tail]
		)
		;
			setTheSceneSimpleFluent(Tail, _U, InitTime, StPoint, CurrentList)
	), !.


% count how many times a time-point has been derived
countSolutions(C, [(T,_)], [(T,C)]) :- !.

countSolutions(C, [(T,_), (T,_)|Rest], CL) :-
	N is C+1, !,
	countSolutions(N, [(T,_)|Rest], CL).

countSolutions(C, [(T,_), (T2,_)|Rest], [(T,C)|MoreCL]) :-
	!, countSolutions(1, [(T2,_)|Rest], MoreCL).
	
/****** compute starting points ******/

computeStartingPoints(F=V, SurvInitList, InitList, R, Oe) :-
	initList(F=V, SurvInitList, DList, R),
	initListOnTime(F=V, DList, InitList, Oe).


computeStartingPointsOnTime(F=V, SurvInitList, InitList) :-
        initListOnTime(F=V, SurvInitList, InitList, true).

% find the initiating time-points falling in the non-overlapping part of two consecutive query times

initListOnTime(F=V, SurvInitList, InitList, true) :-
	setof((T,R), initiatedAt_OnTime(F=V, T, R), L), !,
	countSolutions(1, L, TempI),
	multiset_union(SurvInitList, TempI, InitList).

initListOnTime(_U, SurvInitList, SurvInitList, true) :- !.

initListOnTime(_U, SurvInitList, SurvInitList, false).


% find the initiating time-points falling in the overlapping part of two consecutive query times
initList(_U, SurvInitList, SurvInitList, []) :- !.

initList(F=V, SurvInitList, InitList, Rules) :-
	setof((T,R), (member(R, Rules), initiatedAt_Delay(F=V, T, R)), L), !,
	countSolutions(1, L, TempI),
	multiset_union(SurvInitList, TempI, InitList).

initList(_U, SurvInitList, SurvInitList, _Rules).


initPoint(F=V, Ts, R) :-
	initiatedAt_Delay(F=V, Ts, R).


/****** compute ending points ******/

computeEndingPoints(F=V, [], TerminList, _R, Oe) :-
	!, terminList_all(F=V, TermOver),
	terminListOnTime(F=V, TermOver, TerminList, Oe).

computeEndingPoints(F=V, SurvTermList, TerminList, R, Oe) :-
	terminList(F=V, SurvTermList, DList, R),
	terminListOnTime(F=V, DList, TerminList, Oe).


computeEndingPointsOnTime(F=V, [], TerminList) :-
	!, terminList_all(F=V, TermOver),
	terminListOnTime(F=V, TermOver, TerminList, true).

computeEndingPointsOnTime(F=V, SurvTermList, TerminList) :-
    terminListOnTime(F=V, SurvTermList, TerminList, true).


% find the terminating time-points falling in the non-overlapping part of two consecutive query times

terminListOnTime(F=V, SurvTermList, TerminList, true) :-
	setof((T,R), terminatedAt_OnTime(F=V, T, R), L), !,
	countSolutions(1, L, TempT),
	multiset_union(SurvTermList, TempT, TerminList).

terminListOnTime(_U, SurvTermList, SurvTermList, true) :- !.

terminListOnTime(_U, SurvTermList, SurvTermList, false).

% find the terminating time-points within (Qi-WM,Qi]
terminList_all(F=V, TerminList) :-
	setof((T,R), terminatedAt(F=V, T, R), L), !,
	countSolutions(1, L, TerminList).

terminList_all(_U, []).

% find the terminating time-points falling in the overlapping part of two consecutive query times
terminList(F=V, SurvTermList, SurvTermList, []) :- !.

terminList(F=V, SurvTermList, TerminList, Rules) :-
	setof((T,R), (member(R, Rules), terminatedAt_Delay(F=V, T, R)), L), !,
	countSolutions(1, L, TempT),
	multiset_union(SurvTermList, TempT, TerminList).

terminList(_U, SurvTermList, SurvTermList, _Rules).


/****** delete starting points ******/

deleteStartingPoints0(_U, [], [], _R) :- !.

deleteStartingPoints0(_U, Surv, Surv, []) :- !.

deleteStartingPoints0(F=V, Surv, Fin, R) :-
	deleteStartingPoints(F=V, Surv, Fin, R).

deleteStartingPoints(_U, [], [], _R) :- !.

deleteStartingPoints(F=V, [(H, Cp)|Tail], [(H,C)|Result], R) :-
        deleteStartingPoints2(F=V, (H, Cp), C, R), !, 
        deleteStartingPoints(F=V, Tail, Result, R).

deleteStartingPoints(F=V, [_H|Tail], Result, R) :-
        deleteStartingPoints(F=V, Tail, Result, R).

deleteStartingPoints2(F=V, (H, Cf), Cf, []) :- Cf > 0, !.

deleteStartingPoints2(F=V, (H, Cp), Cf, [RH|RT]) :-
	deleteStartingPoints3(F=V, (H, Cp), Ct, RH),
	deleteStartingPoints2(F=V, (H, Ct), Cf, RT).

deleteStartingPoints3(F=V, (H, Cf), Cf, []) :- !.

deleteStartingPoints3(F=V, (H, Cp), Cf, [RH|_RT]) :-
	once(initiatedAt_Retract(F=V, H, RH)), !, Cf is Cp -1.

deleteStartingPoints3(F=V, (H, Cp), Cf, [_RH|RT]) :-
	deleteStartingPoints3(F=V, (H, Cp), Cf, RT).


/****** delete ending points ******/

deleteEndingPoints0(_U, [], [], _R) :- !.

deleteEndingPoints0(_U, Surv, Surv, []) :- !.

deleteEndingPoints0(F=V, Surv, Fin, R) :-  
        deleteEndingPoints(F=V, Surv, Fin, R).

deleteEndingPoints(_U, [], [], _R) :- !.

deleteEndingPoints(F=V, [(H, Cp)|Tail], [(H,C)|Result], R) :-
        deleteEndingPoints2(F=V, (H, Cp), C, R), !, 
        deleteEndingPoints(F=V, Tail, Result, R).

deleteEndingPoints(F=V, [_H|Tail], Result, R) :-
        deleteEndingPoints(F=V, Tail, Result, R).

deleteEndingPoints2(F=V, (H, Cf), Cf, []) :- Cf > 0, !.

deleteEndingPoints2(F=V, (H, Cp), Cf, [RH|RT]) :-
        deleteEndingPoints3(F=V, (H, Cp), Ct, RH),
        deleteEndingPoints2(F=V, (H, Ct), Cf, RT).  

deleteEndingPoints3(F=V, (H, Cf), Cf, []) :- !.

deleteEndingPoints3(F=V, (H, Cp), Cf, [RH|_RT]) :-
        once(terminatedAt_Retract(F=V, H, RH)), !, Cf is Cp -1.

deleteEndingPoints3(F=V, (H, Cp), Cf, [_RH|RT]) :-
        deleteEndingPoints3(F=V, (H, Cp), Cf, RT).


/****** compute new intervals given the computed starting and ending points ******/
% this predicate is used when there are no delayed events
holdsForSimpleFluentOnTime(_InitTime, _U, [], [], SurvTermList, [], _Pq, []) :- !.

holdsForSimpleFluentOnTime(-1, F=V, [], InitList, SurvTermList, TerminList, _Pq, OnTimeList) :-
    computeEndingPointsOnTime(F=V, SurvTermList, TerminList),
	makeIntervalsFromSEPointsOnTime(InitList, TerminList, OnTimeList), !.

holdsForSimpleFluentOnTime(_InitTime, F=V, PeriodList, InitList, SurvTermList, TerminList, Pq, OnTimeList) :-
    % compute the ending points within (Qi-WM,Qi]
	computeEndingPointsOnTime(F=V, SurvTermList, TerminList),
	makeIntervalsFromSEPoints(InitList, TerminList, Pq, PeriodList, OnTimeList), !.

% this predicate is used when there are delayed events
holdsForSimpleFluent(_U, _Oe, _R, [], [], SurvTermList, [], _Pq, []) :- !.

holdsForSimpleFluent(F=V, Oe, Rules, PeriodList, InitList, SurvTermList, TerminList, Pq, OnTimeList) :-
	% compute the ending points within (Qi-WM,Qi]
	computeEndingPoints(F=V, SurvTermList, TerminList, Rules, Oe),
	makeIntervalsFromSEPoints(InitList, TerminList, Pq, PeriodList, OnTimeList), !.	


% the predicate below works under the assumption that the lists of 
% initiating and terminating points are temporally sorted

makeIntervalsFromSEPoints([], _EPoints, Pq, [], []) :- !.

% base cases: single initiation point
makeIntervalsFromSEPoints([(Ts, _)], EPoints, Pq, Period, OnTime) :-
    member((Tf, _), EPoints),
    Ts=<Tf,
    (
        Ts=Tf, !,
        Period=[], OnTime=[]
        ;
        %Ts<Tf
        Tf<Pq,
        !, nextTimePoint(Ts, TsNew), nextTimePoint(Tf, TfNew), Period=[(TsNew,TfNew)], OnTime=[]
        ;
        nextTimePoint(Ts, TsNew), TsNew<Pq,
        !, nextTimePoint(Tf, TfNew), Period=[(TsNew,Pq)], OnTime=[(Pq,TfNew)]
        ;
        nextTimePoint(Ts, TsNew),
        !, nextTimePoint(Tf, TfNew), Period=[], OnTime=[(TsNew,TfNew)]
    ).

makeIntervalsFromSEPoints([(Ts,_)], _EPoints, Pq, [(TsNew,Pq)], [(Pq,inf)]) :- nextTimePoint(Ts, TsNew), TsNew<Pq, !.

makeIntervalsFromSEPoints([(Ts,_)], _EPoints, Pq, [], [(TsNew,inf)]) :- nextTimePoint(Ts, TsNew), !.

% recursion: at least two initiation points
makeIntervalsFromSEPoints([(T,_)|MoreTs], [(T,_)|MoreTf], Pq, Periods, OnTime) :-
        T<Pq, !, makeIntervalsFromSEPoints(MoreTs, MoreTf, Pq, Periods, OnTime).

makeIntervalsFromSEPoints([(T,_)|MoreTs], [(T,_)|MoreTf], Pq, [], OnTime) :-
        !, makeIntervalsFromSEPointsOnTime(MoreTs, MoreTf, OnTime).

makeIntervalsFromSEPoints([(Ts,_)|MoreTs], [(Tf,_)|MoreTf], Pq, Periods, OnTime) :-
        Tf<Ts, Ts<Pq, !,
        makeIntervalsFromSEPoints([(Ts,_)|MoreTs], MoreTf, Pq, Periods, OnTime).

makeIntervalsFromSEPoints([(Ts,_)|MoreTs], [(Tf,_)|MoreTf], Pq, [], OnTime) :-
        Tf<Ts, !,
        makeIntervalsFromSEPointsOnTime([(Ts,_)|MoreTs], MoreTf, OnTime).

makeIntervalsFromSEPoints([(Ts,_),(Ts2,_)|MoreTs], [(Tf,_)|MoreTf], Pq, Periods, OnTime) :-
	%Ts<Tf,
	Ts2=<Tf,
	!, makeIntervalsFromSEPoints([(Ts,_)|MoreTs], [(Tf,_)|MoreTf], Pq, Periods, OnTime).

makeIntervalsFromSEPoints([(Ts,_),(Ts2,_)|MoreTs], [(Tf,_)|MoreTf], Pq, [(TsNew,TfNew)|MorePeriods], OnTime) :-
	%Ts<Tf,
	Ts2>Tf, Tf<Pq,
	!, nextTimePoint(Ts, TsNew), nextTimePoint(Tf, TfNew),
	makeIntervalsFromSEPoints([(Ts2,_)|MoreTs], MoreTf, Pq, MorePeriods, OnTime).

makeIntervalsFromSEPoints([(Ts,_),(Ts2,_)|MoreTs], [(Tf,_)|MoreTf], Pq, [(TsNew,Pq)], [(Pq,TfNew)|MoreOnTime]) :-
	%Ts<Tf,
	Ts2>Tf, nextTimePoint(Ts, TsNew), TsNew<Pq,
	!, nextTimePoint(Tf, TfNew),
	makeIntervalsFromSEPointsOnTime([(Ts2,_)|MoreTs], MoreTf, MoreOnTime).

makeIntervalsFromSEPoints([(Ts,_),(Ts2,_)|MoreTs], [(Tf,_)|MoreTf], Pq, [], [(TsNew,TfNew)|MoreOnTime]) :-
	%Ts<Tf,
	Ts2>Tf, nextTimePoint(Ts, TsNew),
	!, nextTimePoint(Tf, TfNew),
	makeIntervalsFromSEPointsOnTime([(Ts2,_)|MoreTs], MoreTf, MoreOnTime).

makeIntervalsFromSEPoints([(Ts,_),_Tnext|_MoreTs], _EPoints, Pq, [(TsNew,Pq)], [(Pq,inf)]) :-
        nextTimePoint(Ts, TsNew), TsNew<Pq, !.

makeIntervalsFromSEPoints([(Ts,_),_Tnext|_MoreTs], _EPoints, Pq, [], [(TsNew,inf)]) :-
        nextTimePoint(Ts, TsNew), !.

% the predicate below works under the assumption that the lists of 
% initiating and terminating points are temporally sorted

makeIntervalsFromSEPointsOnTime([], _EPoints, []) :- !.

% base cases: single initiation point
makeIntervalsFromSEPointsOnTime([(Ts, _)], EPoints, Period) :-
    member((Tf, _), EPoints), 
    Ts=<Tf, 
    (
        Ts=Tf, !, 
        Period=[]
        ;   
        %Ts<Tf
        !, nextTimePoint(Ts, TsNew), nextTimePoint(Tf, TfNew), Period=[(TsNew,TfNew)]
    ).

makeIntervalsFromSEPointsOnTime([(Ts,_)], _EPoints, [(TsNew,inf)]) :- nextTimePoint(Ts, TsNew), !.

% recursion: at least two initiation points
makeIntervalsFromSEPointsOnTime([(T,_)|MoreTs], [(T,_)|MoreTf], Periods) :-
    !, makeIntervalsFromSEPointsOnTime(MoreTs, MoreTf, Periods).

makeIntervalsFromSEPointsOnTime([(Ts,_)|MoreTs], [(Tf,_)|MoreTf], Periods) :-
    Tf<Ts, !, 
    makeIntervalsFromSEPointsOnTime([(Ts,_)|MoreTs], MoreTf, Periods).

makeIntervalsFromSEPointsOnTime([(Ts,_),(Ts2,_)|MoreTs], [(Tf,_)|MoreTf], Periods) :-
    %Ts<Tf,  
    %Tf=Tnext,
    Ts2=<Tf,
    !, makeIntervalsFromSEPointsOnTime([(Ts,_)|MoreTs], [(Tf,_)|MoreTf], Periods).

makeIntervalsFromSEPointsOnTime([(Ts,_),(Ts2,_)|MoreTs], [(Tf,_)|MoreTf], [(TsNew,TfNew)|MorePeriods]) :-
    %Ts<Tf,  
    !, nextTimePoint(Ts, TsNew), nextTimePoint(Tf, TfNew),
    makeIntervalsFromSEPointsOnTime([(Ts2,_)|MoreTs], MoreTf, MorePeriods).

makeIntervalsFromSEPointsOnTime([(Ts,_),_Tnext|_MoreTs], _EPoints, [(TsNew,inf)]) :-
    nextTimePoint(Ts, TsNew).



/****** computesimpleFPList  ******/

computesimpleFPList(L, -1, L, []) :- !.

computesimpleFPList([], _InitTime, [], []) :- !.

computesimpleFPList([(Start,End)|Tail], InitTime, [(Start,End)|Tail], []) :-
	Start>InitTime, !.

computesimpleFPList([(Start,End)|Tail], InitTime, [(NewInitTime,End)|Tail], [(Start,NewInitTime)]) :-
	nextTimePoint(InitTime, NewInitTime), 
	\+ NewInitTime = End, !.

computesimpleFPList([Head|Tail], _InitTime, Tail, [Head]).



computesimpleFPList2([(Start,End)|Tail], InitTime, Pq, BrokenPeriod, Overlapping, OnTime) :-
	% look for an interval starting before or on Qi-WM and ending after Qi-WM
	(gt(End,InitTime), nextTimePoint(InitTime,NewInitTime), \+ NewInitTime = End ->
	(
		Start=<InitTime, BrokenPeriod=[(Start,NewInitTime)], seperate([(NewInitTime,End)|Tail], Overlapping, OnTime, Pq)
		;
		BrokenPeriod=[], seperate([(Start,End)|Tail], Overlapping, OnTime, Pq)
	)
	;
		computesimpleFPList2(Tail, InitTime, Pq, BrokenPeriod, Overlapping, OnTime)
	), !.

computesimpleFPList2(_EPList, _InitTime, _Pq, [], [], []).


/****** updateSimpleFPList  ******/

updatesimpleFPList(_Index, _U, _, _, [], _, _, [], []) :- !.

updatesimpleFPList(Index, F=V, InitList, TerminList, NewPeriods, BrokenPeriod, Delays, OnTime, Retractions) :-
	keyOfEvent(F=V, Key), !,
	recordz(Key, simpleFPList(Index, F=V, InitList, TerminList, NewPeriods, BrokenPeriod, Delays, OnTime, Retractions), _).

updatesimpleFPList2(_Index, _U, _, _, [], _, _, [], _) :- !.

updatesimpleFPList2(Index, F=V, InitList, TerminList, NewPeriods, BrokenPeriod, Delays, OnTime, Retractions) :-
	keyOfEvent(F=V, Key), !,
	recordz(Key, simpleFPList(Index, F=V, InitList, TerminList, NewPeriods, BrokenPeriod, Delays, OnTime, Retractions), _).


% Collection of rules
% IR: rules that may delete a starting point
% TR: rules that may delete an ending point
% ID: rules that may produce a starting point due to delays
% TD: rules that may produce an ending point due to delays
collectRules(F=V, IR, TR, ID, TD) :-
    dependency(F=V, Dep), !,
    assignToRules(Dep, SR),
    % the predicate below is defined in the declarations file of a specific example
    rgraph(F=V, [R1, R2, R3, R4], SR), !,
	collectValidRules1(R1, IR),
	collectValidRules1(R2, TR),
	collectValidRules2(R3, ID),
	collectValidRules2(R4, TD).


assignToRules([], []) :- !.

assignToRules([P|Tail], [true|Rest]) :-
    once(P), !,
    assignToRules(Tail, Rest).

assignToRules([_P|Tail], [false|Rest]) :-
    assignToRules(Tail, Rest).

collectValidRules1([], []) :- !.

collectValidRules1([H|Tail], [Result|Rest]) :-
	collectValidRules2(H, Result), Result \= [], !,
	collectValidRules1(Tail, Rest).

collectValidRules1([_H|Tail], Result) :-
	collectValidRules1(Tail, Result).

collectValidRules2([], []) :- !.

collectValidRules2([(true, X)|Tail], [X|Rest]) :-
	!, collectValidRules2(Tail, Rest).

collectValidRules2([_H|Tail], FR) :-
	collectValidRules2(Tail, FR).
