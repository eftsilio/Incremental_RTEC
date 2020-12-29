

% ========================
/*
EVENT RECOGNITION LOOP

--- initialiseRecognition(+InputFlag, +PreProcessingFlag, +TemporalDistance). 
InputFlag=ordered means that input facts are temporally sorted. 
InputFlag=any_other_value means that input facts are not temporally sorted. 
PreProcessingFlag=preprocessing means that there is a need for preprocessing by means of an application-dependent preProcessing/1. See the experiments on the CAVIAR dataset for an example of preprocessing.
PreProcessingFlag=any_other_value means that there is no need for preprocessing.
TemporalDistance is an integer denoting the distance between two consecutive time-points. Eg, in CAVIAR the temporal distance is 40.  

Assert input facts at your leisure, even in a non-chronological manner. Then perform event recognition:
--- eventRecognition(+Qi, +WM, +Step).
where Qi is the current query time, WM is the 'working memory', Step is the sliding step.

A NOTE ON THE LISTS THAT ARE USED IN THE CODE

- simpleFPList(Index, F=V, InitList, TerminList, OverlappingPeriods, BrokenPeriod, Delays, OnTime, Retractions) where InitList is the list of initiation points within (Qi-WM, Qi], TerminList is the list of termination points within within (Qi-WM, Qi], OverlappingPeriods is the list of periods falling in the overlapping part of two consecutive windows, BrokenPeriod is the period before Qi-WM, Delays is the list of intervals falling in the overlapping part and calculated at Qi, OnTime is the list of intervals falling in the non-overlapping part and Retractions is the list of intervals falling in the overlapping part and retracted at Qi. BrokenPeriod must be amalgamated with OverlappingPeriods and the result must be connected with OnTime in order to produce the correct result of event recognition at Qi. F=V is a simple fluent and an output entity.

- sdFPList(Index, F=V, OverlappingPeriods, BrokenPeriod, Delays, OnTime, Retractions) where OverlappingPeriods is the list of periods falling in the overlapping part of two consecutive windows, BrokenPeriod is the period before Qi-WM, Delays is the list of intervals falling in the overlapping part and calculated at Qi, OnTime is the list of intervals falling in the non-overlapping part and Retractions is the list of intervals falling in the overlapping part and retracted at Qi. BrokenPeriod must be amalgamated with OverlappingPeriods and the result must be connected with OnTime in order to produce the correct result of event recognition at Qi. F=V is a statically determined fluent of type 1 and an output entity.

- sdFPList2(Index, F=V, DepSavedPeriods, OverlappingPeriods, BrokenPeriod, Delays, OnTime, Retractions) like above except that DepSavedPeriods includes the overlapping, delayed, on time and retracted intervals of the dependencies of statically determined fluent of type 2 F=V.

- iePList(IE, Index, OverlappingPeriods, BrokenPeriod, Delays, OnTime, Retractions) similar to sdFPList, except that IE is an input entity.

---RTEC PREDICATES---

The predicates below are available to the user:

-happensAt(E, T) represents the time-points T in which an event E occurs.
-happensAtIER(E, T) represents the retracted time-points T of event E.
-delay(E, T) represents the time-points T in which an event E occurs but arrived to the system with a delay.

-initiatedAt_Retract(F=V, T, _) states that time-point T belonging to the overlapping part of consecutive windows no longer initiates F=V.
-initiatedAt_Delay(F=V, T, _) states that time-point T belonging to the overlapping part of consecutive windows initiates F=V.
-terminatedAt_Retract(F=V, T, _) states that time-point T belonging to the overlapping part of consecutive windows no longer terminates F=V.
-terminatedAt_Delay(F=V, T, _) states that time-point T belonging to the overlapping part of consecutive windows terminates F=V.
-initiatedAt_OnTime(F=V, T, _) states that time-point T belonging to the non-overlapping part of consecutive windows initiates F=V.
-terminatedAt_OnTime(F=V, T, _) states that time-point T belonging to the non-overlapping part of consecutive windows terminates F=V.
-terminatedAt(F=V, T, _) states that time-point T belonging to (Qi-WM, Qi]) terminates F=V.
-holdsFor(F=V, L) represents that the list of maximal intervals L during which F=V holds continuously.
-holdsAt(F=V, T) states that F=V holds at time-point T. 


-happensAtProcessedStaticIE_Over(Index, E, [T|_]) represents the time-points belonging to the overlapping part in which event E occurs. The retracted time-points are excluded.
-happensAtProcessedStaticIE_Delays(Index, E, [T|_]) represents the time-points belonging to the overlapping part in which event E occurs and arrived with a delay.
-happensAtProcessedStaticIE_Retractions(Index, E, [T|_]) represents the time-points belonging to the overlapping part in which event E occurred and at Qi are retracted.
-happensAtProcessedStaticIE_OnTime(Index, E, [T|_]) represents the time-points belonging to the non-overlapping part in which event E occurs.
happensAtProcessedStaticIE_Qi_notDelays(Index, E, [T|_]) represents the time-points belonging to the overlapping part in which event E occurs. Delays and retractions are excluded.

happensAtProcessedStaticIE_Qi_wRetractions(Index, E, [T|_]) represents the time-points belonging to the overlapping part in which event E occurs. Retractions are included.


holdsForProcessedIEInc(Index, IE, L, Delays, OnTime, Retractions) retrieves the cached lists of intervals of an input entity/statically determined fluent.
L: The list of intervals falling in the overlapping part
Delays: The list of intervals falling in the overlapping part and calculated at Qi
OnTime: The list of intervals falling in the non-overlapping part and calculated at Qi
Retractions: The list of intervals falling in the overlapping part and retracted at Qi
-holdsForProcessedSimpleFluentInc(Index, F=V, L, Delays, OnTime, Retractions). Similar to the above but for simple fluents
-holdsForProcessedSDFluentInc(Index, F=V, L, Delays, OnTime, Retractions) Similar to the above but for statically determined fluents.

-holdsFor(F=V, L) is used for user interaction.
-holdsForDelays(Index, F=V, L) retrieves the list of intervals L falling in the the overlapping part and calculated at Qi.
-holdsForRetractions(Index, F=V, L) retrieves the list of intervals L falling in the the overlapping part and retracted at Qi.

-holdsAtProcessedSimpleFluent(Index, F=V, T) checks whether the intervals falling in the overlapping part of a cached simple fluent include a given time-point.
-holdsAtProcessedSimpleFluent_Delays(Index, F=V, T) checks whether the delayed intervals of a cached simple fluent include a given time-point.
-holdsAtProcessedSimpleFluent_OnTime(Index, F=V, T) checks whether the intervals falling in the non-overlapping part of a cached simple fluent include a given time-point.
-holdsAtProcessedSimpleFluent_Retractions(Index, F=V, T) checks whether the retracted intervals of a cached simple fluent include a given time-point.


-holdsAtProcessedSDFluent(Index, F=V, T) checks whether the intervals falling in the overlapping part of a cached output entity/statically determined fluent include a given time-point.
-holdsAtProcessedSDFluent_Delays(Index, F=V, T) checks whether the delayed intervals of a cached statically determined fluent include a given time-point.
-holdsAtProcessedSDFluent_OnTime(Index, F=V, T) checks whether the intervals falling in the non-overlapping part of a cached statically determined fluent include a given time-point.
-holdsAtProcessedSDFluent_Retractions(Index, F=V, T) checks whether the retracted intervals of a cached statically determined fluent include a given time-point.

-holdsAt(F=V, T) is used for user interaction.

NOTE: statically determined fluents are defined only in terms of interval manipulation constructs, ie they are not defined by means of holdsAt.
NOTE: The second argument in holdsAtX query should be ground.

DECLARATIONS:

-event(E) states that E is an event.
-event2(E) states that E is an event of type 2.
-simpleFluent(F=V) states that F=V is a simple fluent.
-sDFluent(F=V) states that F=V is a statically determined fluent of type 1.
-sDFluent2(F=V) states that F=V is a statically determined fluent of type 2.

-keyOfEvent(E, K) states that the key ofr event E is K (for use with the internal database).
-dependency(F=V, L) states that L is the list of body literals used in the definitions of fluent F=V.
-secondIndex(F=V) states that F is a relational fluent.

-inputEntity(U) represents the input entities (events and/or statically determined fluents).
-outputEntity(U) represents the composite entities (events, simple fluents and/or statically determined fluents).

-rgraph(F=V, R, B) states that the values in B correspond to a rule of simple fluent F=V in R.
-mgraph(F=V, L, I, D, O, R) states that the intervals (I, D, O, R) of statically determined fluent of type 2 F=V can be retrieved from L.

-collectIntervals(F=V) states that the list of intervals of input entity/statically determined fluent F=V will be produced by the RTEC input module by collecting the reported individual intervals
 
-temporalDistance(TD) denotes the temporal distance between consecutive time-points. In some applications, such as video surveillance, there is a fixed temporal distance between time-points (video frames). In other applications this is not the case and therefore temporalDistance/1 should be undefined.

-cachingOrder(Index, U) denotes the order of entity (event or fluent) processing. The first argument is the index of the entity.
*/

% ========================

:- set_prolog_flag(toplevel_print_options, [max_depth(400)]).
%:- set_prolog_flag(unknown, fail).

:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(terms)).

:- ['compiler.prolog'].
:- ['inputModule.prolog'].
:- ['processSimpleFluentsInc.prolog'].
:- ['processSDFluentsInc.prolog'].
:- ['utilities/interval-manipulation.prolog'].
:- ['utilities/interval-manipulation-incremental.prolog'].
:- ['utilities/amalgamate-periods.prolog'].
:- ['utilities/auxiliary_functions.prolog'].

:- dynamic temporalDistance/1, input/1, preProcessing/1, initTime/1.


:- discontiguous updateSDE/4, happensAtProcessedIE/3, happensAtProcessedSDFluent/3, event/1, event2/1, inputEntity/1, index/2, secondIndex/1, outputEntity/1, simpleFluent/1, sDFluent/1, keyOfEvent/2, sDFluent2/1, dependency/2, rgraph/3, mgraph/6.


/********************************** INITIALISE RECOGNITION ***********************************/


initialiseRecognition(InputFlag, PreProcessingFlag, TemporalDistance) :-
	assert(temporalDistance(TemporalDistance)),
	(InputFlag=ordered, assert(input(InputFlag)) ; assert(input(unordered))),
	% if we need preprocessing then preProcessing/1 is already defined
	% so there is no need to assert anything here
	(PreProcessingFlag=preprocessing ; assert(preProcessing(_,_,_))), !.


/************************************* EVENT RECOGNITION *************************************/


eventRecognition(QueryTime, WM, Step) :-
	InitTime is QueryTime-WM,
	assert(initTime(InitTime)),
	nextTimePoint(InitTime, NewInitTime),
	Q is QueryTime - Step,
	nextTimePoint(Q, PrevQueryTime),
	% compute the intervals of input entities/statically determined fluents
	% writeln('Start....'),
	inputProcessing(InitTime, QueryTime, NewInitTime, PrevQueryTime),
	% the order in which entities are processed makes a difference
	% start from lower-level entities and then move to higher-level entities
	% in this way the higher-level entities will use the CACHED lower-level entities
	% the order in which we process entities is set by cachingOrder/1 
	% which is specified in the domain-dependent file 
	% cachingOrder2/2 is produced in the compilation stage 
	% by combining cachingOrder/1, indexOf/2 and grounding/1
	findall(OE, (cachingOrder2(Index,OE), processEntity(Index,OE,InitTime,NewInitTime,PrevQueryTime)), _),
	retract(initTime(InitTime)).


processEntity(Index, OE, InitTime, NewInitTime, PrevQueryTime) :-
	(
		% compute the intervals of output entities/statically determined fluents
		sDFluent(OE),
		getCPUTime(ST,yap),
		processSDFluentInc(Index, OE, InitTime, NewInitTime, PrevQueryTime),
		getCPUTime(ET,yap),
		T is ET - ST, nb_getval(window_sdf, LT), FLT is LT + T, nb_setval(window_sdf, FLT)
		;
		% compute the intervals of output entities/statically determined fluents of type 2
		sDFluent2(OE),
		getCPUTime(ST,yap),
		processSDFluent2Inc(Index, OE, InitTime, NewInitTime, PrevQueryTime),
		getCPUTime(ET,yap),
		T is ET - ST, nb_getval(window_sdf, LT), FLT is LT + T, nb_setval(window_sdf, FLT)
		;
		% compute the intervals of simple fluents 
		% (simple fluents are by definition output entities) 
		simpleFluent(OE),
		getCPUTime(ST,yap),
		processSimpleFluentInc(Index, OE, InitTime, NewInitTime, PrevQueryTime),
		getCPUTime(ET,yap),
		T is ET - ST, nb_getval(window_simple, LT), FLT is LT + T, nb_setval(window_simple, FLT)
		;
		% compute the time-points of output entities/events
		processEvent(Index, OE)
	), !.

% When the event recognition in the current window finishes we must compute the new materialization. This means that delayed and on time entries should be retracted and be asserted again in order to be part of the overlap of the next window.
computeNewMaterialization :-
	dealWithInsertions.
%	dealWithDeletions(LK).


dealWithInsertions :-
	findall(K, (inputEntity(E), keyOfEvent(E,K),(event(E);event2(E))), LK),
	assertDelayedEntries(LK),
	assertOnTimeEntries(LK),
	removeDelayedEntries(LK),
	removeOnTimeEntries(LK).


assertDelayedEntries([]) :- !.

assertDelayedEntries([Key|RK]) :-
	atomic_list_concat(['010', Key], Dd),
	atomic_list_concat(['100', Key], Dold),
	findall(E, (recorded(Dd, E, _R), recordz(Dold, E, _Rold)), _),
	assertDelayedEntries(RK).

assertOnTimeEntries([]) :- !.

assertOnTimeEntries([Key|RK]) :-
	atomic_list_concat(['011', Key], Don),
	atomic_list_concat(['100', Key], Dold),
	findall(E, (recorded(Don, E, _R), recordz(Dold, E, _Rold)), _),
	assertOnTimeEntries(RK).

removeDelayedEntries([]) :- !.

removeDelayedEntries([Key|RK]) :-
	atomic_list_concat(['010', Key], Dd),
	eraseall(Dd),
	removeDelayedEntries(RK).

removeOnTimeEntries([]) :- !.

removeOnTimeEntries([Key|RK]) :-
	atomic_list_concat(['011', Key], Don),
	eraseall(Don),
	removeOnTimeEntries(RK).


/******************* entity index: use of cut to avoid backtracking *********************/

indexOf(Index, E) :-
	index(E, Index), !.

/******************* APPLICATION-INDEPENDENT holdsFor, holdsAt AND happensAt (INCARNATIONS) *********************/

happensAtProcessedStaticIE_Delays(Index, F, T) :-
	keyOfEvent(F, Key),
    % '010' is used as a prefix for delays
	atomic_list_concat(['010', Key], Dd),
	recorded(Dd, [Index|T], _).

happensAtProcessedStaticIE_OnTime(Index, F, T) :-
    keyOfEvent(F, Key),
    % '011' is used as a prefix for on time arrivals
    atomic_list_concat(['011', Key], Don),
    recorded(Don, [Index|T], _).

happensAtProcessedStaticIE_Retractions(Index, F, T) :-
	keyOfEvent(F, Key),
    % '001' is used as a prefix for retractions
	atomic_list_concat(['001', Key], Dr),
	recorded(Dr, [Index|T], _).

happensAtProcessedStaticIE_Qi_notDelays(Index, F, T) :-
	keyOfEvent(F, Key),
	atomic_list_concat(['100', Key], Dold),
	atomic_list_concat(['001', Key], Dr),
	recorded(Dold, [Index|T], _),
	\+ recorded(Dr, [Index|T], _).
     

happensAtProcessedStaticIE_Qi_wRetractions(Index, F, T) :-
	keyOfEvent(F, Key),
	atomic_list_concat(['100', Key], Dold),
	atomic_list_concat(['010', Key], Dd),
	(
		recorded(Dold, [Index|T], _)
		;
		recorded(Dd, [Index|T], _)
	), !.

happensAtProcessedStaticIE_Over(Index, F, T) :-
	keyOfEvent(F, Key),
    % '100' is used as a prefix for events occurring in the overlap
	atomic_list_concat(['100', Key], Dold),
	atomic_list_concat(['001', Key], Dr),
	atomic_list_concat(['010', Key], Dd),
	(
		recorded(Dold, [Index|T], _), \+ recorded(Dr, [Index|T], _)
		;
		recorded(Dd, [Index|T], _)
	).


%%%%%%% holdsFor as used in the body of entity definitions

% processed input entity/statically determined fluent

holdsForProcessedIEInc(Index, IE, L, Delays, OnTime, Retractions) :-
	keyOfEvent(IE, Key),
	atomic_list_concat(['100', Key], Dkey),
	createIndex(IE, IE2),
	checkCounter(Dkey, Dkey2),
	recorded(Dkey2, iePList(IE2, Index, L, _, Delays, OnTime, Retractions), _), !.

holdsForProcessedIEInc(_Index, _IE, [], [], [], []).

% cached simple fluent

holdsForProcessedSimpleFluentInc(Index, F=V, L, Delays, OnTime, Retractions) :-
	keyOfEvent(F=V, Key),
	recorded(Key, simpleFPList(Index, F=V, _, _, L, _, Delays, OnTime, Retractions), _), !.

holdsForProcessedSimpleFluentInc(_Index, _U, [], [], [], []).

% cached output entity/statically determined fluent

holdsForProcessedSDFluentInc(Index, F=V, L, Delays, OnTime, Retractions) :-
	keyOfEvent(F=V, Key),
	recorded(Key, sdFPList(Index, F=V, L, _, Delays, OnTime, Retractions), _), !.

holdsForProcessedSDFluentInc(_Index, _U, [], [], [], []).

%%%%%%% holdsAt as used in the body of entity definitions

% T should be given in all 4 predicates below

% processed input entity/statically determined fluent
/* the predicates below are not used

holdsAtProcessedIE(Index, F=V, T) :-
	keyOfEvent(F=V, Key),
	atomic_list_concat(['100', Key], Dkey),
	recorded(Dkey, iePList(Index, F=V, [H|Tail], _, _, _, _), _), !,
	tinIntervals(T, [H|Tail]).

holdsAtProcessedIE_Delays(Index, F=V, T) :-
	keyOfEvent(F=V, Key),
	atomic_list_concat(['100', Key], Dkey),
	recorded(Dkey, iePList(Index, F=V, _, _, [H|Tail], _, _), _), !,
	tinIntervals(T, [H|Tail]).

holdsAtProcessedIE_OnTime(Index, F=V, T) :-
	keyOfEvent(F=V, Key),
	atomic_list_concat(['100', Key], Dkey),
	recorded(Dkey, iePList(Index, F=V, _, _, _, [H|Tail], _), _), !,
	tinIntervals(T, [H|Tail]).

holdsAtProcessedIE_Retractions(Index, F=V, T) :-
	keyOfEvent(F=V, Key),
	atomic_list_concat(['100', Key], Dkey),
	recorded(Dkey, iePList(Index, F=V, _, _, _, _, [H|Tail]), _), !,
	tinIntervals(T, [H|Tail]).

*/

% cached simple fluent

holdsAtProcessedSimpleFluent(Index, F=V, T) :-
	keyOfEvent(F=V, Key),
	recorded(Key, simpleFPList(Index, F=V, _, _, [H|Tail], _, _, _, _), _), !,
	tinIntervals(T, [H|Tail]).

holdsAtProcessedSimpleFluent_Retractions(Index, F=V, T) :-
	keyOfEvent(F=V, Key),
	recorded(Key, simpleFPList(Index, F=V, _, _, _, _, _, _, [H|Tail]), _), !,
	tinIntervals(T, [H|Tail]).

holdsAtProcessedSimpleFluent_Delays(Index, F=V, T) :-
	keyOfEvent(F=V, Key),
	recorded(Key, simpleFPList(Index, F=V, _, _, _, _, [H|Tail], _, _), _), !,
	tinIntervals(T, [H|Tail]).

holdsAtProcessedSimpleFluent_OnTime(Index, F=V, T) :-
	keyOfEvent(F=V, Key),
	recorded(Key, simpleFPList(Index, F=V, _, _, _, _, _, [H|Tail], _), _), !,
	tinIntervals(T, [H|Tail]).
    

% cached output entity/statically determined fluent

holdsAtProcessedSDFluent(Index, F=V, T) :-
	keyOfEvent(F=V, Key),
	recorded(Key, sdFPList(Index, F=V, [H|Tail], _, _, _, _), _), !,
	tinIntervals(T, [H|Tail]).

holdsAtProcessedSDFluent_Retractions(Index, F=V, T) :-
	keyOfEvent(F=V, Key),
	recorded(Key, sdFPList(Index, F=V, _, _, _, _, [H|Tail]), _), !,
	tinIntervals(T, [H|Tail]).

holdsAtProcessedSDFluent_Delays(Index, F=V, T) :-
	keyOfEvent(F=V, Key),
	recorded(Key, sdFPList(Index, F=V, _, _, [H|Tail], _, _), _), !,
	tinIntervals(T, [H|Tail]).

holdsAtProcessedSDFluent_OnTime(Index, F=V, T) :-
	keyOfEvent(F=V, Key),
	recorded(Key, sdFPList(Index, F=V, _, _, _, [H|Tail], _), _), !,
	tinIntervals(T, [H|Tail]).

/* the predicates below are not used for now
%%%%%%% happensAt as used in the body of entity definitions

%%%% special event: the starting time of a fluent

%%% in each case below (input entity/statically determined fluent, simple fluent 
%%% and output entity/statically determined fluent), the first rule checks if 
%%% the first interval in (Qi-WM, Qi] is amalgamated with the last interval before Qi-WM
%%% If it is then start(F=V) does not take place at the starting time 
%%% of the first interval in (Qi-WM, Qi]

% compute the starting points of processed input entities/statically determined fluents

happensAtProcessedIE(Index, start(F=V), S) :-
	keyOfEvent(F=V, Key),
	atomic_list_concat(['100', Key], Dkey),
	happensAtProcessedIE2(Dkey, Index, F=V, S).

happensAtProcessedIE2(Dkey, Index, F=V, S) :-
	recorded(Dkey, iePList(Index, F=V, [(IntervalBreakingPoint,End)|Tail], [(_,IntervalBreakingPoint)], _, OnTime, _), _), !,
	iset_union([(IntervalBreakingPoint,End)|Tail], OnTime, FL),
	member((S,_E), FL), S > IntervalBreakingPoint.

happensAtProcessedIE2(Dkey, Index, F=V, S) :-
	recorded(Dkey, iePList(Index, F=V, L, [], _, OnTime, _), _), !,
	iset_union(L, OnTime, FL),
	member((S,_E), FL).


% compute the starting points of simple fluents

happensAtProcessedSimpleFluent(Index, start(F=V), S) :-
	keyOfEvent(F=V, Key),
	%atomic_list_concat(['100', Key], Dkey),
	happensAtProcessedSimpleFluent2(Key, Index, F=V, S).

happensAtProcessedSimpleFluent2(Dkey, Index, F=V, S) :-
	recorded(Dkey, simpleFPList(Index, F=V, _, _, [(IntervalBreakingPoint,End)|Tail], [(_,IntervalBreakingPoint)], _, OnTime, _), _), !,
	iset_union([(IntervalBreakingPoint,End)|Tail], OnTime, FL),
	member((S,_E), FL), S > IntervalBreakingPoint.

happensAtProcessedSimpleFluent2(Dkey, Index, F=V, S) :-
	recorded(Dkey, simpleFPList(Index, F=V, _, _, L, [], _, OnTime, _), _), !,
	iset_union(L, OnTime, FL),
	member((S,_E), FL).


happensAtProcessedSimpleFluent_Retractions(Index, start(F=V), S) :-
	keyOfEvent(F=V, Key),
	%atomic_list_concat(['100', Key], Dkey),
	recorded(Dkey, simpleFPList(Index, F=V, _, _, _, _, _, _, Retractions), _), !,
	member((S,_), Retractions).


% compute the starting points of output entities/statically determined fluents

happensAtProcessedSDFluent(Index, start(F=V), S) :-
	keyOfEvent(F=V, Key),
	%atomic_list_concat(['100', Key], Dkey),
	happensAtProcessedSDFluent2(Key, Index, F=V, S).

happensAtProcessedSDFluent2(Dkey, Index, F=V, S) :-
	recorded(Dkey, sdFPList(Index, F=V, [(IntervalBreakingPoint,End)|Tail], [(_,IntervalBreakingPoint)], _, OnTime, _), _), !,
	iset_union([(IntervalBreakingPoint,End)|Tail], OnTime, FL),
	member((S,_E), FL), S > IntervalBreakingPoint.

happensAtProcessedSDFluent2(Dkey, Index, F=V, S) :-
	recorded(Dkey, sdFPList(Index, F=V, L, [], _, OnTime, _), _), !,
	iset_union(L, OnTime, FL),
	member((S,_E), FL).

% start(F=V) is not defined for fluents that are neither input nor output entities, 
% ie fluents that are not cached
% For such fluents we do not have access to the last interval before Qi-WM 
% and therefore we cannot compute whether the last interval before Qi-WM 
% is amalgamated with the first interval in (Qi-WM,Qi]


%%%% special event: the ending time of a fluent

% compute the ending points of processed input entities/statically determined fluents

happensAtProcessedIE(Index, end(F=V), E) :-
	keyOfEvent(F=V, Key),
	atomic_list_concat(['100', Key], Dkey),
	recorded(Dkey, iePList(Index, F=V, L, _, _, OnTime, _), _), !,
	iset_union(L, OnTime, FL),
	member((_S,E), FL),
	\+ E=inf.

% compute the ending points of simple fluents

happensAtProcessedSimpleFluent(Index, end(F=V), E) :-
	keyOfEvent(F=V, Key),
	%atomic_list_concat(['100', Key], Dkey),
	recorded(Key, simpleFPList(Index, F=V, _, _, L, _, _, OnTime, _), _), !,
	iset_union(L, OnTime, FL),
	member((_S,E), FL),
	\+ E=inf.

happensAtProcessedSimpleFluent_Retractions(Index, end(F=V), E) :-
	keyOfEvent(F=V, Key),
	%atomic_list_concat(['100', Key], Dkey),
	recorded(Key, simpleFPList(Index, F=V, _, _, _, _, _, _, Retractions), _), !,
	member((_S,E), Retractions).

% compute the ending points of output entities/statically determined fluents

happensAtProcessedSDFluent(Index, end(F=V), E) :-
	keyOfEvent(F=V, Key),
	%atomic_list_concat(['100', Key], Dkey),
	recorded(Key, sdFPList(Index, F=V, L, _, _, OnTime, _), _), !,
	iset_union(L, OnTime, FL),
	member((_S,E), FL),
	\+ E=inf.

%%%% happensAtProcessed for non-special events

% cached events
happensAtProcessed(Index, E, T) :-
	evTList(Index, E, L),
	member(T, L).
*/

%%%%%%% USER INTERACTION %%%%%%%

%%%%%%% holdsFor is used ONLY for user interaction
%%%%%%% use iePList/simpleFPList/sdFPList and look no further

holdsForRetractions(Index, F=V, L) :-
	keyOfEvent(F=V, Key), !,
	recorded(Key, simpleFPList(Index, F=V, _, _, _, _, _, _, L), _), !.

holdsForDelays(Index, F=V, L) :-
	keyOfEvent(F=V, Key), !,
	recorded(Key, simpleFPList(Index, F=V, _, _, _, _, L, _, _), _), !.

holdsFor(F=V, L) :-
	retrieveIntervals(F=V, L).

% retrieve the intervals of input entities (those for which we collect their intervals)
retrieveIntervals(F=V, L) :-
	% collectIntervals2/2 is produced in the compilation stage 
	% by combining collectIntervals/1, indexOf/2 and grounding/1
	collectIntervals2(Index, F=V),
	retrieveIEIntervals(Index, F=V, L).


% retrieve the intervals of output entities
retrieveIntervals(F=V, L) :-
	% cachingOrder2/2 is produced in the compilation stage 
	% by combining cachingOrder/1, indexOf/2 and grounding/1
	cachingOrder2(Index, F=V),
	retrieveOEIntervals(Index, F=V, L).


retrieveIEIntervals(Index, F=V, L) :-
	]keyOfEvent(F=V, Key),
	atomic_list_concat(['100', Key], Dkey),
	createIndex(F=V, U),
	checkCounter(Dkey, Dkey2),
	recorded(Dkey2, iePList(U, Index, RestrictedList, Extension, _, OnTime, _), _), !,
	amalgamatePeriods(Extension, RestrictedList, Ltemp),
	iset_union(Ltemp, OnTime, L).

retrieveIEIntervals(_Index, _U, []).


retrieveOEIntervals(Index, F=V, L) :-
	sDFluent(F=V), !,
	retrieveOESDFluentIntervals(Index, F=V, L).

retrieveOEIntervals(Index, F=V, L) :-
	sDFluent2(F=V), !,
	retrieveOESDFluentIntervals2(Index, F=V, L).

retrieveOEIntervals(Index, F=V, L) :-
	keyOfEvent(F=V, Key),
	recorded(Key, simpleFPList(Index, F=V, _, _, RestrictedList, Extension, _, OnTime, _), _), !,
	amalgamatePeriods(Extension, RestrictedList, Ltemp),
	iset_union(Ltemp, OnTime, L).

retrieveOEIntervals(_Index, _U, []).


retrieveOESDFluentIntervals(Index, F=V, L) :-
	keyOfEvent(F=V, Key),
	recorded(Key, sdFPList(Index, F=V, RestrictedList, Extension, _, OnTime, _), _), !,
	amalgamatePeriods(Extension, RestrictedList, Ltemp),
	iset_union(Ltemp, OnTime, L).

retrieveOESDFluentIntervals(_Index, _U, []).


retrieveOESDFluentIntervals2(Index, F=V, L) :-
    keyOfEvent(F=V, Key),
	recorded(Key, sdFPList2(Index, F=V, _, RestrictedList, Extension, _, OnTime, _), _), !,
    amalgamatePeriods(Extension, RestrictedList, Ltemp),
    iset_union(Ltemp, OnTime, L).

retrieveOESDFluentIntervals2(_Index, _U, []).


%%%%%%% holdsAt is used ONLY for user interaction
% T should be given

holdsAt(F=V, T) :-
	holdsFor(F=V, [H|Tail]),
	tinIntervals(T, [H|Tail]).


tinIntervals(T, L) :-
	member((S,E), L),
	gt(E,T), !, S=<T.


%%%%%%% happensAt is used ONLY for user interaction

% retrieve the time-points of input entities
happensAt(E, T) :-
	%inputStatic(Index, E),
	keyOfEvent(E, Key),
	%atomic_list_concat(['100', Index, Key], Dkey),
	atomic_list_concat(['100', Key], Dold),
	recorded(Dold, [_Id|T], _).

happensAtIER(E, T) :-
	keyOfEvent(E, Key), !,
	atomic_list_concat(['001', Key], Kr),
	index(E, Index), !,
	recorded(Kr, [Index, T|_], _).

delay(E, T) :-
        keyOfEvent(E, Key), !,
        atomic_list_concat(['010', Key], Kd),
        index(E, Index), !,
        recorded(Kd, [Index, T|_], _).

% creates index for relational input entities
createIndex(E, K) :-
	secondIndex(E), !,
	term_hash(E, K).
