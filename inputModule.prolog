:- use_module(library(aggregate)).

/******************************* FORGET MECHANISM *********************************/


% the rule below deals with the case in which the input stream is not temporally sorted

forget(InitTime) :-
	input(unordered), !,
	nextTimePoint(InitTime, NewInitTime),
	% forget input entities/events
	findall(K, 
		(
			inputEntity(Event), keyOfEvent(Event, Eid), atomic_list_concat(['100', Eid], K),
			recorded(K, Z, R), dealWithInputEvents(Event, K, Z, R, InitTime, NewInitTime)
		),
		 _).
   
% treat input events
% if the input event ends before Qi-WM then delete it
dealWithInputEvents(E, _K, Z, R, InitTime, _NewInitTime) :-
	event(E),
	Z = [_, T], !,
	T =< InitTime,
	erase(R).

dealWithInputEvents(E, _K, Z, R, InitTime, _NewInitTime) :-
	event2(E),
	Z = [_, T|_Tail], !,
	T =< InitTime,
	erase(R).

% treat input fluents
dealWithInputEvents(E, K, Z, R, InitTime, NewInitTime) :-
	sDFluent(E),
	Z = [F=V, (Start,End)], !,
	dealWithInputFluents(K, F=V, R, Start, End, InitTime, NewInitTime).

% if the input fluent starts after Qi-WM then keep it (do nothing)
dealWithInputFluents(_K, _H, _R, Start, _End, InitTime, _NewInitTime) :-
	Start>InitTime, !.

% if the input fluent ends before Qi-WM then delete it
dealWithInputFluents(_K, _H, R, _Start, End, InitTime, _NewInitTime) :-
	End=<InitTime,
	erase(R), !.

% if the input fluent starts before or on Qi-WM and ends after Qi-WM then break it
dealWithInputFluents(K, F=V, R, _Start, End, _InitTime, NewInitTime) :-
	erase(R), !,
	\+ NewInitTime=End,
	recordz(K, [F=V, (NewInitTime,End)], _).


/************************************************************************************************** 
 Compute the list of intervals of input entities/statically determined fluents.
 If the intervals of the input entities are provided then RTEC simply collects these intervals 
 and stores them in a list --- see 'collectIntervals' flag.
 **************************************************************************************************/

inputProcessing(InitTime, QueryTime, NewInitTime, PrevQueryTime) :-
	% collect the input entity/statically determined fluent intervals into a list	
	findall(F=V, 
		(
			keyOfEvent(F=V,Key),
			atomic_list_concat(['100', Key], Dkey),
			processIECollectI(Dkey, Index, Key, F=V, InitTime, NewInitTime, PrevQueryTime)
		), _).


%%%%%%% processIECollectI

processIECollectI(Dkey, Index, Key, F=V, InitTime, NewInitTime, PrevQueryTime) :-
	createIndex(F=V, U),
	checkCounter(Dkey, Dkey2),
	recorded(Dkey2, iePList(U, Index, RestrictedList, Extension, _, PrOnTime, _), R), !,
	erase(R),
	amalgamatePeriods(Extension, RestrictedList, Ltemp),
	iset_union(Ltemp, PrOnTime, ExtendedPList),
	% the predicate below is defined in processSDFluentsInc.prolog
	setTheSceneSDFluent(ExtendedPList, InitTime, BrokenPeriod, NRL),
	holdsForIE(collectIntervals, Key, F=V, Delays, NewInitTime),
	iset_union(NRL, Delays, OverPeriods),
	holdsForIE_OnTime(collectIntervals, Key, F=V, OnTime),
	updateiePList(Index, Dkey2, U, OverPeriods, BrokenPeriod, Delays, OnTime, []).

% this predicate deals with the case where no intervals for F=V were computed at the previous query time
processIECollectI(Dkey, Index, Key, F=V, _InitTime, NewInitTime, _PrevQueryTime) :-
	increaseCounter(Dkey, Dkey2),
	createIndex(F=V, U),
	holdsForIE(collectIntervals, Key, F=V, OverPeriods, NewInitTime),
	holdsForIE_OnTime(collectIntervals, Key, F=V, OnTime),
	updateiePList(Index, Dkey2, U, OverPeriods, [], OverPeriods, OnTime, []).

checkCounter(Dkey, Dkey2) :-
	get_value(pr, V),
	S is ceiling(V/5000),
	between(1,S,E),
	atomic_list_concat([Dkey, E], Dkey2).

increaseCounter(Dkey, Dkey2) :-
	get_value(pr, V),
	NV is V+1,
	set_value(pr, NV),
	S is ceiling(V/5000),
	atomic_list_concat([Dkey, S], Dkey2).	


%%%%%%% updateiePList

% if no IE intervals have been computed then do not assert anything
updateiePList(_Index, _DKey, _U, [], [], [], [], []) :- !.

updateiePList(Index, DKey, IE, NewPeriods, BrokenPeriod, Delays, OnTime, Retractions) :-
	recordz(DKey, iePList(IE, Index, NewPeriods, BrokenPeriod, Delays, OnTime, Retractions), _).


%%%%%%% holdsForIE --- collectIntervals

% collect the list of delayed intervals; setof sorts the list of intervals
holdsForIE(collectIntervals, Key, F=V, OverPeriods, InitTime) :-
	atomic_list_concat(['010', Key], Dkey),
	setof((S,E), (checkIE(Dkey, F=V, (S,E)), E >= InitTime), OverPeriods), !.

% if there are no delayed intervals in the input then setof will fail
% in this case return the empty list of intervals
holdsForIE(collectIntervals, _Key, _U, [], _InitTime).


% collect the list of intervals that belong to the non-overlapping part of two consecutive query times; setof sorts the list of intervals
holdsForIE_OnTime(collectIntervals, Key, F=V, OnTime) :-
	atomic_list_concat(['011', Key], Dokey),
	setof((S,E), checkIE(Dokey, F=V, (S,E)), OnTime), !.

% if there are no intervals in the non-overlapping part then setof will fail
% in this case return the empty list of intervals
holdsForIE_OnTime(collectIntervals, _Key, _U, []).

% retrieve from memory an interval and delete it
checkIE(Dkey, F=V, (S,E)) :-
	recorded(Dkey, [F=V, (S,E)], R),
	erase(R).
