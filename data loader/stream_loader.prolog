:- use_module(library(apply_macros)).
:- ['manual_csv_read.prolog'].
	
loadIEStreams([_Stream], _StartPoint, _EndPoint, _PrevQ, [end_of_file], [end_of_file]) :- !.

loadIEStreams([Stream], StartPoint, EndPoint, PrevQ, [StreamPosition], [NewStreamPosition]) :-
    !,
    % set Stream as input
    set_input(Stream),
    loadSingleIEStream(Stream, StartPoint, EndPoint, PrevQ, StreamPosition, NewStreamPosition).

loadIEStreams([_Stream|Streams], StartPoint, EndPoint, PrevQ, [end_of_file|SPs], [end_of_file|NSPs]) :-
    !, loadIEStreams(Streams, StartPoint, EndPoint, PrevQ, SPs, NSPs).

loadIEStreams([Stream|Streams], StartPoint, EndPoint, PrevQ, [StreamPosition|SPs], [NewStreamPosition|NSPs]) :-
    % set Stream as input
    set_input(Stream),
    loadSingleIEStream(Stream, StartPoint, EndPoint, PrevQ, StreamPosition, NewStreamPosition),
    loadIEStreams(Streams, StartPoint, EndPoint, PrevQ, SPs, NSPs).


loadSingleIEStream(Stream, StartPoint, EndPoint, PrevQ, StreamPosition, NewStreamPosition) :-
	% does not exist in yap
	%csv_read_file_row(Stream,Row,[line(Line),separator(0'|)]),
	% so, use manual_csv_reader instead
	get_row_from_line(Stream, Row, StreamPosition, NextStreamPosition),
	%write('Processing row: '),writeln(Row),
	getRowArgument(2, Row, IEArrivalTime),
	processRow(Row, StartPoint, EndPoint, PrevQ, IEArrivalTime, ProcessingOutcome),
	(
        ProcessingOutcome = passed_endpoint, !,
        % the StreamPosition does not change
        NewStreamPosition = StreamPosition
        ;
        ProcessingOutcome = end_of_file, !,
        NewStreamPosition = end_of_file
        ;
        loadSingleIEStream(Stream, StartPoint, EndPoint, PrevQ, NextStreamPosition, NewStreamPosition)
	).

processRow([], _StartPoint, _EndPoint, _PrevQ, _IEArrivalTime, end_of_file) :- !.
	
processRow(Row, StartPoint, _EndPoint, _PrevQ, IEArrivalTime, keep_processing) :-
	StartPoint >= IEArrivalTime, !.
	
processRow(Row, StartPoint, EndPoint, PrevQ, IEArrivalTime, ProcessingOutcome) :-
	StartPoint < IEArrivalTime,
    (
        EndPoint >= IEArrivalTime, !,
        ProcessingOutcome = keep_processing,
        getIEFromRowandAssertIt(Row, PrevQ)
        ;
        ProcessingOutcome = passed_endpoint
    ).
        
        
% getIEFromRowandAssertIt(+Row)
% distill the input entity from Row and assert in the RTEC format
getIEFromRowandAssertIt(Row, PrevQ) :-
    % IElabel becomes the first argument of Row
    getRowArgument(1, Row, IElabel),
    (
        % check whether the given input entity is an event
        event(E), E=..[IElabel|_], inputEntity(E), !,
        % assertEvent(+Row),
        % distill from Row the event instance and assert it
        assertEvent(Row, PrevQ)
        ;
        % check whether the given input entity is an event of type 2
        event2(E), E=..[IElabel|_], inputEntity(E), !,
        % assertEvent(+Row),
        % distill from Row the event instance and assert it
        assertEvent2(Row, PrevQ)
        ;
        % check whether the given input entity is a statically determined fluent
        inputEntity(F=V), F=..[IElabel|_], sDFluent(F=V), !,
        % assertFluent(+Row)
        % distill from Row the fluent instance and assert it
        assertFluent(Row, PrevQ)
    ).

% Row contains neither an input event
% not an input statically determined fluent
getIEFromRowandAssertIt(Row, _PrevQ) :-
    write('ERROR IN INPUT CSV; LINE: '), writeln(Row).


% getRowArgument(+N, +Row, -Arg)
% return the Nth argument of row
% the built-in arg/3 raises exception in case of
% empty row, and thus we had to address this
getRowArgument(_N, [], []) :- !.
getRowArgument(N, Row, Arg) :-
    arg(N, Row, Arg).
    

% assertEvent(+Row, +PrevQ)
% distill from Row the event instance and assert it in the RTEC format
assertEvent(Row, PrevQ) :-
    % get rid of row atom and Arrival time
    Row =.. [_RowAtom|[EventLabel|[ArrivalTime|[OccurenceTime|[EventAttributes]]]]],
    Event =.. [EventLabel,EventAttributes],
    keyOfEvent(Event, Key),
    delayedOrOntime(ArrivalTime, PrevQ, Key, Dkey), !,
    recordz(Dkey, [EventAttributes, OccurenceTime], _).

% the event arrival time or occurrence time is missing
assertEvent(Row, _PrevQ) :-
    write('ERROR IN INPUT CSV; LINE: '), writeln(Row).
    
% assertEvent2(+Row, +PrevQ)
% distill from Row the event instance and assert it in the RTEC format
assertEvent2(Row, PrevQ) :-
    % get rid of row atom and Arrival time
    Row =.. [_RowAtom|[EventLabel|[ArrivalTime|[OccurenceTime|[Id|EventAttributes]]]]],
    Event =.. [EventLabel, Id],
    keyOfEvent(Event, Key),
    delayedOrOntime(ArrivalTime, PrevQ, Key, Dkey), !,
    recordz(Dkey, [Id, OccurenceTime | EventAttributes], _).

% the event arrival time or occurrence time is missing
assertEvent2(Row, _PrevQ) :-
    write('ERROR IN INPUT CSV; LINE: '), writeln(Row).


% distill from Row the durative instance of Fluent=Value and assert it in the RTEC format
assertFluent(Row, PrevQ) :-
    % get rid of row atom and arrival time
    Row =.. [_RowAtom|[FluentLabel|[ArrivalTime|[StartOccurenceTime|[EndOccurenceTime|[Value|FluentAttributes]]]]]],
    Fluent =.. [FluentLabel|FluentAttributes],
    keyOfEvent(Fluent=Value, Key), !,
    delayedOrOntimeFluent(ArrivalTime, PrevQ, Key, StartOccurenceTime, EndOccurenceTime, Fluent=Value).

% the fluent in the CSV file is not consistent with the declarations of the event description
% Note: we do not check the attributes of the fluent
assertFluent(Row, _PrevQ) :-
    write('ERROR IN INPUT CSV; LINE: '), writeln(Row).

% check if the entry is delayed or on time
delayedOrOntime(ArrivalTime, PrevQ, Key, Dkey) :-
	ArrivalTime >= PrevQ, !,
	atomic_list_concat(['011', Key], Dkey).

delayedOrOntime(_ArrivalTime, _PrevQ, Key, Dkey) :-
	atomic_list_concat(['010', Key], Dkey).

% check which intervals are delayed and which are not
delayedOrOntimeFluent(ArrivalTime, PrevQ, Key, ST, ET, F=V) :-
	ArrivalTime =< PrevQ, !,
	atomic_list_concat(['010', Key], Dkey),
	recordz(Dkey, [F=V, (ST, ET)], _).

delayedOrOntimeFluent(ArrivalTime, PrevQ, Key, ST, ET, F=V) :-
	ST < PrevQ, !,
	atomic_list_concat(['011', Key], Dokey),
	atomic_list_concat(['010', Key], Ddkey),
	recordz(Ddkey, [F=V, (ST, PrevQ)], _),
	recordz(Dokey, [F=V, (PrevQ, ET)], _).

delayedOrOntimeFluent(ArrivalTime, PrevQ, Key, ST, ET, F=V) :-
	atomic_list_concat(['011', Key], Dokey),
    recordz(Dokey, [F=V, (ST, ET)], _).
