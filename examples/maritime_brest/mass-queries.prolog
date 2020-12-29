%:- use_module(library(listing)).
%:- yap_flag(update_semantics,logical).
%:- use_module(library(cleanup)).
%:- yap_flag(gc_trace, very_verbose).
:- ['./utils/globals.prolog'].

performFullER(IEFileName, TimesFilename, ResultFilename, InitPoint, WM, Step, LastTime) :-
    % Initialization
    write('Initialization...'),
    nb_getval(sts, STS),
    %nb_setval(sfpl_at, 0),
    stats_init(STS, TimesFilename, ResultFilename),
    initialiseRecognition(unordered, nopreprocessing, 1),
    InitWin is InitPoint + WM,
    openInputFiles(IEFileName, IEStreams, IEPositions),
    writeln('done.'),
    getEvents(IEStreams, InitPoint, InitWin, -1, IEPositions, IEPositions1),
    InitWinPlus1 is InitWin+1,
    write('IER: '), write(InitWin), write(' '), write(InitWinPlus1), nl,
    %stats_on(STS,Son,InitWin,InitWinPlus1),
    InitTime is InitWin-InitWinPlus1,
    dynamicGrounding,
    eventRecognition(InitWin,InitWinPlus1,Step),
    computeNewMaterialization,
    %stats_off(STS,Son,_CC,_ListofTimePoints,InitWin,InitWinPlus1),
    nb_setval(window_sdf, 0),
    nb_setval(window_simple, 0),
    nb_setval(window_union, 0),
    nb_setval(window_inter, 0),
    nb_setval(window_comp, 0),
    %aggregate_all(count, simpleFPList(_,_,_,_,_,_,_,_,_), NS), write('In iRTEC # of SFPL assertions is: '), writeln(NS),
    %nb_getval(sfpl_at, TS), ASA is TS / NS,
    %write('In iRTEC time of total SFPL assertions is: '), writeln(TS),
    %write('In iRTEC average time of an SFPL assertion is: '), writeln(ASA),
    %retractall(simpleFPList(_,_,[],_,[],_,_,_,_)),
    %retractall(sdFPList(_,_,[],_,_,[],_)),
    %retractall(sdFPList2(_,_,[],[],_,[],_)),
    %retractall(iePList(_,_,[],_,_,[],_)),
    % Continue
    CurrentTime is InitWin+Step,
    NextInit is InitPoint + Step,
    nextTimePoint(InitWin,PrevQ),

    %cleanup_all,
    %garbage_collect,
  
    getEvents(IEStreams, NextInit, CurrentTime, PrevQ, IEPositions1, NewPositions), !,
    querying(IEStreams, NewPositions, WM, Step, CurrentTime, LastTime),

    %cleanup_all,
    %garbage_collect,
  
    % Close/Clean-up
    writeln('ER done. Closing/Cleaning up...'),
    stats_end(STS),
    closeInputFiles(IEStreams),!.


querying(_IEStreams, _Positions, _WM, _Step, CurrentTime, LastTime) :-
    CurrentTime > LastTime,
    !.

querying(IEStreams, Positions, WM, Step, CurrentTime, LastTime) :-
    nb_getval(sts, STS),
    %nb_setval(sfpl_at, 0),
    RemainingSteps is round((LastTime-CurrentTime)/Step),
    write('IER: '), write(CurrentTime), write(' '), write(WM), write(' Remaining steps: '), write(RemainingSteps), nl,
  
    stats_on(STS, Son,CurrentTime, WM),
    InitTime is CurrentTime-WM,
    % delete input entities that have taken place before or on Qi-WM
    forget(InitTime),
    dynamicGrounding,
  
    eventRecognition(CurrentTime, WM,Step),
    computeNewMaterialization,
    stats_off(STS,Son,_CC,_ListofTimePoints,CurrentTime,WM),
    NextInit is InitTime + Step,
    update_global_vars(overlapping, NextInit),
  
    NewCurrentTime is CurrentTime+Step,
    nextTimePoint(CurrentTime, PrevQ),

    %aggregate_all(count, simpleFPList(_,_,_,_,_,_,_,_,_), NS), write('In iRTEC # of SFPL assertions is: '), writeln(NS),
    %nb_getval(sfpl_at, TS), ASA is TS / NS,
    %write('In iRTEC time of total SFPL assertions is: '), writeln(TS),
    %write('In iRTEC average time of an SFPL assertion is: '), writeln(ASA),
    %retractall(simpleFPList(_,_,[],_,[],_,_,_,_)),
    %retractall(sdFPList(_,_,[],_,_,[],_)),
    %retractall(sdFPList2(_,_,[],[],_,[],_)),
    %retractall(iePList(_,_,[],_,_,[],_)),

    %cleanup_all,
    %garbage_collect,
    %  garbage_collect_atoms,
    getEvents(IEStreams, NextInit, NewCurrentTime, PrevQ, Positions, NewPositions), !,
    %cleanup_all,
    %garbage_collect,
    %  garbage_collect_atoms,
    querying(IEStreams, NewPositions, WM, Step, NewCurrentTime, LastTime).

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Event I/O Utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

writeStacksInfo :-
    nb_getval(times_stream,Stream),
    statistics('global_stack',[GUsed,GFree]),
    statistics('local_stack',[LUsed,LFree]),
    statistics('trail',[TUsed,TFree]),
    GUg is GUsed/1073741824,
    GFg is GFree/1073741824,
    LUg is LUsed/1073741824,
    LFg is LFree/1073741824,
    TUg is TUsed/1073741824,
    TFg is TFree/1073741824,
    write(Stream,'GLOBAL (Used/Free): '),write(Stream,GUg),write(Stream,'/'),writeln(Stream,GFg),
    write(Stream,'LOCAL (Used/Free): '),write(Stream,LUg),write(Stream,'/'),writeln(Stream,LFg),
    write(Stream,'TRAIL (Used/Free): '),write(Stream,TUg),write(Stream,'/'),writeln(Stream,TFg).
  

getEvents(IEStreams, Start, End, PrevEnd, Positions, NewPositions) :-
    loadIEStreams(IEStreams, Start, End, PrevEnd, Positions, NewPositions), !.


writeCEs(ResultStream, []) :-
    nl(ResultStream), !.
    
writeCEs(ResultStream, [(_CE,[])|OtherCCs]) :-
    writeCEs(ResultStream, OtherCCs), !.
    
writeCEs(ResultStream, [(F=V,L)|OtherCCs]) :-
    write(ResultStream, '('),
    write(ResultStream, F),
    write(ResultStream, '='),
    write(ResultStream, V),
    write(ResultStream,', '),
    write(ResultStream, L),
    write(ResultStream, ').'),
    nl(ResultStream),
    writeCEs(ResultStream,OtherCCs).

writeCEinLines(ResultStream, CE, []).
writeCEinLines(ResultStream, CE, [(A,B)|L]):-
    B\=inf,
    writeElementsSeparated(ResultStream,[CE,A,B]),
    writeCEinLines(ResultStream,CE,L),!.
writeCEinLines(ResultStream,CE,[(A,B)|L]):-
    B==inf,
    writeElementsSeparated(ResultStream,[CE,A,B]),
    writeCEinLines(ResultStream,CE,L),!.
writeCEinLines(ResultStream,CE,[A|L]):-
    writeElementsSeparated(ResultStream,[CE,A,A]),!.

writeElementsSeparated(ResultStream, []).
writeElementsSeparated(ResultStream, [E]):-
    write(ResultStream, E),
    nl(ResultStream).
writeElementsSeparated(ResultStream, [E|L]):-
    write(ResultStream,E),
    write(ResultStream,'|'),
    writeElementsSeparated(ResultStream,L).
    
openInputFiles([File], [Stream], [Position]) :-
    open(File, read, Stream),
    stream_property(Stream, position(Position)), !.
    
closeInputFiles([Stream]) :-
    close(Stream).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Statistics Utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% CT in microsecs
getCPUTime(CT, yap) :-
    statistics(walltime,[CS,_T1]),
    CT is round(CS), !.
    
getCPUTime(CT, swipl) :-
    statistics(runtime, [CS, _T]),
    CT is round(CS), !.

stats_init(false, _TimesFilename, _ResultFilename) :- !.
stats_init(true, TimesFilename, ResultFilename) :-
    open(TimesFilename, write, TimesStream),
    open(ResultFilename, write, ResultStream),
    nb_setval(times_stream, TimesStream),
    nb_setval(results_stream, ResultStream).

stats_on(false, _Son, _QT, _WM) :- !.
stats_on(true, Son, QT, WM) :-
    nb_getval(results_stream, ResultStream),
    nb_getval(prl, PRL),
    getCPUTime(Son, PRL).

stats_off(false, _Son, [], [], CurrentTime, WM) :- !.
stats_off(true, Son, CC, ListofTimePoints, CurrentTime, WM) :-
    writeln('Collecting stats...'),
    StartTime is CurrentTime-WM,
    findall((F=V, L), (outputEntity(F=V), holdsFor(F=V,L), L\==[]), CC), %rm-dups(CC,CC2), length(CC2,X), writeln(X),
    findall((EE,[TT]), (outputEntity(EE),happensAt(EE,TT)), ListofTimePoints), %length(ListofTimePoints, X), writeln(X),
    nb_getval(prl, PRL),
    getCPUTime(Soff, PRL),
    S is Soff-Son,
    get_statistics(PRL, GUsed, LUsed, TUsed, HUsed, MUnshared, AtomsNo, AtomsMem),
    update_global_vars(hles, CC, ListofTimePoints),
    update_global_vars(lles),
    update_global_vars(time, S),
    update_global_fluents,
    update_global_vars(mems, GUsed, LUsed, TUsed, HUsed, MUnshared, AtomsNo, AtomsMem),
    nb_getval(results_stream, ResultStream),
    writeCEs(ResultStream, CC), writeCEs(ResultStream, ListofTimePoints).


get_statistics(yap, GUsed, LUsed, TUsed, HUsed, MUnshared, AtomsNo, AtomsMem):-
    statistics(global_stack, [GUsed,_GFree]),
    statistics(local_stack, [LUsed,_LFree]),
    statistics(trail, [TUsed,_TFree]),
    statistics(heap, [HUsed,_HFree]),
    statistics(program, [MUnshared,_MFree]),
    statistics(atoms, [AtomsNo,AtomsMem]).

get_statistics(swipl, GUsed, LUsed, TUsed, HUsed, MUnshared, AtomsNo, AtomsMem):-
    statistics(global_stack, [GUsed,_GFree]),
    statistics(local_stack, [LUsed,_LFree]),
    statistics(trail, [TUsed,_TFree]),
    statistics(heapused, HUsed),
    statistics(memory, [MUnshared,_MFree]),
    statistics(atoms, [AtomsNo,AtomsMem]).

stats_end(false) :- !.
stats_end(true) :-
    nb_getval(times_stream, TimesStream),
    nb_getval(results_stream, ResultStream),
    writeStats(ResultStream, TimesStream),
    close(TimesStream), close(ResultStream).

update_global_vars(mems, GUsed, LUsed, TUsed, HUsed, MUnshared, AtomsNo, AtomsMem) :-
    nb_getval(gstack, GList),
    append(GList, [GUsed], NewGList),
    nb_setval(gstack, NewGList),
    nb_getval(lstack, LList),
    append(LList, [LUsed], NewLList),
    nb_setval(lstack, NewLList),
    nb_getval(tstack, TList),
    append(TList, [TUsed], NewTList),
    nb_setval(tstack, NewTList),
    nb_getval(heap, HList),
    append(HList, [HUsed], NewHList),
    nb_setval(heap, NewHList),
    nb_getval(memunshared, MUList),
    append(MUList, [MUnshared], NewMUList),
    nb_setval(memunshared, NewMUList),
    nb_getval(atomsno, ANList),
    append(ANList, [AtomsNo], NewANList),
    nb_setval(atomsno, NewANList),
    nb_getval(atomsmem, AMList),
    append(AMList, [AtomsMem], NewAMList),
    nb_setval(atomsmem, NewAMList).

update_global_vars(hles, HoldsHLEs, HappensHLEs):-
    length(HoldsHLEs, HoldsNo),
    length(HappensHLEs, HappensNo),
    nb_getval(hleinstances, HLEs),
    TotalNo is HoldsNo + HappensNo,
    write('Ihles: '), writeln(TotalNo),
    append(HLEs, [TotalNo], NewHLEs),
    nb_setval(hleinstances, NewHLEs).
 
update_global_vars(lles):-
  
    findall(T, (event(F), happensAt(F, [T])), Hap), length(Hap, Happens), %write('Ille: '), writeln(Happens),
    findall(X, (inputEntity(F=V), F \= proximity(_,_), holdsFor(F=V,L), length(L,X)), HF), sum_list(HF, HoldsFor),
    findall(X, (inputEntity(F=V), F = proximity(_,_), holdsFor(F=V,L), L \== [], length(L,X)), SHF),  sum_list(SHF, SpHoldsFor), %writeln(SpHoldsFor),
  
    findall(T, (event2(F), (F = entersArea(_) ; F = leavesArea(_)), happensAt(F, T)), SpHap),
    length(SpHap, SpHappens), %writeln(SpHappens),
  
    HapLength is Happens + HoldsFor,
    SpLength is SpHappens + SpHoldsFor,
    nb_getval(inputdata,InData),
    nb_getval(spdata,SpData),
    append(InData, [HapLength], NewInData),
    append(SpData, [SpLength], NewSpData),
    nb_setval(inputdata, NewInData),
    nb_setval(spdata, NewSpData).

update_global_vars(time,Time):-
    nb_getval(time, TList),
    append(TList, [Time], NewTList),
    nb_setval(time, NewTList).

update_global_vars(inp_time, Time):-
    nb_getval(inp_time, TList),
    append(TList, [Time], NewTList),
    nb_setval(inp_time, NewTList).

update_global_vars(overlapping, NextInit) :-

    findall(T, (event(F), happensAt(F, [T]), T > NextInit), Hap), length(Hap, Happens), %writeln(Happens),

    findall(X, (inputEntity(F=V), F \= proximity(_,_), holdsFor(F=V,L), L \== [], setTheSceneSDFluent(L, NextInit, _, L2), length(L2,X)), HF),
    sum_list(HF, HoldsFor),

    findall(X, (inputEntity(F=V), F = proximity(_,_), holdsFor(F=V,L), L \== [], setTheSceneSDFluent(L, NextInit, _, L2), length(L2,X)), SHF),
    sum_list(SHF, SpHoldsFor),% writeln(SpHoldsFor),

    findall(T, (event2(F), (F = entersArea(_) ; F = leavesArea(_)), happensAt(F, [T|_]), T > NextInit), SpHap), length(SpHap, SpHappens),

    HapLength is Happens + HoldsFor,
    SpLength is SpHappens + SpHoldsFor,
    TotalOver is HapLength + SpLength,
    nb_getval(overlap, OverData),
    append(OverData, [TotalOver], NewOverData),
    nb_setval(overlap, NewOverData).

update_global_fluents :-
	nb_getval(window_sdf, WSDF),
	nb_getval(window_simple, WSimple),
	write('Isimple: '), writeln(WSimple),
	write('Isdf: '), writeln(WSDF),
	nb_getval(sdf, SDF),
	nb_getval(simple, Simple),
	append(SDF, [WSDF], FSDF),
	append(Simple, [WSimple], FSimple),
	nb_setval(window_sdf, 0),
	nb_setval(window_simple, 0),
	nb_setval(sdf, FSDF),
	nb_setval(simple, FSimple),
	nb_getval(window_union, WU),
	nb_getval(window_inter, WInter),
	nb_getval(window_comp, WComp),
	nb_getval(union, Union),
	nb_getval(inter, Inter),
	nb_getval(comp, Comp),
	append(Union, [WU], FU),
	append(Inter, [WInter], FI),
	append(Comp, [WComp], FC),
	nb_setval(window_union, 0),
	nb_setval(window_inter, 0),
	nb_setval(window_comp, 0),
	nb_setval(union, FU),
	nb_setval(inter, FI),
	nb_setval(comp, FC).

  
average(List, Average):- 
    sum_list(List, Sum),
    length(List, Length),
    Length > 0, 
    Temp is Sum * 0, Temp =:= 0,
    Average is round(Sum / Length), !.

average(_List, 0).


compute_overlap([LLEs], [SPEs], _OverLLEs, [], 0, 0) :- !.

compute_overlap([_Hlles|LLEs], [_Hspes|SPEs], OverLLEs, AllOver, TotalOver, AvgOver) :-
    last(OverLLEs, Lo),
    append(RestOver, [Lo], OverLLEs),
    sum_list(LLEs, Tlle), sum_list(SPEs, Tspe), sum_list(RestOver, Tover),
    TotalOver is float(Tover / (Tlle + Tspe)) * 100,
    compute_overlap2(LLEs, SPEs, RestOver, [], AllOver),
    average(AllOver, AvgOver).

compute_overlap2([], [], [], AllOver, AllOver) :- !.

compute_overlap2([Hlles|RLLEs], [Hspes|RSPEs], [Hover|Rover], TempOver, AllOver) :-
    Cwover is float(Hover / (Hlles + Hspes)) * 100,
    append(TempOver, [Cwover], NewTempOver),
    compute_overlap2(RLLEs, RSPEs, Rover, NewTempOver, AllOver).

    
writeStats(ResultsStream, Stream):-
    nb_getval(inputdata, LLEs),
    nb_getval(spdata, SPEs),
    nb_getval(overlap, OverLLEs),
    compute_overlap(LLEs, SPEs, OverLLEs, AllOver, TotalOver, AvgOver),
    nb_getval(hleinstances, HLEs),
    nb_getval(time, Time),
    nb_getval(sdf, LTsdf),
    nb_getval(simple, LTsimple),
    nb_getval(union, LTunion),
    nb_getval(inter, LTinter),
    nb_getval(comp, LTcomp),
  
    nb_getval(gstack, GS),
    nb_getval(lstack, LS),
    nb_getval(tstack, TS),
    nb_getval(heap, HS),
    nb_getval(memunshared, MUS),
    nb_getval(atomsno, ANS),
    nb_getval(atomsmem, AMS),
  
    sum_list(LLEs, TotalLLEs),
    sum_list(SPEs, TotalSPEs),
    sum_list(HLEs, TotalHLEs),
    sum_list(Time, TotalTime),

    average(LLEs, AvgLLE),
    average(SPEs, AvgSPE),
    average(HLEs, AvgHLE),
    average(Time, AvgT),
    average(LTsdf, AvgSDF),
    average(LTsimple, AvgSimple),
    average(LTunion, AvgUnion),
    average(LTinter, AvgInter),
    average(LTcomp, AvgComp),

    average(GS, AvgGlobal),
    average(LS, AvgLocal),
    average(TS, AvgTrail),
    average(HS, AvgHeap),
    average(MUS, AvgMemUnshared),
    average(ANS, AvgAtomsNo),
    average(AMS, AvgAtomsMem),
    write('Total/Average number of input LLEs: '), write(TotalLLEs), write('/'), writeln(AvgLLE),
    write('Total/Average number of input SPEs: '), write(TotalSPEs), write('/'), writeln(AvgSPE),
    write('Total/Average number of overlapping LLES: '), write(TotalOver), write('/'), writeln(AvgOver),
    write('Total/Average number of HLE instances: '), write(TotalHLEs), write('/'), writeln(AvgHLE),
  
    write('Total/Average time: '), write(TotalTime), write('/'), writeln(AvgT),
    write('Average time of SDFluents: '), writeln(AvgSDF),
    write('Average time of Simple Fluents: '), writeln(AvgSimple),
    write('Average time of Union: '), writeln(AvgUnion),
    write('Average time of Intersection: '), writeln(AvgInter),
    write('Average time of Complement: '), writeln(AvgComp),
  
    write('Average memory usage (GLOBAL): '), writeln(AvgGlobal),
    write('Average memory usage (LOCAL): '), writeln(AvgLocal),
    write('Average memory usage (TRAIL): '), writeln(AvgTrail),
    write('Average memory usage (HEAP): '), writeln(AvgHeap),
    write('Average unshared memory usage : '), writeln(AvgMemUnshared),
    write('Average atoms number : '), writeln(AvgAtomsNo),
    write('Average atoms memory : '), writeln(AvgAtomsMem),
  
    write(Stream, 'LLEs: '), write(Stream,LLEs), nl(Stream),
    write(Stream, 'Total/Average number of input LLEs: '),
    write(Stream, TotalLLEs), write(Stream, '/'),
    write(Stream, AvgLLE),
    nl(Stream),
  
    write(Stream, 'SPEs: '), write(Stream, SPEs), nl(Stream),
    write(Stream, 'Total/Average number of input SPEs: '),
    write(Stream, TotalSPEs), write(Stream,'/'),
    write(Stream, AvgSPE),
    nl(Stream),
  
    write(Stream, 'Overlapping number LLEs: '), write(Stream, OverLLEs), nl(Stream),
    write(Stream, 'Overlapping LLEs: '), write(Stream, AllOver), nl(Stream),
    write(Stream, 'Total/Average number of overlapping LLEs: '),
    write(Stream, TotalOver), write(Stream,'/'),
    write(Stream, AvgOver),
    nl(Stream),
  
    write(Stream, 'HLEs: '), write(Stream, HLEs), nl(Stream),
    write(Stream, 'Total/Average number of HLE instances: '),
    write(Stream, TotalHLEs), write(Stream,'/'),
    write(Stream, AvgHLE),
    nl(Stream),
  
    write(Stream, 'Times: '), write(Stream, Time), nl(Stream),
    write(Stream, 'Total/Average time: '), write(Stream, TotalTime), write(Stream, '/'), write(Stream, AvgT), nl(Stream),
    max_list(Time, Max),
    write(Stream, 'Max: '), write(Stream, Max),
    nl(Stream),
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
    write(Stream, 'SDFluents: '), write(Stream, LTsdf), nl(Stream),
    write(Stream, 'Average time of SDFluents: '), write(Stream, AvgSDF), nl(Stream),
  
    write(Stream, 'Simple Fluents: '), write(Stream, LTsimple), nl(Stream),
    write(Stream, 'Average time of Simple Fluents: '), write(Stream, AvgSimple), nl(Stream),
  
    write(Stream, 'Union: '), write(Stream, LTunion), nl(Stream),
    write(Stream, 'Average time of Union: '), write(Stream, AvgUnion), nl(Stream),
  
    write(Stream, 'Intersectiom: '), write(Stream, LTinter), nl(Stream),
    write(Stream, 'Average time of Intersection: '), write(Stream, AvgInter), nl(Stream),
  
    write(Stream, 'Complement: '), write(Stream, LTcomp), nl(Stream),
    write(Stream, 'Average time of Complement: '), write(Stream, AvgComp), nl(Stream),
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    write(Stream, 'GLOBAL: '), write(Stream, GS), nl(Stream),
    write(Stream, 'Average memory usage (GLOBAL): '),
    write(Stream, AvgGlobal),
    nl(Stream),
  
    write(Stream, 'LOCAL: '), write(Stream, LS), nl(Stream),
    write(Stream, 'Average memory usage (LOCAL): '),
    write(Stream, AvgLocal),
    nl(Stream),
  
    write(Stream, 'TRAIL: '), write(Stream, TS), nl(Stream),
    write(Stream, 'Average memory usage (TRAIL): '),
    write(Stream, AvgTrail),
    nl(Stream),
  
    write(Stream, 'HEAP: '), write(Stream, HS), nl(Stream),
    write(Stream, 'Average memory usage (HEAP): '),
    write(Stream, AvgHeap),
    nl(Stream),
  
    write(Stream, 'Memory Unshared: '), write(Stream, MUS), nl(Stream),
    write(Stream, 'Average unshared memory usage: '),
    write(Stream, AvgMemUnshared),
    nl(Stream),
  
    write(Stream, 'Atoms No: '), write(Stream, ANS), nl(Stream),
    write(Stream, 'Average atoms number: '),
    write(Stream, AvgAtomsNo),
    nl(Stream),
  
    write(Stream, 'Atoms Memory: '), write(Stream, AMS), nl(Stream),
    write(Stream, 'Average atoms memory: '),
    write(Stream, AvgAtomsMem),
    nl(Stream).
