:- use_module(library(system)).

dynamicGrounding :-
	% Find new vessels given the new SDE
	findall(Vessel,	(event2(E), keyOfEvent(E, Key), findNewVessels(E, Key, Vessel)), NV),
	findall(Vessel, (event2(E), keyOfEvent(E, Key), findNewVessels2(E, Key, Vessel)), NV2),
	rm-dups(NV, NewVessels),
	rm-dups(NV2, NewVessels2),
		
	% Check which of the old vessels need to be kept
	findall(KeepOldVessel,
			(
				recorded(vessel, KeepOldVessel, _),
				checkV(KeepOldVessel)
			),
			KeptVessels),
	append([NewVessels,NewVessels2,KeptVessels],UniqueVessels0), 
	rm-dups(UniqueVessels0, UniqueVessels),

	keyOfEvent(proximity(_,_)=true, PKey), atomic_list_concat(['010', PKey], DPKey), atomic_list_concat(['011', PKey], DoPKey),
	atomic_list_concat(['100', PKey], DPKey2),

	findall((Vessel1,Vessel2),
			(
				recorded(DPKey, [proximity(Vessel1,Vessel2)=true, (_S,_E)], _)
				;
				recorded(DoPKey, [proximity(Vessel1,Vessel2)=true, (_S,_E)], _)
			), 
			NewVesselPairs),

	% Check which of the old vessel pairs need to be kept
	findall((Vessel1,Vessel2),
			(
				recorded(vesselPair, (Vessel1,Vessel2), _),
				checkProx(DPKey2,Vessel1,Vessel2)
			),
			KeptVesselPairs),
	append(NewVesselPairs,KeptVesselPairs,VP12),
	rm-dups(VP12,UniqueVesselPairs),

	retractVessels(UniqueVessels),
	assertVessels(UniqueVessels),
	assertVessels2(NewVessels2),
	assertVessels3(NewVessels),
	retractVesselPairs(UniqueVesselPairs),
	assertVesselPairs(UniqueVesselPairs).



%checkV(Vessel) :-
%        inputEntity(F=V),
%        keyOfEvent(F=V, Key),
%        atomic_list_concat(['100', Key], Dkey),
%        checkCounter(Dkey, Dkey2),
%        recorded(Dkey2, iePList(_, Vessel, _, _, _, _, _), _), !.


checkV(Vessel) :-
	simpleFluent(F=V),
	keyOfEvent(F=V, Key),
	recorded(Key, simpleFPList(Vessel, _, _, _, _, _, _, _, _), _), !.

checkV(Vessel) :-
	sDFluent(F=V),
	outputEntity(F=V),
	keyOfEvent(F=V, Key),
	recorded(Key, sdFPList(Vessel, _, _, _, _, _, _), _), !.

checkV(Vessel) :-
	sDFluent2(F=V),
	keyOfEvent(F=V, Key),
	recorded(Key, sdFPList2(Vessel, _, _, _, _, _, _, _), _), !.

checkV(Vessel) :-
	inputEntity(F=V),
	keyOfEvent(F=V, Key),
	atomic_list_concat(['100', Key], Dkey),
	checkCounter(Dkey, Dkey2),
	recorded(Dkey2, iePList(_, Vessel, _, _, _, _, _), _), !.


checkProx(DPKey, Vessel1, Vessel2) :-
	createIndex(proximity(Vessel1,Vessel2) = true, Pindex),
	checkCounter(DPKey, DPKey2),
	recorded(DPKey2, iePList(Pindex, Vessel1, _, _, _, _, _), _), !.


findNewVessels(velocity(_), Key, Vessel) :-
	atomic_list_concat(['010', Key], K),
	recorded(K, [Vessel|Tail], _).

findNewVessels(entersArea(_), Key, Vessel) :-
	atomic_list_concat(['010', Key], K),
	recorded(K, [Vessel|Tail], _).

findNewVessels(leavesArea(_), Key, Vessel) :-
	atomic_list_concat(['010', Key], K),
	recorded(K, [Vessel|Tail], _).

findNewVessels2(velocity(_), Key, Vessel) :-
        atomic_list_concat(['011', Key], K),
        recorded(K, [Vessel|Tail], _).

findNewVessels2(entersArea(_), Key, Vessel) :-
	atomic_list_concat(['011', Key], K),
	recorded(K, [Vessel|Tail], _).

findNewVessels2(leavesArea(_), Key, Vessel) :-
    atomic_list_concat(['011', Key], K),
    recorded(K, [Vessel|Tail], _).
	



% If Vessel exists, but is no longer required, either based on new SDEs, 
% or previous lists (see checkV), then retract it
retractVessels(NewUniqueVessels) :-
	findall(Vessel,
		(
			recorded(vessel, Vessel, R),
			\+member(Vessel,NewUniqueVessels),
			erase(R)
		), _).
		
% If Vessel is required and does not exist,
% assert it. If it exists, do nothing.
assertVessels(NewUniqueVessels) :-
	findall(Vessel,
		(
			member(Vessel,NewUniqueVessels),
			\+ recorded(vessel, Vessel, _),
			recordz(vessel, Vessel, _)
		), _).

% assert a vessel if it is associated with events that arrived on time
assertVessels2(NewUniqueVessels) :-
	eraseall(vesselIns),
	findall(Vessel,
		(
			member(Vessel,NewUniqueVessels),
			recordz(vesselIns, Vessel, _)
		), _).

% assert a vessel if it is associated with events that arrived with a delay
assertVessels3(NewUniqueVessels) :-
        eraseall(vesselDel),
        findall(Vessel,
                (
                        member(Vessel,NewUniqueVessels),
                        recordz(vesselDel, Vessel, _)            
                ), _).


retractVesselPairs(NewUniqueVesselPairs) :-
	findall((Vessel1,Vessel2),
		(
			recorded(vesselPair, (Vessel1,Vessel2), R),
			\+member((Vessel1,Vessel2),NewUniqueVesselPairs),
			erase(R)
		), _).
		
% If vesselPair is required and does not exist,
% assert it. If it exists, do nothing.
assertVesselPairs(NewUniqueVesselPairs) :-
	eraseall(vesselPairNew),
	findall((Vessel1,Vessel2),
		(
			member((Vessel1,Vessel2),NewUniqueVesselPairs),
			\+ recorded(vesselPair, (Vessel1,Vessel2), _),
			recordz(vesselPair, (Vessel1,Vessel2), _),
			recordz(vesselPairNew, (Vessel1,Vessel2), _)
		), _).

% check if a vessel is associated with a delayed event
delayedEntity(Index) :-
    recorded(vesselDel, Index, _).

% check if a vessel is associated with an on time event
onTimeEntity(Index, true) :-
    recorded(vesselIns, Index, _), !.

onTimeEntity(_Index, false).
