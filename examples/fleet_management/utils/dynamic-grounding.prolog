:- use_module(library(system)).

dynamicGrounding :-
	% Find new vehicles given the new SDE
	findall(Vehicle, ((event2(E);event(E)), keyOfEvent(E, Key), findNewVehicles(E, Key, Vehicle)), NV),
	findall(Vehicle, ((event2(E);event(E)), keyOfEvent(E, Key), findNewVehicles2(E, Key, Vehicle)), NV2),
	rm-dups(NV, NewVehicles),
	rm-dups(NV2, NewVehicles2),

	% Check which of the old vehicles need to be kept
	findall(KeepOldVehicle,
			(
			recorded(vehicle, KeepOldVehicle, _),
			checkV(KeepOldVehicle)
			),
			KeptVehicles),

	append([NewVehicles,NewVehicles2,KeptVehicles],UniqueVehicles0), 
	rm-dups(UniqueVehicles0, UniqueVehicles),

	retractVehicles(UniqueVehicles),
	assertVehicles(UniqueVehicles),
	assertVehicles2(NewVehicles2),
	assertVehicles3(NewVehicles).
	
	
checkV(Vehicle) :-
	simpleFluent(F=V),
	keyOfEvent(F=V, Key),
	recorded(Key, simpleFPList(Vehicle, _, _, _, _, _, _, _, _), _), !.




findNewVehicles(moving(_), Key, Vehicle) :-
	atomic_list_concat(['010', Key], K),
	recorded(K, [Vehicle|_], _).

findNewVehicles(notMoving(_), Key, Vehicle) :-
	atomic_list_concat(['010', Key], K),
	recorded(K, [Vehicle, _], _).

findNewVehicles(stopped(_), Key, Vehicle) :-
	atomic_list_concat(['010', Key], K),
	recorded(K, [Vehicle, _], _).


findNewVehicles2(moving(_), Key, Vehicle) :-
	atomic_list_concat(['011', Key], K),
	recorded(K, [Vehicle|_], _).

findNewVehicles2(notMoving(_), Key, Vehicle) :-
	atomic_list_concat(['011', Key], K),
	recorded(K, [Vehicle, _], _).

findNewVehicles2(stopped(_), Key, Vehicle) :-
	atomic_list_concat(['011', Key], K),
	recorded(K, [Vehicle, _], _).


% If Vehicle exists, but is no longer required, either based on new SDEs,
% or previous lists (see checkV), then retract it
retractVehicles(NewUniqueVehicles) :-
	findall(Vehicle,
		(
			recorded(vehicle, Vehicle, R),
			\+member(Vehicle,NewUniqueVehicles),
			erase(R)
		), _).
		
% If Vehicle is required and does not exist,
% assert it. If it exists, do nothing.
assertVehicles(NewUniqueVehicles) :-
	findall(Vehicle,
		(
			member(Vehicle,NewUniqueVehicles),
			\+ recorded(vehicle, Vehicle, _),
			recordz(vehicle, Vehicle, _)
		), _).


assertVehicles2(NewUniqueVehicles) :-
	eraseall(vehicleIns),
	findall(Vehicle,
		(
			member(Vehicle,NewUniqueVehicles),
			recordz(vehicleIns, Vehicle, _)
		), _).


assertVehicles3(NewUniqueVehicles) :-
	eraseall(vehicleDel),
	findall(Vehicle,
		(
			member(Vehicle,NewUniqueVehicles),
			recordz(vehicleDel, Vehicle, _)
		), _).
  

% check if a vehicle is associated with a delayed event
delayedEntity(Index) :-
        recorded(vehicleDel, Index, _).

% check if a vehicle is associated with an on time event
onTimeEntity(Index, true) :-
        recorded(vehicleIns, Index, _), !.

onTimeEntity(_Index, false).
