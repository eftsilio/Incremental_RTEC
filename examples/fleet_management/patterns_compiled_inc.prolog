
:- discontiguous initiatedAt_Retract/3, initiatedAt_Delay/3, terminatedAt_Retract/3, terminatedAt_Delay/3, initiatedAt_OnTime/3, terminatedAt_OnTime/3, terminatedAt/3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% overSpeeding %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initiatedAt_Retract(overSpeeding(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, moving(Index), [T, _]).

initiatedAt_Delay(overSpeeding(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, moving(Index), [T, Speed]),
	vehicleType(Index, VT),
	vehicleSpeed(VT, VTS),
	Speed > VTS.

terminatedAt_Retract(overSpeeding(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, moving(Index), [T, _]).

terminatedAt_Retract(overSpeeding(Index)=true, T, '2e') :-
	happensAtProcessedStaticIE_Retractions(Index, notMoving(Index), [T]).

terminatedAt_Retract(overSpeeding(Index)=true, T, '3e') :-
	happensAtProcessedStaticIE_Retractions(Index, stopped(Index), [T]).


terminatedAt_Delay(overSpeeding(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, moving(Index), [T, Speed]),
	vehicleType(Index, VT),
	vehicleSpeed(VT, VTS),
	Speed =< VTS.

terminatedAt_Delay(overSpeeding(Index)=true, T, '2e') :-
	happensAtProcessedStaticIE_Delays(Index, notMoving(Index), [T]).

terminatedAt_Delay(overSpeeding(Index)=true, T, '3e') :-
	happensAtProcessedStaticIE_Delays(Index, stopped(Index), [T]).

%%%%%On time

initiatedAt_OnTime(overSpeeding(Index)=true, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, moving(Index), [T, Speed]),
	vehicleType(Index, VT),
	vehicleSpeed(VT, VTS),
	Speed > VTS.
     
terminatedAt_OnTime(overSpeeding(Index)=true, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, moving(Index), [T, Speed]),
	vehicleType(Index, VT),
	vehicleSpeed(VT, VTS),
	Speed =< VTS.

terminatedAt_OnTime(overSpeeding(Index)=true, T, 'b') :-
	happensAtProcessedStaticIE_OnTime(Index, notMoving(Index), [T]).

terminatedAt_OnTime(overSpeeding(Index)=true, T, 'c') :-
	happensAtProcessedStaticIE_OnTime(Index, stopped(Index), [T]).

%%%%%%% Termination all over points

terminatedAt(overSpeeding(Index)=true, T, 'a') :-
        happensAtProcessedStaticIE_Over(Index, moving(Index), [T, Speed]),
        vehicleType(Index, VT),
        vehicleSpeed(VT, VTS),
        Speed =< VTS.

terminatedAt(overSpeeding(Index)=true, T, 'b') :-
        happensAtProcessedStaticIE_Over(Index, notMoving(Index), [T]).

terminatedAt(overSpeeding(Index)=true, T, 'c') :-
        happensAtProcessedStaticIE_Over(Index, stopped(Index), [T]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% dangerousDriving %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


initiatedAt_Retract(dangerousDriving(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, acceleration(Index), [T]).

initiatedAt_Retract(dangerousDriving(Index)=true, T, '2e') :-
	happensAtProcessedStaticIE_Retractions(Index, braking(Index), [T]).

initiatedAt_Retract(dangerousDriving(Index)=true, T, '3e') :-
	happensAtProcessedStaticIE_Retractions(Index, cornering(Index), [T]).

initiatedAt_Retract(dangerousDriving(Index)=true, T, '4e') :-
	happensAtProcessedStaticIE_Retractions(Index, ice(Index), [T]).

initiatedAt_Retract(dangerousDriving(Index)=true, T, '1f') :-
	holdsAtProcessedSimpleFluent_Retractions(Index, overSpeeding(Index)=true, T).

     
initiatedAt_Delay(dangerousDriving(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, acceleration(Index), [T]),
	holdsAtProcessedSimpleFluent(Index, overSpeeding(Index)=true, T).

initiatedAt_Delay(dangerousDriving(Index)=true, T, '1f') :-
	holdsForDelays(Index, overSpeeding(Index)=true, L),
	happensAtProcessedStaticIE_Qi_notDelays(Index, acceleration(Index), [T]),
	tinIntervals(T, L).

initiatedAt_Delay(dangerousDriving(Index)=true, T, '2e') :-
        happensAtProcessedStaticIE_Delays(Index, braking(Index), [T]),
        holdsAtProcessedSimpleFluent(Index, overSpeeding(Index)=true, T).

initiatedAt_Delay(dangerousDriving(Index)=true, T, '2f') :-
        holdsForDelays(Index, overSpeeding(Index)=true, L),
        happensAtProcessedStaticIE_Qi_notDelays(Index, braking(Index), [T]),
        tinIntervals(T, L).

initiatedAt_Delay(dangerousDriving(Index)=true, T, '3e') :-
        happensAtProcessedStaticIE_Delays(Index, cornering(Index), [T]),
        holdsAtProcessedSimpleFluent(Index, overSpeeding(Index)=true, T).

initiatedAt_Delay(dangerousDriving(Index)=true, T, '3f') :-
        holdsForDelays(Index, overSpeeding(Index)=true, L),
        happensAtProcessedStaticIE_Qi_notDelays(Index, cornering(Index), [T]),
        tinIntervals(T, L).

initiatedAt_Delay(dangerousDriving(Index)=true, T, '4e') :-
        happensAtProcessedStaticIE_Delays(Index, ice(Index), [T]),
        holdsAtProcessedSimpleFluent(Index, overSpeeding(Index)=true, T).

initiatedAt_Delay(dangerousDriving(Index)=true, T, '4f') :-
        holdsForDelays(Index, overSpeeding(Index)=true, L),
        happensAtProcessedStaticIE_Qi_notDelays(Index, ice(Index), [T]),
        tinIntervals(T, L).



terminatedAt_Retract(dangerousDriving(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, notMoving(Index), [T]).

terminatedAt_Retract(dangerousDriving(Index)=true, T, '2e') :-
	happensAtProcessedStaticIE_Retractions(Index, stopped(Index), [T]).

%terminatedAt_Retract(dangerousDriving(Index)=true, T, '1f') :-
%	happensAtProcessedSimpleFluent_Retractions(Index, end(overSpeeding(Index)=true), T).
	

terminatedAt_Delay(dangerousDriving(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, notMoving(Index), [T]).

terminatedAt_Delay(dangerousDriving(Index)=true, T, '2e') :-
	happensAtProcessedStaticIE_Delays(Index, stopped(Index), [T]).

%%%%%On time

initiatedAt_OnTime(dangerousDriving(Index)=true, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, acceleration(Index), [T]),
	holdsAtProcessedSimpleFluent_OnTime(Index, overSpeeding(Index)=true, T).

initiatedAt_OnTime(dangerousDriving(Index)=true, T, 'b') :-
	happensAtProcessedStaticIE_OnTime(Index, braking(Index), [T]),
	holdsAtProcessedSimpleFluent_OnTime(Index, overSpeeding(Index)=true, T).

initiatedAt_OnTime(dangerousDriving(Index)=true, T, 'c') :-
	happensAtProcessedStaticIE_OnTime(Index, cornering(Index), [T]),
	holdsAtProcessedSimpleFluent_OnTime(Index, overSpeeding(Index)=true, T).

initiatedAt_OnTime(dangerousDriving(Index)=true, T, 'd') :-
	happensAtProcessedStaticIE_OnTime(Index, ice(Index), [T]),
	holdsAtProcessedSimpleFluent_OnTime(Index, overSpeeding(Index)=true, T).


terminatedAt_OnTime(dangerousDriving(Index)=true, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, notMoving(Index), [T]).

terminatedAt_OnTime(dangerousDriving(Index)=true, T, 'b') :-
	happensAtProcessedStaticIE_OnTime(Index, stopped(Index), [T]).

%terminatedAt_OnTime(dangerousDriving(Index)=true, T) :-
%	happensAtProcessedSimpleFluent(Index, end(overSpeeding(Index)=true), T).


%%%%%%% Termination all over points

terminatedAt(dangerousDriving(Index)=true, T, 'a') :-
        happensAtProcessedStaticIE_Over(Index, notMoving(Index), [T]).

terminatedAt(dangerousDriving(Index)=true, T, 'b') :-
        happensAtProcessedStaticIE_Over(Index, stopped(Index), [T]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% notEconomicDriving %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initiatedAt_Retract(notEconomicDriving(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, fuelLevel(Index), [T, _]).

initiatedAt_Retract(notEconomicDriving(Index)=true, T, '1f') :-
	holdsAtProcessedSimpleFluent_Retractions(Index, overSpeeding(Index)=true, T).



initiatedAt_Delay(notEconomicDriving(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, fuelLevel(Index), [T, Tank]),
	holdsAtProcessedSimpleFluent(Index, overSpeeding(Index)=true, T),
	vehicleType(Index, VT),
	vehicleTank(VT, VTT), HT is VTT / 2,
	Tank < HT.
	
initiatedAt_Delay(notEconomicDriving(Index)=true, T, '1f') :-
	holdsForDelays(Index, overSpeeding(Index)=true, L),
	happensAtProcessedStaticIE_Qi_notDelays(Index, fuelLevel(Index), [T, Tank]),
	tinIntervals(T, L),
	vehicleType(Index, VT),
	vehicleTank(VT, VTT), HT is VTT / 2,
	Tank < HT.


terminatedAt_Retract(notEconomicDriving(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, fuelLevel(Index), [T, _]).

terminatedAt_Retract(notEconomicDriving(Index)=true, T, '1f') :-
	holdsAtProcessedSimpleFluent_Delays(Index, overSpeeding(Index)=true, T).

terminatedAt_Retract(notEconomicDriving(Index)=true, T, '2e') :-
	happensAtProcessedStaticIE_Retractions(Index, notMoving(Index), [T]).

terminatedAt_Retract(notEconomicDriving(Index)=true, T, '3e') :-
	happensAtProcessedStaticIE_Retractions(Index, stopped(Index), [T]).

%terminatedAt_Retract(notEconomicDriving(Index)=true,  T) :-
%	happensAtProcessedSimpleFluent_Retractions(Index, end(overSpeeding(Index)=true), T).


terminatedAt_Delay(notEconomicDriving(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, fuelLevel(Index), [T, Tank]),
	\+ holdsAtProcessedSimpleFluent(Index, overSpeeding(Index)=true, T),
	vehicleType(Index, VT),
	vehicleTank(VT, VTT), HT is VTT / 2,
	Tank >= HT.

terminatedAt_Delay(notEconomicDriving(Index)=true, T, '1f') :-
	holdsForRetractions(Index, overSpeeding(Index)=true, L),
	happensAtProcessedStaticIE_Qi_notDelays(Index, fuelLevel(Index), [T, Tank]),
	tinIntervals(T, L),
	vehicleType(Index, VT),
	vehicleTank(VT, VTT), HT is VTT / 2,
	Tank >= HT.

terminatedAt_Delay(notEconomicDriving(Index)=true, T, '2e') :-
	happensAtProcessedStaticIE_Delays(Index, notMoving(Index), [T]).

terminatedAt_Delay(notEconomicDriving(Index)=true, T, '3e') :-
	happensAtProcessedStaticIE_Delays(Index, stopped(Index), [T]).


%%%%%On time

initiatedAt_OnTime(notEconomicDriving(Index)=true, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, fuelLevel(Index), [T, Tank]),
	holdsAtProcessedSimpleFluent_OnTime(Index, overSpeeding(Index)=true, T),
	vehicleType(Index, VT),
	vehicleTank(VT, VTT), HT is VTT / 2,
	Tank < HT.

terminatedAt_OnTime(notEconomicDriving(Index)=true, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, fuelLevel(Index), [T, Tank]),
	\+ holdsAtProcessedSimpleFluent_OnTime(Index, overSpeeding(Index)=true, T),
	vehicleType(Index, VT),
	vehicleTank(VT, VTT), HT is VTT / 2,
	Tank >= HT.
     
terminatedAt_OnTime(notEconomicDriving(Index)=true, T, 'b') :-
	happensAtProcessedStaticIE_OnTime(Index, notMoving(Index), [T]).

terminatedAt_OnTime(notEconomicDriving(Index)=true, T, 'c') :-
	happensAtProcessedStaticIE_OnTime(Index, stopped(Index), [T]).

%terminatedAt_OnTime(notEconomicDriving(Index)=true, T) :-
%	happensAtProcessedSimpleFluent(Index, end(overSpeeding(Index)=true), T).

%%%%%%% Termination all over points


terminatedAt(notEconomicDriving(Index)=true, T, 'a') :-
        happensAtProcessedStaticIE_Over(Index, fuelLevel(Index), [T, Tank]),
        \+ holdsAtProcessedSimpleFluent(Index, overSpeeding(Index)=true, T),
        vehicleType(Index, VT),
        vehicleTank(VT, VTT), HT is VTT / 2,
        Tank >= HT.

terminatedAt(notEconomicDriving(Index)=true, T, 'b') :-
        happensAtProcessedStaticIE_Over(Index, notMoving(Index), [T]).

terminatedAt(notEconomicDriving(Index)=true, T, 'c') :-
        happensAtProcessedStaticIE_Over(Index, stopped(Index), [T]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Refuel %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


initiatedAt_Retract(reFuel(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, closeToGas(Index), [T]).

initiatedAt_Retract(reFuel(Index)=true, T, '1f') :-
	holdsAtProcessedSimpleFluent_Retractions(Index, notEconomicDriving(Index)=true, T).


initiatedAt_Delay(reFuel(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, closeToGas(Index)=true, [T]),
	holdsAtProcessedSimpleFluent(Index, notEconomicDriving(Index)=true, T).

initiatedAt_Delay(reFuel(Index)=true, T, '1f') :-
	holdsForDelays(Index, notEconomicDriving(Index)=true, L),
	happensAtProcessedStaticIE_Qi_notDelays(Index, closeToGas(Index)=true, [T]),
	tinIntervals(T, L).
	

terminatedAt_Retract(reFuel(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, fuelLevel(Index), [T, _]).

terminatedAt_Delay(reFuel(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, fuelLevel(Index), [T, Tank]),
	vehicleType(Index, VT),
	vehicleTank(VT, VTT), HT is VTT / 2,
	Tank >= HT.


%%%%%On time

initiatedAt_OnTime(reFuel(Index)=true, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, closeToGas(Index), [T]),
	holdsAtProcessedSimpleFluent_OnTime(Index, notEconomicDriving(Index)=true, T).

terminatedAt_OnTime(reFuel(Index)=true, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, fuelLevel(Index), [T, Tank]),
	vehicleType(Index, VT),
	vehicleTank(VT, VTT), HT is VTT / 2,
	Tank >= HT.

%%%%%%% Termination all over points

terminatedAt(reFuel(Index)=true, T, 'a') :-
        happensAtProcessedStaticIE_Over(Index, fuelLevel(Index), [T, Tank]),
        vehicleType(Index, VT),
        vehicleTank(VT, VTT), HT is VTT / 2,
        Tank >= HT.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cachingOrder2(Index, overSpeeding(Index)=true) :-
     recorded(vehicle, Index, _).

cachingOrder2(Index, dangerousDriving(Index)=true) :-
     recorded(vehicle, Index, _).

cachingOrder2(Index, notEconomicDriving(Index)=true) :-
     recorded(vehicle, Index, _).

cachingOrder2(Index, reFuel(Index)=true) :-
     recorded(vehicle, Index, _).
