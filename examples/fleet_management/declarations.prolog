:- dynamic grounding/1.
:- set_value(pr,0).

%%%%%%%%%%%%%%%%%% INPUT %%%%%%%%%%%%%%%%%%

event(notMoving(_)).
inputEntity(notMoving(_)).
keyOfEvent(notMoving(_), '02').
index(notMoving(Vehicle), Vehicle).

event(stopped(_)).
inputEntity(stopped(_)).
keyOfEvent(stopped(_), '03').
index(stopped(Vehicle), Vehicle).

event(acceleration(_)).
inputEntity(acceleration(_)).
keyOfEvent(acceleration(_), '04').
index(acceleration(Vehicle), Vehicle).

event(braking(_)).
inputEntity(braking(_)).
keyOfEvent(braking(_), '05').
index(braking(Vehicle), Vehicle).

event(cornering(_)).
inputEntity(cornering(_)).
keyOfEvent(cornering(_), '06').
index(cornering(Vehicle), Vehicle).

event(closeToGas(_)).
inputEntity(closeToGas(_)).
keyOfEvent(closeToGas(_), '07').
index(closeToGas(Vehicle), Vehicle).

event(ice(_)).
inputEntity(ice(_)).
keyOfEvent(ice(_), '08').
index(ice(Vehicle), Vehicle).

event2(moving(_)).
inputEntity(moving(_)).
keyOfEvent(moving(_), '01').
index(moving(Vehicle), Vehicle).

event2(fuelLevel(_)).
inputEntity(fuelLevel(_)).
keyOfEvent(fuelLevel(_), '09').
index(fuelLevel(Vehicle), Vehicle).

%%%%%%%%%%%% OUTPUT %%%%%%%%%%%%

simpleFluent(overSpeeding(_)=true).
outputEntity(overSpeeding(_)=true).
keyOfEvent(overSpeeding(_)=true, '10').
itIsChild(overSpeeding(_)=true).
index(overSpeeding(Vehicle)=true, Vehicle).

rgraph(overSpeeding(Vehicle)=true, [[[(R1, '1e')]], [[(R1, '1e'), (R2, '2e'), (R3, '3e')]], [(R4, '1e')], [(R4, '1e'), (R5, '2e'), (R6, '3e')]], [R1,R2,R3,R4,R5,R6]).
dependency(overSpeeding(Vehicle) = true, [happensAtIER(moving(Vehicle), _), happensAtIER(notMoving(Vehicle), _), happensAtIER(stopped(Vehicle), _), 
						delay(moving(Vehicle), _), delay(notMoving(Vehicle), _), delay(stopped(Vehicle), _)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simpleFluent(dangerousDriving(_)=true).
outputEntity(dangerousDriving(_)=true).
keyOfEvent(dangerousDriving(_)=true, '11').
index(dangerousDriving(Vehicle)=true, Vehicle).

rgraph(dangerousDriving(Vehicle)=true, [[[(R1, '1e'), (R2, '2e'), (R3, '3e'), (R4, '1f')], [(R5, '4e')]], [[(R6, '1e'), (R7, '2e')]],
					[(R8, '1e'), (R9, '1f'), (R10, '2e'), (R9, '2f'), (R11, '3e'), (R9, '3f'), (R12, '4e'), (R9, '4f')], [(R13, '1e'), (R14, '2e')]],
					[R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12,R13,R14]).
dependency(dangerousDriving(Vehicle) = true, [happensAtIER(acceleration(Vehicle), _), happensAtIER(braking(Vehicle), _), happensAtIER(cornering(Vehicle), _),
						holdsForRetractions(Vehicle, overSpeeding(Vehicle) = true, [_|_]), happensAtIER(ice(Vehicle), _),
						happensAtIER(notMoving(Vehicle), _), happensAtIER(stopped(Vehicle), _),
						delay(acceleration(Vehicle), _), holdsForDelays(Vehicle, overSpeeding(Vehicle) = true, [_|_]),
						delay(braking(Vehicle), _), delay(cornering(Vehicle), _), delay(ice(Vehicle), _),
                                                delay(notMoving(Vehicle), _), delay(stopped(Vehicle), _)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simpleFluent(notEconomicDriving(_)=true).
outputEntity(notEconomicDriving(_)=true).
keyOfEvent(notEconomicDriving(_)=true, '12').
itIsChild(notEconomicDriving(_)=true).
index(notEconomicDriving(Vehicle)=true, Vehicle).

rgraph(notEconomicDriving(Vehicle)=true, [[[(R1, '1e'), (R2, '1f')]], [[(R1, '1e'), (R3, '1f')], [(R4, '2e'), (R5, '3e')]], [(R6, '1e'), (R3, '1f')],
						[(R6, '1e'), (R2, '1f'), (R7, '2e'), (R8, '3e')]], [R1,R2,R3,R4,R5,R6,R7,R8]).
dependency(notEconomicDriving(Vehicle) = true, [happensAtIER(fuelLevel(Vehicle), _), holdsForRetractions(Vehicle, overSpeeding(Vehicle) = true, [_|_]),
						holdsForDelays(Vehicle, overSpeeding(Vehicle) = true, [_|_]), happensAtIER(notMoving(Vehicle), _), happensAtIER(stopped(Vehicle), _),
						delay(fuelLevel(Vehicle), _),
						delay(notMoving(Vehicle), _), delay(stopped(Vehicle), _)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simpleFluent(reFuel(_)=true).
outputEntity(reFuel(_)=true).
keyOfEvent(reFuel(_)=true, '13').
index(reFuel(Vehicle)=true, Vehicle).

rgraph(reFuel(Vehicle)=true, [[[(R1, '1e'), (R2, '1f')]], [[(R3, '1e')]], [(R4, '1e'), (R5, '1f')], [(R6, '1e')]], [R1,R2,R3,R4,R5,R6]).
dependency(reFuel(Vehicle) = true, [happensAtIER(closeToGas(Vehicle), _), holdsForRetractions(Vehicle, notEconomicDriving(Vehicle) = true, [_|_]), 
					happensAtIER(fuelLevel(Vehicle), _),
					delay(closeToGas(Vehicle), _), holdsForDelays(Vehicle, notEconomicDriving(Vehicle) = true, [_|_]),
					delay(fuelLevel(Vehicle), _)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


needsGrounding(_, _, _) :- fail.
buildFromPoints(_) :- fail.
