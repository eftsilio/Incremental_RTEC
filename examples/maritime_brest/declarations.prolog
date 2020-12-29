:- dynamic grounding/1.
:- set_value(pr,0).

/*showWhatLoadedDeclsSpPrep:-
    getcwd(Dir),
    write('Loaded '),write(Dir),writeln('/amin_declarations.prolog').

:- showWhatLoadedDeclsSpPrep.*/


/********************************************************************** DECLARATIONS *******************************************************************************
 -Declare the entities of the event description: events, simple and statically determined fluents.
 -For each entity state if it is input or output (simple fluents are by definition output entities).
 -For each input/output entity state its index.
 -For input entities/statically determined fluents state whether the intervals will be collected into a list or built from time-points.
 -Declare the groundings of the fluents and output entities/events.
 -Declare the order of caching of output entities.
 *******************************************************************************************************************************************************************/

%%%%%%%%%%%%%%%%%% INPUT %%%%%%%%%%%%%%%%%%

event(change_in_speed_start(_)).
inputEntity(change_in_speed_start(_)).
keyOfEvent(change_in_speed_start(_), '01').
index(change_in_speed_start(Vessel), Vessel).

event(change_in_speed_end(_)).
inputEntity(change_in_speed_end(_)).
keyOfEvent(change_in_speed_end(_), '02').
index(change_in_speed_end(Vessel), Vessel).

event(change_in_heading(_)).
inputEntity(change_in_heading(_)).
keyOfEvent(change_in_heading(_), '03').
index(change_in_heading(Vessel), Vessel).

event(stop_start(_)).
inputEntity(stop_start(_)).
keyOfEvent(stop_start(_), '04').
index(stop_start(Vessel), Vessel).

event(stop_end(_)).
inputEntity(stop_end(_)).
keyOfEvent(stop_end(_), '05').
index(stop_end(Vessel), Vessel).

event(slow_motion_start(_)).
inputEntity(slow_motion_start(_)).
keyOfEvent(slow_motion_start(_), '06').
index(slow_motion_start(Vessel), Vessel).

event(slow_motion_end(_)).
inputEntity(slow_motion_end(_)).
keyOfEvent(slow_motion_end(_), '07').
index(slow_motion_end(Vessel), Vessel).

event(gap_start(_)).
inputEntity(gap_start(_)).
keyOfEvent(gap_start(_), '08').
index(gap_start(Vessel), Vessel).

event(gap_end(_)).
inputEntity(gap_end(_)).
keyOfEvent(gap_end(_), '09').
index(gap_end(Vessel), Vessel).

event2(entersArea(_)).
inputEntity(entersArea(_)).
keyOfEvent(entersArea(_), '10').
index(entersArea(Vessel), Vessel).

event2(leavesArea(_)).
inputEntity(leavesArea(_)).
keyOfEvent(leavesArea(_), '11').
index(leavesArea(Vessel), Vessel).

event2(coord(_)).
inputEntity(coord(_)).
keyOfEvent(coord(_), '12').
index(coord(Vessel), Vessel).

event2(velocity(_)).
inputEntity(velocity(_)).
keyOfEvent(velocity(_), '13').
index(velocity(Vessel), Vessel).

sDFluent(proximity(_,_)=true).
inputEntity(proximity(_,_)=true).
keyOfEvent(proximity(_,_)=true, '14').
index(proximity(Vessel,_)=true,Vessel).
secondIndex(proximity(Vessel,_)=true).

%%%%%%%%%%%% OUTPUT %%%%%%%%%%%%

simpleFluent(gap(_)=nearPort).
outputEntity(gap(_)=nearPort).
keyOfEvent(gap(_)=nearPort, '15').
%itIsChild(gap(_)=nearPort).
index(gap(Vessel)=nearPort, Vessel).

rgraph(gap(Vessel)=nearPort, [[[(R1, '1e'), (R2,'1f')]], [[(R3,'1e')]], [(R4,'1e'),(R5,'1f')], [(R6,'1e')]], [R1,R2,R3,R4,R5,R6]).
dependency(gap(Vessel)=nearPort, [happensAtIER(gap_start(Vessel), _), holdsForRetractions(Vessel, withinArea(Vessel,nearPorts)=true, [_|_]),
                                happensAtIER(gap_end(Vessel), _), delay(gap_start(Vessel), _), holdsForDelays(Vessel, withinArea(Vessel,nearPorts)=true, [_|_]),
                                delay(gap_end(Vessel), _)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simpleFluent(gap(_)=farFromPorts).
outputEntity(gap(_)=farFromPorts).
keyOfEvent(gap(_)=farFromPorts, '16').
%itIsChild(gap(_)=farFromPorts).
index(gap(Vessel)=farFromPorts, Vessel).

rgraph(gap(Vessel)=farFromPorts, [[[(R1, '1e'), (R2,'1f')]], [[(R3,'1e')]], [(R4,'1e'),(R5,'1f')], [(R6,'1e')]], [R1,R2,R3,R4,R5,R6]).
dependency(gap(Vessel)=farFromPorts, [happensAtIER(gap_start(Vessel), _), holdsForDelays(Vessel, withinArea(Vessel,nearPorts)=true, [_|_]),
                                happensAtIER(gap_end(Vessel), _), delay(gap_start(Vessel), _), holdsForRetractions(Vessel, withinArea(Vessel,nearPorts)=true, [_|_]),
                                delay(gap_end(Vessel), _)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simpleFluent(stopped(_) = nearPort).
outputEntity(stopped(_) = nearPort).
keyOfEvent(stopped(_) = nearPort, '17').
index(stopped(Vessel) = nearPort, Vessel).

rgraph(stopped(Vessel) = nearPort, [[[(R1, '1e'), (R2,'1f')]], [[(R3,'1e')]], [(R4,'1e'),(R5,'1f')], [(R6,'1e')]], [R1,R2,R3,R4,R5,R6]).
dependency(stopped(Vessel) = nearPort, [happensAtIER(stop_start(Vessel), _), holdsForRetractions(Vessel, withinArea(Vessel,nearPorts)=true, [_|_]),
                                happensAtIER(stop_end(Vessel), _), delay(stop_start(Vessel), _), holdsForDelays(Vessel, withinArea(Vessel,nearPorts)=true, [_|_]),
                                delay(stop_end(Vessel), _)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simpleFluent(stopped(_) = farFromPorts).
outputEntity(stopped(_) = farFromPorts).
keyOfEvent(stopped(_) = farFromPorts, '18').
itIsChild(stopped(_) = farFromPorts).
index(stopped(Vessel) = farFromPorts, Vessel).

rgraph(stopped(Vessel) = farFromPorts, [[[(R1, '1e'), (R2,'1f')]], [[(R3,'1e')]], [(R4,'1e'),(R5,'1f')], [(R6,'1e')]], [R1,R2,R3,R4,R5,R6]).
dependency(stopped(Vessel) = farFromPorts, [happensAtIER(stop_start(Vessel), _), holdsForDelays(Vessel, withinArea(Vessel,nearPorts)=true, [_|_]),
                                happensAtIER(stop_end(Vessel), _), delay(stop_start(Vessel), _), holdsForRetractions(Vessel, withinArea(Vessel,nearPorts)=true, [_|_]),
                                delay(stop_end(Vessel), _)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simpleFluent(lowSpeed(_) = true).
outputEntity(lowSpeed(_) = true).
keyOfEvent(lowSpeed(_) = true, '19').
itIsChild(lowSpeed(_) = true).
index(lowSpeed(Vessel) = true, Vessel).

rgraph(lowSpeed(Vessel) = true, [[[(R1,'1e')]], [[(R2,'1e')]], [(R3,'1e')], [(R4,'1e')]], [R1,R2,R3,R4]).
dependency(lowSpeed(Vessel) = true, [happensAtIER(slow_motion_start(Vessel), _), happensAtIER(slow_motion_end(Vessel), _),
                                delay(slow_motion_start(Vessel), _), delay(slow_motion_end(Vessel), _)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simpleFluent(withinArea(_,anchorage)=true).
outputEntity(withinArea(_,anchorage)=true).
keyOfEvent(withinArea(_,anchorage)=true, '20').
index(withinArea(Vessel,anchorage)=true, Vessel).

simpleFluent(withinArea(_,fishing)=true).
outputEntity(withinArea(_,fishing)=true).
keyOfEvent(withinArea(_,fishing)=true, '21').
itIsChild(withinArea(_,fishing)=true).
index(withinArea(Vessel,fishing)=true, Vessel).

simpleFluent(withinArea(_,natura)=true).
outputEntity(withinArea(_,natura)=true).
keyOfEvent(withinArea(_,natura)=true, '22').
index(withinArea(Vessel,natura)=true, Vessel).

simpleFluent(withinArea(_,nearCoast)=true).
outputEntity(withinArea(_,nearCoast)=true).
keyOfEvent(withinArea(_,nearCoast)=true, '23').
index(withinArea(Vessel,nearCoast)=true, Vessel).

simpleFluent(withinArea(_,nearCoast5k)=true).
outputEntity(withinArea(_,nearCoast5k)=true).
keyOfEvent(withinArea(_,nearCoast5k)=true, '24').
index(withinArea(Vessel,nearCoast5k)=true, Vessel).

simpleFluent(withinArea(_,nearPorts)=true).
outputEntity(withinArea(_,nearPorts)=true).
keyOfEvent(withinArea(_,nearPorts)=true, '25').
itIsChild(withinArea(_,nearPorts)=true).
index(withinArea(Vessel,nearPorts)=true, Vessel).

rgraph(withinArea(Vessel,_)=true, [[[(R1, '1e')]], [[(R2, '1e')], [(R3,'2e')]], [(R4,'1e')], [(R5, '1e'), (R6, '2e')]], [R1,R2,R3,R4,R5,R6]).
dependency(withinArea(Vessel,_)=true, [happensAtIER(entersArea(Vessel), _), happensAtIER(leavesArea(Vessel), _), happensAtIER(gap_start(Vessel), _),
                                delay(entersArea(Vessel), _), delay(leavesArea(Vessel), _), delay(gap_start(Vessel), _)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simpleFluent(underWay(_)=true).
outputEntity(underWay(_)=true).
keyOfEvent(underWay(_)=true, '26').
itIsChild(underWay(_)=true).
index(underWay(Vessel)=true, Vessel).

rgraph(underWay(Vessel) = true, [[[(R1, '1e')]], [[(R1, '1e')]], [(R2, '1e')], [(R2, '1e')]], [R1,R2]).
dependency(underWay(Vessel) = true, [happensAtIER(velocity(Vessel), _), delay(velocity(Vessel), _)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simpleFluent(adrift(_)=true).
outputEntity(adrift(_)=true).
keyOfEvent(adrift(_)=true, '27').
index(adrift(Vessel)=true, Vessel).
rgraph(adrift(Vessel) = true, [[[(R1, '1e'), (R2, '1f')]], [[(R1, '1e'), (R2, '1f')], [(R3, '2f')]], [(R4, '1e'), (R3, '1f')], [(R4, '1e'), (R3, '1f'), (R4, '2e'), (R2, '2f')]], [R1,R2,R3,R4]).
dependency(adrift(Vessel) = true, [happensAtIER(velocity(Vessel), _), holdsForRetractions(Vessel, underWay(Vessel) = true, [_|_]), holdsForDelays(Vessel, underWay(Vessel) = true, [_|_]),
                                delay(velocity(Vessel), _)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simpleFluent(anchor(_)=nearPort).
outputEntity(anchor(_)=nearPort).
keyOfEvent(anchor(_)=nearPort, '28').
itIsChild(anchor(_)=nearPort).
index(anchor(Vessel)=nearPort, Vessel).

rgraph(anchor(Vessel) = nearPort, [[[(R1, '1e'), (R2, '1f')]], [[(R1, '1e'), (R3, '1f')]], [(R4,'1e'), (R3, '1f')], [(R4, '1e'), (R2, '1f'), (R4, '2e')]], [R1,R2,R3,R4]).
dependency(anchor(Vessel) = nearPort, [happensAtIER(velocity(Vessel), _), holdsForRetractions(Vessel, withinArea(Vessel,nearPorts) = true, [_|_]),
                                holdsForDelays(Vessel, withinArea(Vessel,nearPorts) = true, [_|_]), delay(velocity(Vessel), _)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simpleFluent(anchor(_)=farFromPorts).
outputEntity(anchor(_)=farFromPorts).
keyOfEvent(anchor(_)=farFromPorts, '29').
index(anchor(Vessel)=farFromPorts, Vessel).

rgraph(anchor(Vessel) = farFromPorts, [[[(R1, '1e'), (R2, '1f')]], [[(R1, '1e'), (R3, '1f')]], [(R4,'1e'), (R3, '1f')], [(R4, '1e'), (R2, '1f'), (R4, '2e')]], [R1,R2,R3,R4]).
dependency(anchor(Vessel) = farFromPorts, [happensAtIER(velocity(Vessel), _), holdsForDelays(Vessel, withinArea(Vessel,nearPorts) = true, [_|_]),
                                holdsForRetractions(Vessel, withinArea(Vessel,nearPorts) = true, [_|_]), delay(velocity(Vessel), _)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%itIsChild(atAnchor_1(_) = nearPort).
index(atAnchor_1(Vessel) = nearPort, Vessel).

sDFluent2(atAnchor(_) = nearPort) .
outputEntity(atAnchor(_) = nearPort).
keyOfEvent(atAnchor(_) = nearPort, '30').
index(atAnchor(Vessel) = nearPort, Vessel).
dependency(atAnchor(Vessel)=nearPort, [(Vessel, atAnchor_1(Vessel)=nearPort)]).

mgraph(atAnchor_1(Vessel)=nearPort, [(atAnchor_1(Vessel)=nearPort, I, D, O, R)], I, D, O, R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sDFluent(atAnchor(_) = farFromPorts).
outputEntity(atAnchor(_) = farFromPorts).
keyOfEvent(atAnchor(_) = farFromPorts, '31').
index(atAnchor(Vessel) = farFromPorts, Vessel).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simpleFluent(beforeaground(_) = nearPort).
outputEntity(beforeaground(_) = nearPort).
keyOfEvent(beforeaground(_) = nearPort, '32').
index(beforeaground(Vessel) = nearPort, Vessel).

rgraph(beforeaground(Vessel) = nearPort, [[[(R1, '1e'), (R2, '1f')]], [[(R1, '1e')]], [(R3,'1e'), (R4, '1f')], [(R3,'1e')]], [R1,R2,R3, R4]).
dependency(beforeaground(Vessel) = nearPort, [happensAtIER(velocity(Vessel), _), holdsForRetractions(Vessel, withinArea(Vessel,nearPorts) = true, [_|_]),
                                delay(velocity(Vessel), _), holdsForDelays(Vessel, withinArea(Vessel,nearPorts) = true, [_|_])]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simpleFluent(beforeaground(_) = farFromPorts).
outputEntity(beforeaground(_) = farFromPorts).
keyOfEvent(beforeaground(_) = farFromPorts, '33').
index(beforeaground(Vessel) = farFromPorts, Vessel).

rgraph(beforeaground(Vessel) = farFromPorts, [[[(R1, '1e'), (R2, '1f')]], [[(R1, '1e')]], [(R3,'1e'), (R4, '1f')], [(R3,'1e')]], [R1,R2,R3, R4]).
dependency(beforeaground(Vessel) = farFromPorts, [happensAtIER(velocity(Vessel), _), holdsForDelays(Vessel, withinArea(Vessel,nearPorts) = true, [_|_]),
                                delay(velocity(Vessel), _), holdsForRetractions(Vessel, withinArea(Vessel,nearPorts) = true, [_|_])]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sDFluent(aground(_) = nearPort).
outputEntity(aground(_) = nearPort).
keyOfEvent(aground(_) = nearPort, '34').
itIsChild(aground(_) = nearPort).
index(aground(Vessel) = nearPort, Vessel).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sDFluent(aground(_) = farFromPorts).
outputEntity(aground(_) = farFromPorts).
keyOfEvent(aground(_) = farFromPorts, '35').
index(aground(Vessel) = farFromPorts, Vessel).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simpleFluent(trawlSpeed(_)=true).
outputEntity(trawlSpeed(_)=true).
keyOfEvent(trawlSpeed(_)=true, '36').
itIsChild(trawlSpeed(_)=true).
index(trawlSpeed(Vessel)=true, Vessel).

rgraph(trawlSpeed(Vessel)=true, [[[(R1, '1e'), (R2, '1f')]], [[(R1, '1e'), (R3, '1f')]], [(R4,'1e'), (R3,'1f')], [(R4,'1e'), (R4, '2e'), (R2,'2f')]], [R1,R2,R3,R4]).
dependency(trawlSpeed(Vessel)=true, [happensAtIER(velocity(Vessel), _), holdsForDelays(Vessel, withinArea(Vessel,nearPorts)=true, [_|_]),
                                holdsForRetractions(Vessel, withinArea(Vessel,nearPorts)=true, [_|_]), delay(velocity(Vessel), _)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simpleFluent(speedLessThanMin(_)=true).
outputEntity(speedLessThanMin(_)=true).
keyOfEvent(speedLessThanMin(_)=true, '37').
index(speedLessThanMin(Vessel)=true, Vessel).

rgraph(speedLessThanMin(Vessel)=true, [[[(R1, '1e'), (R2, '1e2')]], [[(R1,'1e')]], [(R3, '1e')], [(R3, '1e')]], [R1,R2,R3]).
dependency(speedLessThanMin(Vessel)=true, [happensAtIER(velocity(Vessel), _), delay(gap_start(Vessel), _), delay(velocity(Vessel), _)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simpleFluent(speedGrThanMax(_)=true).
outputEntity(speedGrThanMax(_)=true).
keyOfEvent(speedGrThanMax(_)=true, '38').
index(speedGrThanMax(Vessel)=true, Vessel).

rgraph(speedGrThanMax(Vessel)=true, [[[(R1, '1e'), (R2, '1e2')]], [[(R1,'1e')]], [(R3, '1e')], [(R3, '1e')]], [R1,R2,R3]).
dependency(speedGrThanMax(Vessel)=true, [happensAtIER(velocity(Vessel), _), delay(gap_start(Vessel), _), delay(velocity(Vessel), _)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simpleFluent(travelSpeed(_)=true).
outputEntity(travelSpeed(_)=true).
keyOfEvent(travelSpeed(_)=true, '39').
itIsChild(travelSpeed(_)=true).
index(travelSpeed(Vessel)=true, Vessel).

rgraph(travelSpeed(Vessel)=true, [[[(R1, '1e'), (R2, '1e2')]], [[(R1,'1e')]], [(R3, '1e')], [(R3, '1e')]], [R1,R2,R3]).
dependency(travelSpeed(Vessel)=true, [happensAtIER(velocity(Vessel), _), delay(gap_start(Vessel), _), delay(velocity(Vessel), _)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

itIsChild(rendezVous_1(_,_)=true).
index(rendezVous_1(Vessel,_)=true, Vessel).

itIsChild(rendezVous_2(_,_)=true).
index(rendezVous_2(Vessel,_)=true, Vessel).

itIsChild(rendezVous_3(_,_)=true).
index(rendezVous_3(Vessel,_)=true, Vessel).

%itIsChild(rendezVous_4(_,_)=true).
index(rendezVous_4(Vessel,_)=true, Vessel).

sDFluent2(rendezVous(_,_)=true).
outputEntity(rendezVous(_,_)=true).
keyOfEvent(rendezVous(_,_)=true, '40').
index(rendezVous(Vessel,_)=true, Vessel).
dependency(rendezVous(Vessel,Vessel2)=true, [(Vessel, rendezVous_1(Vessel,Vessel2)=true), (Vessel2, rendezVous_2(Vessel,Vessel2)=true), (Vessel, rendezVous_3(Vessel,Vessel2)=true), (Vessel, rendezVous_4(Vessel,Vessel2)=true)]).

mgraph(rendezVous_1(Vessel,Vessel2)=true, [(rendezVous_1(Vessel,Vessel2)=true, I, D, O, R), _, _, _], I, D, O, R).
mgraph(rendezVous_2(Vessel,Vessel2)=true, [_, (rendezVous_2(Vessel,Vessel2)=true, I, D, O, R), _, _], I, D, O, R).
mgraph(rendezVous_3(Vessel,Vessel2)=true, [_, _, (rendezVous_3(Vessel,Vessel2)=true, I, D, O, R), _], I, D, O, R).
mgraph(rendezVous_4(Vessel,Vessel2)=true, [_, _, _, (rendezVous_4(Vessel,Vessel2)=true, I, D, O, R)], I, D, O, R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%itIsChild(trawling_1(_)=true).
index(trawling_1(Vessel)=true, Vessel).

sDFluent2(trawling(_)=true).
outputEntity(trawling(_)=true).
keyOfEvent(trawling(_)=true, '41').
index(trawling(Vessel)=true, Vessel).
dependency(trawling(Vessel)=true, [(Vessel, trawling_1(Vessel)=true)]).

mgraph(trawling_1(Vessel)=true, [(trawling_1(Vessel)=true, I, D, O, R)], I, D, O, R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

itIsChild(unusualSpeed_1(_)=true).
index(unusualSpeed_1(Vessel)=true, Vessel).

sDFluent2(unusualSpeed(_)=true).
outputEntity(unusualSpeed(_)=true).
keyOfEvent(unusualSpeed(_)=true, '42').
index(unusualSpeed(Vessel)=true, Vessel).
dependency(unusualSpeed(Vessel)=true, [(Vessel, unusualSpeed_1(Vessel)=true)]).

mgraph(unusualSpeed_1(Vessel)=true, [(unusualSpeed_1(Vessel)=true, I, D, O, R)], I, D, O, R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% for input entities/statically determined fluents state whether 
% the intervals will be collected into a list or built from given time-points

collectIntervals(proximity(_,_)=true).

% define the groundings of the fluents and output entities/events

grounding(proximity(Vessel1,Vessel2) = true)                :- vessel(Vessel1), vessel(Vessel2), Vessel1 =\= Vessel2.
grounding(gap(Vessel) = PortStatus)                         :- vessel(Vessel),portstatus(PortStatus).
grounding(stopped(Vessel) = PortStatus)                     :- vessel(Vessel),portstatus(PortStatus).
grounding(lowSpeed(Vessel) = true)                          :- vessel(Vessel).
grounding(withinArea(Vessel,AreaType) = true)               :- vessel(Vessel),areaType(AreaType).
grounding(illegalFishing(Vessel)=true)                      :- vessel(Vessel).
grounding(fastApproach(Vessel))                             :- vessel(Vessel).
%grounding(suspiciousDelay(Vessel) = true)                  :- vessel(Vessel).
grounding(underWay(Vessel) = true)                          :- vessel(Vessel).
grounding(adrift(Vessel) = true)                            :- vessel(Vessel).
grounding(beforeaground(Vessel) = PortStatus )              :- vessel(Vessel),portstatus(PortStatus).
grounding(aground(Vessel) = PortStatus)                     :- vessel(Vessel),portstatus(PortStatus).
grounding(anchor(Vessel) = PortStatus)                      :- vessel(Vessel),portstatus(PortStatus).
grounding(atAnchor(Vessel) = PortStatus)                    :- vessel(Vessel),portstatus(PortStatus).
grounding(trawlSpeed(Vessel) = true)                        :- vessel(Vessel).
grounding(travelSpeed(Vessel) = true)                       :- vessel(Vessel).
grounding(speedLessThanMin(Vessel) = true)                  :- vessel(Vessel).
grounding(speedGrThanMax(Vessel) = true)                    :- vessel(Vessel).
grounding(tugging(Vessel1,Vessel2) = true)                  :- vessel(Vessel1), vessel(Vessel2), Vessel1 =\= Vessel2.
grounding(rendezVous(Vessel1,Vessel2) = true)               :- vessel(Vessel1), vessel(Vessel2), Vessel1 =\= Vessel2.
grounding(trawling(Vessel) = true)                          :- vessel(Vessel).
grounding(unusualSpeed(Vessel) = true)                      :- vessel(Vessel).

% cachingOrder should be defined for all output entities

cachingOrder(gap(_) = nearPort).
cachingOrder(gap(_) = farFromPorts).
cachingOrder(stopped(_) = nearPort).
cachingOrder(stopped(_) = farFromPorts).
cachingOrder(lowSpeed(_) = true).
cachingOrder(withinArea(_,fishing) = true).
cachingOrder(withinArea(_,natura) = true).
cachingOrder(illegalFishing(_)=true).
cachingOrder(fastApproach(_)).
%cachingOrder(suspiciousDelay(_) = true).
cachingOrder(underWay(_) = true).
cachingOrder(adrift(_) = true).
cachingOrder(beforeaground(_) = nearPort).
cachingOrder(beforeaground(_) = farFromPorts).
cachingOrder(aground(_) = nearPort).
cachingOrder(aground(_) = farFromPorts).
cachingOrder(anchor(_) = nearPort).
cachingOrder(anchor(_) = farFromPorts).
cachingOrder(atAnchor(_) = nearPort).
cachingOrder(atAnchor(_) = farFromPorts).
cachingOrder(trawlSpeed(_) = true).
cachingOrder(travelSpeed(_) = true).
cachingOrder(speedLessThanMin(_) = true).
cachingOrder(speedGrThanMax(_) = true).
cachingOrder(tugging(_,_) = true).
cachingOrder(rendezVous(_,_) = true).
%cachingOrder(possibleRendezvouz(_,_) = true).
cachingOrder(trawling(_) = true).
cachingOrder(unusualSpeed(_) = true).

needsGrounding(_, _, _) :- fail.
buildFromPoints(_) :- fail.
