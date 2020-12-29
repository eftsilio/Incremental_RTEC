
:- discontiguous initiatedAt_Retract/3, initiatedAt_Delay/3, terminatedAt_Retract/3, terminatedAt_Delay/3, initiatedAt_OnTime/3, terminatedAt_OnTime/3, terminatedAt/3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% GAP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initiatedAt_Retract(gap(Index)=nearPort, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, gap_start(Index), [T]).

initiatedAt_Retract(gap(Index)=nearPort, T, '1f') :-
	holdsAtProcessedSimpleFluent_Retractions(Index, withinArea(Index,nearPorts)=true, T).


initiatedAt_Retract(gap(Index)=farFromPorts, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, gap_start(Index), [T]).

initiatedAt_Retract(gap(Index)=farFromPorts, T, '1f') :-
	holdsAtProcessedSimpleFluent_Delays(Index, withinArea(Index,nearPorts)=true, T).


initiatedAt_Delay(gap(Index)=nearPort, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, gap_start(Index), [T]),
	holdsAtProcessedSimpleFluent(Index, withinArea(Index,nearPorts)=true, T).

initiatedAt_Delay(gap(Index)=nearPort, T, '1f') :-
	holdsForDelays(Index, withinArea(Index,nearPorts)=true, L),
	happensAtProcessedStaticIE_Qi_notDelays(Index, gap_start(Index), [T]),
	tinIntervals(T, L).
	

initiatedAt_Delay(gap(Index)=farFromPorts, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, gap_start(Index), [T]),
	\+ holdsAtProcessedSimpleFluent(Index, withinArea(Index,nearPorts)=true, T).

initiatedAt_Delay(gap(Index)=farFromPorts, T, '1f') :-
	holdsForRetractions(Index, withinArea(Index,nearPorts)=true, L),
	happensAtProcessedStaticIE_Qi_notDelays(Index, gap_start(Index), [T]),
	tinIntervals(T, L).
	

terminatedAt_Retract(gap(Index)=_PortStatus, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, gap_end(Index), [T]).


terminatedAt_Delay(gap(Index)=_PortStatus, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, gap_end(Index), [T]).


%%%%%On time

initiatedAt_OnTime(gap(Index)=nearPort, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, gap_start(Index), [T]),
	holdsAtProcessedSimpleFluent_OnTime(Index, withinArea(Index,nearPorts)=true, T).

     
initiatedAt_OnTime(gap(Index)=farFromPorts, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, gap_start(Index), [T]),
	\+ holdsAtProcessedSimpleFluent_OnTime(Index, withinArea(Index,nearPorts)=true, T).


terminatedAt_OnTime(gap(Index)=_PortStatus, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, gap_end(Index), [T]).

%%%%%%% Termination all over points

terminatedAt(gap(Index)=_PortStatus, T, 'a') :-
	happensAtProcessedStaticIE_Over(Index, gap_end(Index), [T]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% STOPPED %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


initiatedAt_Retract(stopped(Index)=nearPort, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, stop_start(Index), [T]).

initiatedAt_Retract(stopped(Index)=nearPort, T, '1f') :-
	holdsAtProcessedSimpleFluent_Retractions(Index, withinArea(Index,nearPorts)=true, T).


initiatedAt_Retract(stopped(Index)=farFromPorts, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, stop_start(Index), [T]).


initiatedAt_Retract(stopped(Index)=farFromPorts, T, '1f') :-
	holdsAtProcessedSimpleFluent_Delays(Index, withinArea(Index,nearPorts)=true, T).

     
initiatedAt_Delay(stopped(Index)=nearPort, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, stop_start(Index), [T]),
	\+ happensAtProcessedStaticIE_Delays(Index, gap_start(Index), [T]),
	holdsAtProcessedSimpleFluent(Index, withinArea(Index,nearPorts)=true, T).

initiatedAt_Delay(stopped(Index)=nearPort, T, '1f') :-
	holdsForDelays(Index, withinArea(Index,nearPorts)=true, L),
	happensAtProcessedStaticIE_Qi_notDelays(Index, stop_start(Index), [T]),
	\+ happensAtProcessedStaticIE_Qi_notDelays(Index, gap_start(Index), [T]),
	tinIntervals(T, L).


initiatedAt_Delay(stopped(Index)=farFromPorts, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, stop_start(Index), [T]),
	\+ happensAtProcessedStaticIE_Delays(Index, gap_start(Index), [T]),
	\+ holdsAtProcessedSimpleFluent(Index, withinArea(Index,nearPorts)=true, T).


initiatedAt_Delay(stopped(Index)=farFromPorts, T, '1f') :-
	holdsForRetractions(Index, withinArea(Index,nearPorts)=true, L),
	happensAtProcessedStaticIE_Qi_notDelays(Index, stop_start(Index), [T]),
	\+ happensAtProcessedStaticIE_Qi_notDelays(Index, gap_start(Index), [T]),
	tinIntervals(T, L).


terminatedAt_Retract(stopped(Index)=_PortStatus, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, stop_end(Index), [T]).

%terminatedAt_Retract(stopped(Index)=_PortStatus, T) :-
%     happensAtProcessedSimpleFluent_Retractions(Index, start(gap(Index)=_), T).

terminatedAt_Delay(stopped(Index)=_PortStatus, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, stop_end(Index), [T]).
     
%%%%%On time

initiatedAt_OnTime(stopped(Index)=nearPort, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, stop_start(Index), [T]),
	\+ happensAtProcessedStaticIE_OnTime(Index, gap_start(Index), [T]),
	holdsAtProcessedSimpleFluent_OnTime(Index, withinArea(Index,nearPorts)=true, T).

initiatedAt_OnTime(stopped(Index)=farFromPorts, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, stop_start(Index), [T]),
	\+ happensAtProcessedStaticIE_OnTime(Index, gap_start(Index), [T]),
	\+ holdsAtProcessedSimpleFluent_OnTime(Index, withinArea(Index,nearPorts)=true, T).

terminatedAt_OnTime(stopped(Index)=_PortStatus, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, stop_end(Index), [T]).

%terminatedAt_OnTime(stopped(Index)=_PortStatus, T) :-
%     happensAtProcessedSimpleFluent(Index, start(gap(Index)=_), T).


%%%%%%% Termination all over points

terminatedAt(stopped(Index)=_PortStatus, T, 'a') :-
	happensAtProcessedStaticIE_Over(Index, stop_end(Index), [T]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%% LOWSPEED %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initiatedAt_Retract(lowSpeed(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, slow_motion_start(Index), [T]).


initiatedAt_Delay(lowSpeed(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, slow_motion_start(Index), [T]),
	\+ happensAtProcessedStaticIE_Delays(Index, gap_start(Index), [T]).


terminatedAt_Retract(lowSpeed(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, slow_motion_end(Index), [T]).

%terminatedAt_Retract(lowSpeed(Index)=true, T) :-
%     happensAtProcessedSimpleFluent_Retractions(Index, start(gap(Index)=_), T).
     
terminatedAt_Delay(lowSpeed(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, slow_motion_end(Index), [T]).
     

%%%%%On time

initiatedAt_OnTime(lowSpeed(Index)=true, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, slow_motion_start(Index), [T]),
	\+ happensAtProcessedStaticIE_OnTime(Index, gap_start(Index), [T]).
     
terminatedAt_OnTime(lowSpeed(Index)=true, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, slow_motion_end(Index), [T]).

%terminatedAt_OnTime(lowSpeed(Index)=true, T) :-
%     happensAtProcessedSimpleFluent(Index, start(gap(Index)=_), T).


%%%%%%% Termination all over points

terminatedAt(lowSpeed(Index)=true, T, 'a') :-
	happensAtProcessedStaticIE_Over(Index, slow_motion_end(Index), [T]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% WITHIN_AREA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


initiatedAt_Retract(withinArea(Index,_)=true,T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, entersArea(Index), [T,_]).


initiatedAt_Delay(withinArea(Index,Type)=true, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, entersArea(Index), [T,Area]),
	bigAreaType(Area,Type).


terminatedAt_Retract(withinArea(Index,_)=true, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, leavesArea(Index), [T,_]).
     
terminatedAt_Retract(withinArea(Index,_)=true, T, '2e') :-
	happensAtProcessedStaticIE_Retractions(Index, gap_start(Index), [T]).


terminatedAt_Delay(withinArea(Index,Type)=true, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, leavesArea(Index), [T,Area]),
	bigAreaType(Area,Type).

terminatedAt_Delay(withinArea(Index,_)=true, T, '2e') :-
	happensAtProcessedStaticIE_Delays(Index, gap_start(Index), [T]).

%%%%On time

initiatedAt_OnTime(withinArea(Index,Type)=true, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, entersArea(Index), [T,Area]),
	bigAreaType(Area,Type).

terminatedAt_OnTime(withinArea(Index,Type)=true, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, leavesArea(Index), [T,Area]),
	bigAreaType(Area,Type).

terminatedAt_OnTime(withinArea(Index,_)=true, T, 'b') :-
	happensAtProcessedStaticIE_OnTime(Index, gap_start(Index), [T]).


%%%%%%% Termination all over points

terminatedAt(withinArea(Index,Type)=true, T, 'a') :-
	happensAtProcessedStaticIE_Over(Index, leavesArea(Index), [T,Area]),
	bigAreaType(Area,Type).

terminatedAt(withinArea(Index,_)=true, T, 'b') :-
	happensAtProcessedStaticIE_Over(Index, gap_start(Index), [T]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% UNDER_WAY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     
     
initiatedAt_Retract(underWay(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, velocity(Index), [T|_]).

     
initiatedAt_Delay(underWay(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, velocity(Index), [T,Speed|_]),
	\+ happensAtProcessedStaticIE_Delays(Index, gap_start(Index), [T]),
	Speed>2.7,
	Speed<48.6.


terminatedAt_Retract(underWay(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, velocity(Index), [T|_]).

%terminatedAt_Retract(underWay(Index)=true, T) :-
%     happensAtProcessedSimpleFluent_Retractions(Index, start(gap(Index)=_), T).

terminatedAt_Delay(underWay(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, velocity(Index), [T,Speed|_]),
	(Speed>48.6 ; Speed<2.7).
     

%%%%On time

initiatedAt_OnTime(underWay(Index)=true, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, velocity(Index), [T,Speed|_]),
	\+ happensAtProcessedStaticIE_OnTime(Index, gap_start(Index), [T]),
	Speed>2.7,
	Speed<48.6.

terminatedAt_OnTime(underWay(Index)=true, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, velocity(Index), [T,Speed|_]),
	(Speed>48.6 ; Speed<2.7).

%terminatedAt_OnTime(underWay(Index)=true, T) :-
%     happensAtProcessedSimpleFluent(Index, start(gap(Index)=_), T).


%%%%%%% Termination all over points

terminatedAt(underWay(Index)=true, T, 'a') :-
	happensAtProcessedStaticIE_Over(Index, velocity(Index), [T,Speed|_]),
	(Speed>48.6 ; Speed<2.7).
     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ADRIFT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%          

initiatedAt_Retract(adrift(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, velocity(Index), [T|_]).

initiatedAt_Retract(adrift(Index)=true, T, '1f') :-
	holdsAtProcessedSimpleFluent_Retractions(Index, underWay(Index)=true, T).

     
initiatedAt_Delay(adrift(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, velocity(Index), [T,_,CoG,Thead]),
	\+ happensAtProcessedStaticIE_Delays(Index, gap_start(Index), [T]),
	holdsAtProcessedSimpleFluent(Index, underWay(Index)=true, T),
	absoluteAngleDiff(CoG,Thead,AngleDiff),
	AngleDiff>15.

initiatedAt_Delay(adrift(Index)=true, T, '1f') :-
	holdsForDelays(Index, underWay(Index)=true, L),
	happensAtProcessedStaticIE_Qi_notDelays(Index, velocity(Index), [T,_,CoG,Thead]),
	\+ happensAtProcessedStaticIE_Qi_notDelays(Index, gap_start(Index), [T]),
	tinIntervals(T, L),
	absoluteAngleDiff(CoG,Thead,AngleDiff),
	AngleDiff>15.


terminatedAt_Retract(adrift(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, velocity(Index), [T|_]).

terminatedAt_Retract(adrift(Index)=true, T, '1f') :-
	holdsAtProcessedSimpleFluent_Retractions(Index, underWay(Index)=true, T).


%terminatedAt_Retract(adrift(Index)=true, T) :-
%     happensAtProcessedSimpleFluent_Retractions(Index, start(gap(Index)=_), T).

terminatedAt_Retract(adrift(Index)=true, T, '2f') :-
	holdsForDelays(Index, underWay(Index)=true, L),
	happensAtProcessedStaticIE_Qi_notDelays(Index, velocity(Index), [T|_]),
	tinIntervals(T, L).
	

terminatedAt_Delay(adrift(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, velocity(Index), [T,_,CoG,Thead]),
	Thead\=0,
	\+ happensAtProcessedStaticIE_Delays(Index, gap_start(Index), [T]),
	holdsAtProcessedSimpleFluent(Index, underWay(Index)=true, T),
	absoluteAngleDiff(CoG,Thead,AngleDiff),
	AngleDiff=<15.


terminatedAt_Delay(adrift(Index)=true, T, '1f') :-
	holdsForDelays(Index, underWay(Index)=true, L),
	happensAtProcessedStaticIE_Qi_notDelays(Index, velocity(Index), [T,_,CoG,Thead]),
	Thead\=0,
	\+ happensAtProcessedStaticIE_Qi_notDelays(Index, gap_start(Index), [T]),
	tinIntervals(T, L),
	absoluteAngleDiff(CoG,Thead,AngleDiff),
	AngleDiff=<15.

terminatedAt_Delay(adrift(Index)=true, T, '2e') :-
	happensAtProcessedStaticIE_Delays(Index, velocity(Index), [T|_]),
	\+ holdsAtProcessedSimpleFluent(Index, underWay(Index)=true, T).

terminatedAt_Delay(adrift(Index)=true, T, '2f') :-
	holdsForRetractions(Index, underWay(Index)=true, L),
	happensAtProcessedStaticIE_Qi_notDelays(Index, velocity(Index), [T|_]),
	tinIntervals(T, L).
	     

%%%%On time

initiatedAt_OnTime(adrift(Index)=true, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, velocity(Index), [T,_,CoG,Thead]),
	\+ happensAtProcessedStaticIE_OnTime(Index, gap_start(Index), [T]),
	holdsAtProcessedSimpleFluent_OnTime(Index, underWay(Index)=true, T),
	absoluteAngleDiff(CoG,Thead,AngleDiff),
	AngleDiff>15.

terminatedAt_OnTime(adrift(Index)=true, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, velocity(Index), [T,_,CoG,Thead]),
	Thead\=0,
	\+ happensAtProcessedStaticIE_OnTime(Index, gap_start(Index), [T]),
	holdsAtProcessedSimpleFluent_OnTime(Index, underWay(Index)=true, T),
	absoluteAngleDiff(CoG,Thead,AngleDiff),
	AngleDiff=<15.

%terminatedAt_OnTime(adrift(Index)=true, T) :-
%     happensAtProcessedSimpleFluent(Index, start(gap(Index)=_), T).

terminatedAt_OnTime(adrift(Index)=true, T, 'b') :-
	happensAtProcessedStaticIE_OnTime(Index, velocity(Index), [T|_]),
	\+ holdsAtProcessedSimpleFluent_OnTime(Index, underWay(Index)=true, T).


%%%%%%% Termination all over points

terminatedAt(adrift(Index)=true, T, 'a') :-
	happensAtProcessedStaticIE_Over(Index, velocity(Index), [T,_,CoG,Thead]),
	Thead\=0,
	\+ happensAtProcessedStaticIE_Over(Index, gap_start(Index), [T]),
	holdsAtProcessedSimpleFluent(Index, underWay(Index)=true, T),
	absoluteAngleDiff(CoG,Thead,AngleDiff),
	AngleDiff=<15.

terminatedAt(adrift(Index)=true, T, 'b') :-
	happensAtProcessedStaticIE_Over(Index, velocity(Index), [T|_]),
	\+ holdsAtProcessedSimpleFluent(Index, underWay(Index)=true, T).
     
     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% BEFOREAGROUND %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%               

initiatedAt_Retract(beforeaground(Index)=nearPort, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, velocity(Index), [T|_]).

initiatedAt_Retract(beforeaground(Index)=nearPort, T, '1f') :-
	holdsAtProcessedSimpleFluent_Retractions(Index, withinArea(Index,nearPorts)=true, T).



initiatedAt_Retract(beforeaground(Index)=farFromPorts, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, velocity(Index), [T|_]).

initiatedAt_Retract(beforeaground(Index)=farFromPorts, T, '1f') :-
	holdsAtProcessedSimpleFluent_Delays(Index, withinArea(Index,nearPorts)=true, T).


initiatedAt_Delay(beforeaground(Index)=nearPort, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, velocity(Index), [T,Speed|_]),
	\+ happensAtProcessedStaticIE_Delays(Index, gap_start(Index), [T]),
	holdsAtProcessedSimpleFluent(Index, withinArea(Index,nearPorts)=true, T),
	Speed<0.005.


initiatedAt_Delay(beforeaground(Index)=nearPort, T, '1f') :-
	holdsForDelays(Index, withinArea(Index,nearPorts)=true, L),
	happensAtProcessedStaticIE_Qi_notDelays(Index, velocity(Index), [T,Speed|_]),
	\+ happensAtProcessedStaticIE_Qi_notDelays(Index, gap_start(Index), [T]),
	tinIntervals(T, L),
	Speed<0.005.


initiatedAt_Delay(beforeaground(Index)=farFromPorts, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, velocity(Index), [T,Speed|_]),
	\+ happensAtProcessedStaticIE_Delays(Index, gap_start(Index), [T]),
	\+ holdsAtProcessedSimpleFluent(Index, withinArea(Index,nearPorts)=true, T),
	Speed<0.005.


initiatedAt_Delay(beforeaground(Index)=farFromPorts, T, '1f') :-
	holdsForRetractions(Index, withinArea(Index,nearPorts)=true, L),
	happensAtProcessedStaticIE_Qi_notDelays(Index, velocity(Index), [T,Speed|_]),
	\+ happensAtProcessedStaticIE_Qi_notDelays(Index, gap_start(Index), [T]),
	tinIntervals(T, L),
	Speed<0.005.

     
terminatedAt_Retract(beforeaground(Index)=_, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, velocity(Index), [T|_]).

%terminatedAt_Retract(beforeaground(Index)=_, T) :-
%     happensAtProcessedSimpleFluent_Retractions(Index, start(gap(Index)=_), T).
     
terminatedAt_Delay(beforeaground(Index)=_, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, velocity(Index), [T,Speed|_]),
	Speed>0.005.
     

%%%%On time

initiatedAt_OnTime(beforeaground(Index)=nearPort, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, velocity(Index), [T,Speed|_]),
	\+ happensAtProcessedStaticIE_OnTime(Index, gap_start(Index), [T]),
	holdsAtProcessedSimpleFluent_OnTime(Index, withinArea(Index,nearPorts)=true, T),
	Speed<0.005.

initiatedAt_OnTime(beforeaground(Index)=farFromPorts, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, velocity(Index), [T,Speed|_]),
	\+ happensAtProcessedStaticIE_OnTime(Index, gap_start(Index), [T]),
	\+ holdsAtProcessedSimpleFluent_OnTime(Index, withinArea(Index,nearPorts)=true, T),
	Speed<0.005.     
     
terminatedAt_OnTime(beforeaground(Index)=_, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, velocity(Index), [T,Speed|_]),
	Speed>0.005.

%terminatedAt_OnTime(beforeaground(Index)=_, T) :-
%     happensAtProcessedSimpleFluent(Index, start(gap(Index)=_), T).


%%%%%%% Termination all over points

terminatedAt(beforeaground(Index)=_, T, 'a') :-
	happensAtProcessedStaticIE_Over(Index, velocity(Index), [T,Speed|_]),
	Speed>0.005.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ANCHOR %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initiatedAt_Retract(anchor(Index)=nearPort, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, velocity(Index), [T|_]).

initiatedAt_Retract(anchor(Index)=nearPort, T, '1f') :-
	holdsAtProcessedSimpleFluent_Retractions(Index, withinArea(Index,nearPorts)=true, T).


initiatedAt_Retract(anchor(Index)=farFromPorts, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, velocity(Index), [T|_]).

initiatedAt_Retract(anchor(Index)=farFromPorts, T, '1f') :-
	holdsAtProcessedSimpleFluent_Delays(Index, withinArea(Index,nearPorts)=true, T).

     
initiatedAt_Delay(anchor(Index)=nearPort, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, velocity(Index), [T,Speed|_]),
	\+ happensAtProcessedStaticIE_Delays(Index, gap_start(Index), [T]),
	holdsAtProcessedSimpleFluent(Index, withinArea(Index,nearPorts)=true, T),
	Speed<0.2.


initiatedAt_Delay(anchor(Index)=nearPort, T, '1f') :-
	holdsForDelays(Index, withinArea(Index,nearPorts)=true, L),
	happensAtProcessedStaticIE_Qi_notDelays(Index, velocity(Index), [T,Speed|_]),
	\+ happensAtProcessedStaticIE_Qi_notDelays(Index, gap_start(Index), [T]),
	tinIntervals(T, L),
	Speed<0.2.


initiatedAt_Delay(anchor(Index)=farFromPorts, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, velocity(Index), [T,Speed|_]),
	\+ happensAtProcessedStaticIE_Delays(Index, gap_start(Index), [T]),
	\+ holdsAtProcessedSimpleFluent(Index, withinArea(Index,nearPorts)=true, T),
	Speed<0.2.

initiatedAt_Delay(anchor(Index)=farFromPorts, T, '1f') :-
	holdsForRetractions(Index, withinArea(Index,nearPorts)=true, L),
	happensAtProcessedStaticIE_Qi_notDelays(Index, velocity(Index), [T,Speed|_]),
	\+ happensAtProcessedStaticIE_Qi_notDelays(Index, gap_start(Index), [T]),
	tinIntervals(T, L),
	Speed<0.2.


terminatedAt_Retract(anchor(Index)=nearPort, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, velocity(Index), [T|_]).
	

terminatedAt_Retract(anchor(Index)=nearPort, T, '1f') :-
	holdsAtProcessedSimpleFluent_Delays(Index, withinArea(Index,nearPorts)=true, T).


terminatedAt_Retract(anchor(Index)=farFromPorts, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, velocity(Index), [T|_]).

terminatedAt_Retract(anchor(Index)=farFromPorts, T, '1f') :-
	holdsAtProcessedSimpleFluent_Retractions(Index, withinArea(Index,nearPorts)=true, T).

    
%terminatedAt_Retract(anchor(Index)=_, T) :-
%     happensAtProcessedSimpleFluent_Retractions(Index, start(gap(Index)=_), T).


terminatedAt_Delay(anchor(Index)=nearPort, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, velocity(Index), [T|_]),
	\+ holdsAtProcessedSimpleFluent(Index, withinArea(Index,nearPorts)=true, T).

terminatedAt_Delay(anchor(Index)=nearPort, T, '1f') :-
	holdsForRetractions(Index, withinArea(Index,nearPorts)=true, L),
	happensAtProcessedStaticIE_Qi_notDelays(Index, velocity(Index), [T|_]),
	tinIntervals(T, L).
	

terminatedAt_Delay(anchor(Index)=farFromPorts, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, velocity(Index), [T|_]),
	holdsAtProcessedSimpleFluent(Index, withinArea(Index,nearPorts)=true, T).

terminatedAt_Delay(anchor(Index)=farFromPorts, T, '1f') :-
	holdsForDelays(Index, withinArea(Index,nearPorts)=true, L),
	happensAtProcessedStaticIE_Qi_notDelays(Index, velocity(Index), [T|_]),
	tinIntervals(T, L).
	
terminatedAt_Delay(anchor(Index)=_, T, '2e') :-
	happensAtProcessedStaticIE_Delays(Index, velocity(Index), [T,Speed|_]),
	Speed>0.2.
     

%%%%On time

initiatedAt_OnTime(anchor(Index)=nearPort, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, velocity(Index), [T,Speed|_]),
	\+ happensAtProcessedStaticIE_OnTime(Index, gap_start(Index), [T]),
	holdsAtProcessedSimpleFluent_OnTime(Index, withinArea(Index,nearPorts)=true, T),
	Speed<0.2.

initiatedAt_OnTime(anchor(Index)=farFromPorts, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, velocity(Index), [T,Speed|_]),
	\+ happensAtProcessedStaticIE_OnTime(Index, gap_start(Index), [T]),
	\+ holdsAtProcessedSimpleFluent_OnTime(Index, withinArea(Index,nearPorts)=true, T),
	Speed<0.2.

terminatedAt_OnTime(anchor(Index)=nearPort, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, velocity(Index), [T|_]),
	\+ holdsAtProcessedSimpleFluent_OnTime(Index, withinArea(Index,nearPorts)=true, T).
     
terminatedAt_OnTime(anchor(Index)=farFromPorts, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, velocity(Index), [T|_]),
	holdsAtProcessedSimpleFluent_OnTime(Index, withinArea(Index,nearPorts)=true, T).


terminatedAt_OnTime(anchor(Index)=_, T, 'b') :-
	happensAtProcessedStaticIE_OnTime(Index, velocity(Index), [T,Speed|_]),
	Speed>0.2.

%terminatedAt_OnTime(anchor(Index)=_, T) :-
%     happensAtProcessedSimpleFluent(Index, start(gap(Index)=_), T).


%%%%%%% Termination all over points

terminatedAt(anchor(Index)=nearPort, T, 'a') :-
	happensAtProcessedStaticIE_Over(Index, velocity(Index), [T|_]),
	\+ holdsAtProcessedSimpleFluent(Index, withinArea(Index,nearPorts)=true, T).

terminatedAt(anchor(Index)=farFromPorts, T, 'a') :-
	happensAtProcessedStaticIE_Over(Index, velocity(Index), [T|_]),
	holdsAtProcessedSimpleFluent(Index, withinArea(Index,nearPorts)=true, T).

terminatedAt(anchor(Index)=_, T, 'b') :-
	happensAtProcessedStaticIE_Over(Index, velocity(Index), [T,Speed|_]),
	Speed>0.2.
     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRAWLSPEED %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     

initiatedAt_Retract(trawlSpeed(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, velocity(Index), [T|_]).

initiatedAt_Retract(trawlSpeed(Index)=true, T, '1f') :-
	holdsAtProcessedSimpleFluent_Delays(Index, withinArea(Index,nearPorts)=true, T).


initiatedAt_Delay(trawlSpeed(Index)=true, T, '1e') :-
	vesselStaticInfo(Index,fishing,_),
	happensAtProcessedStaticIE_Delays(Index, velocity(Index), [T,Speed|_]),
	\+ happensAtProcessedStaticIE_Delays(Index, gap_start(Index), [T]),
	\+ holdsAtProcessedSimpleFluent(Index, withinArea(Index,nearPorts)=true, T),
	Speed<9.0,
	Speed>1.0.

initiatedAt_Delay(trawlSpeed(Index)=true, T, '1f') :-
	vesselStaticInfo(Index,fishing,_),
	holdsForRetractions(Index, withinArea(Index,nearPorts)=true, L),
	happensAtProcessedStaticIE_Qi_notDelays(Index, velocity(Index), [T,Speed|_]),
	\+ happensAtProcessedStaticIE_Qi_notDelays(Index, gap_start(Index), [T]),
	tinIntervals(T, L),
	Speed<9.0,
	Speed>1.0.


terminatedAt_Retract(trawlSpeed(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, velocity(Index), [T|_]).

terminatedAt_Retract(trawlSpeed(Index)=true, T, '1f') :-
	holdsAtProcessedSimpleFluent_Retractions(Index, withinArea(Index,nearPorts)=true, T).

     
%terminatedAt_Retract(trawlSpeed(Index)=true, T) :-
%     happensAtProcessedSimpleFluent_Retractions(Index, start(gap(Index)=_), T).

terminatedAt_Delay(trawlSpeed(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, velocity(Index), [T,Speed|_]),
	Speed>9.0.

terminatedAt_Delay(trawlSpeed(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, velocity(Index), [T,Speed|_]),
	Speed<1.0.

terminatedAt_Delay(trawlSpeed(Index)=true, T, '2e') :-
	happensAtProcessedStaticIE_Delays(Index, velocity(Index), [T|_]),
	holdsAtProcessedSimpleFluent(Index, withinArea(Index,nearPorts)=true, T).

terminatedAt_Delay(trawlSpeed(Index)=true, T, '2f') :-
	holdsForDelays(Index, withinArea(Index,nearPorts)=true, L),
	happensAtProcessedStaticIE_Qi_notDelays(Index, velocity(Index), [T|_]),
	tinIntervals(T, L).
	     

%%%%On time

initiatedAt_OnTime(trawlSpeed(Index)=true, T, 'a') :-
	vesselStaticInfo(Index,fishing,_),
	happensAtProcessedStaticIE_OnTime(Index, velocity(Index), [T,Speed|_]),
	\+ happensAtProcessedStaticIE_OnTime(Index, gap_start(Index), [T]),
	\+ holdsAtProcessedSimpleFluent_OnTime(Index, withinArea(Index,nearPorts)=true, T),
	Speed<9.0,
	Speed>1.0.

terminatedAt_OnTime(trawlSpeed(Index)=true, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, velocity(Index), [T,Speed|_]),
	Speed>9.0.

terminatedAt_OnTime(trawlSpeed(Index)=true, T, 'b') :-
	happensAtProcessedStaticIE_OnTime(Index, velocity(Index), [T,Speed|_]),
	Speed<1.0.

%terminatedAt_OnTime(trawlSpeed(Index)=true, T) :-
%     happensAtProcessedSimpleFluent(Index, start(gap(Index)=_), T).

terminatedAt_OnTime(trawlSpeed(Index)=true, T, 'c') :-
	happensAtProcessedStaticIE_OnTime(Index, velocity(Index), [T|_]),
	holdsAtProcessedSimpleFluent_OnTime(Index, withinArea(Index,nearPorts)=true, T).


%%%%%%% Termination all over points

terminatedAt(trawlSpeed(Index)=true, T, 'a') :-
	happensAtProcessedStaticIE_Over(Index, velocity(Index), [T,Speed|_]),
	Speed>9.0.

terminatedAt(trawlSpeed(Index)=true, T, 'b') :-
	happensAtProcessedStaticIE_Over(Index, velocity(Index), [T,Speed|_]),
	Speed<1.0.

terminatedAt(trawlSpeed(Index)=true, T, 'c') :-
	happensAtProcessedStaticIE_Over(Index, velocity(Index), [T|_]),
	holdsAtProcessedSimpleFluent(Index, withinArea(Index,nearPorts)=true, T).
     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRAVELSPEED %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     
initiatedAt_Retract(travelSpeed(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, velocity(Index), [T|_]).

initiatedAt_Retract(travelSpeed(Index)=true, T, '1e2') :-
	happensAtProcessedStaticIE_Delays(Index, gap_start(Index), [T]).


initiatedAt_Delay(travelSpeed(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, velocity(Index), [T,Speed|_]),
	\+ happensAtProcessedStaticIE_Delays(Index, gap_start(Index), [T]),
	vesselStaticInfo(Index,Type,_),
	typeSpeed(Type,Min,Max,_),
	Speed > Min,
	Speed < Max.
     

terminatedAt_Retract(travelSpeed(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, velocity(Index), [T|_]).

%terminatedAt_Retract(travelSpeed(Index)=true, T) :-
%     happensAtProcessedSimpleFluent_Retractions(Index, start(gap(Index)=_), T).

terminatedAt_Delay(travelSpeed(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, velocity(Index), [T,Speed|_]),
	vesselStaticInfo(Index,Type,_),
	typeSpeed(Type,Min,Max,_),
	(Speed<Min ; Speed>Max).


%%%%On time

initiatedAt_OnTime(travelSpeed(Index)=true, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, velocity(Index), [T,Speed|_]),
	\+ happensAtProcessedStaticIE_OnTime(Index, gap_start(Index), [T]),
	vesselStaticInfo(Index,Type,_),
	typeSpeed(Type,Min,Max,_),
	Speed > Min,
	Speed < Max.

terminatedAt_OnTime(travelSpeed(Index)=true, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, velocity(Index), [T,Speed|_]),
	vesselStaticInfo(Index,Type,_),
	typeSpeed(Type,Min,Max,_),
	(Speed<Min ; Speed>Max).

%terminatedAt_OnTime(travelSpeed(Index)=true, T) :-
%     happensAtProcessedSimpleFluent(Index, start(gap(Index)=_), T).


%%%%%%% Termination all over points

terminatedAt(travelSpeed(Index)=true, T, 'a') :-
	happensAtProcessedStaticIE_Over(Index, velocity(Index), [T,Speed|_]),
	vesselStaticInfo(Index,Type,_),
	typeSpeed(Type,Min,Max,_),
	(Speed<Min ; Speed>Max).
	
     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% speedLessThanMin %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     
initiatedAt_Retract(speedLessThanMin(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, velocity(Index), [T|_]).

initiatedAt_Retract(speedLessThanMin(Index)=true, T, '1e2') :-
	happensAtProcessedStaticIE_Delays(Index, gap_start(Index), [T]).

     
initiatedAt_Delay(speedLessThanMin(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, velocity(Index), [T,Speed|_]),
	\+ happensAtProcessedStaticIE_Delays(Index, gap_start(Index), [T]),
	vesselStaticInfo(Index,Type,_),
	typeSpeed(Type,Min,_,_),
	Speed < Min.


terminatedAt_Retract(speedLessThanMin(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, velocity(Index), [T|_]).

%terminatedAt_Retract(speedLessThanMin(Index)=true, T) :-
%     happensAtProcessedSimpleFluent_Retractions(Index, start(gap(Index)=_), T).
     
terminatedAt_Delay(speedLessThanMin(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, velocity(Index), [T,Speed|_]),
	vesselStaticInfo(Index,Type,_),
	typeSpeed(Type,Min,_,_),
	Speed>=Min.
     

%%%%On time

initiatedAt_OnTime(speedLessThanMin(Index)=true, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, velocity(Index), [T,Speed|_]),
	\+ happensAtProcessedStaticIE_OnTime(Index, gap_start(Index), [T]),
	vesselStaticInfo(Index,Type,_),
	typeSpeed(Type,Min,_,_),
	Speed < Min.

terminatedAt_OnTime(speedLessThanMin(Index)=true, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, velocity(Index), [T,Speed|_]),
	vesselStaticInfo(Index,Type,_),
	typeSpeed(Type,Min,_,_),
	Speed>=Min.

%terminatedAt_OnTime(speedLessThanMin(Index)=true, T) :-
%     happensAtProcessedSimpleFluent(Index, start(gap(Index)=_), T).


%%%%%%% Termination all over points

terminatedAt(speedLessThanMin(Index)=true, T, 'a') :-
	happensAtProcessedStaticIE_Over(Index, velocity(Index), [T,Speed|_]),
	vesselStaticInfo(Index,Type,_),
	typeSpeed(Type,Min,_,_),
	Speed>=Min.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% speedGrThanMax %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initiatedAt_Retract(speedGrThanMax(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, velocity(Index), [T|_]).

initiatedAt_Retract(speedGrThanMax(Index)=true, T, '1e2') :-
	happensAtProcessedStaticIE_Delays(Index, gap_start(Index), [T]).

     
initiatedAt_Delay(speedGrThanMax(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, velocity(Index), [T,Speed|_]),
	\+ happensAtProcessedStaticIE_Delays(Index, gap_start(Index), [T]),
	vesselStaticInfo(Index,Type,_),
	typeSpeed(Type,_,Max,_),
	Speed > Max.


terminatedAt_Retract(speedGrThanMax(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Retractions(Index, velocity(Index), [T|_]).

%terminatedAt_Retract(speedGrThanMax(Index)=true, T) :-
%     happensAtProcessedSimpleFluent_Retractions(Index, start(gap(Index)=_), T).
     
terminatedAt_Delay(speedGrThanMax(Index)=true, T, '1e') :-
	happensAtProcessedStaticIE_Delays(Index, velocity(Index), [T,Speed|_]),
	vesselStaticInfo(Index,Type,_),
	typeSpeed(Type,_,Max,_),
	Speed=<Max.
     

%%%%On time

initiatedAt_OnTime(speedGrThanMax(Index)=true, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, velocity(Index), [T,Speed|_]),
	\+ happensAtProcessedStaticIE_OnTime(Index, gap_start(Index), [T]),
	vesselStaticInfo(Index,Type,_),
	typeSpeed(Type,_,Max,_),
	Speed > Max.

terminatedAt_OnTime(speedGrThanMax(Index)=true, T, 'a') :-
	happensAtProcessedStaticIE_OnTime(Index, velocity(Index), [T,Speed|_]),
	vesselStaticInfo(Index,Type,_),
	typeSpeed(Type,_,Max,_),
	Speed=<Max.

%terminatedAt_OnTime(speedGrThanMax(Index)=true, T) :-
%     happensAtProcessedSimpleFluent(Index, start(gap(Index)=_), T).


%%%%%%% Termination all over points

terminatedAt(speedGrThanMax(Index)=true, T, 'a') :-
	happensAtProcessedStaticIE_Over(Index, velocity(Index), [T,Speed|_]),
	vesselStaticInfo(Index,Type,_),
	typeSpeed(Type,_,Max,_),
	Speed=<Max.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% AGROUND %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

holdsForSDFluentInc(aground(Index)=nearPort, _, I, OnTime) :-
	holdsForProcessedSimpleFluentInc(Index, beforeaground(Index)=nearPort, Iq, _, Io, _),
	intDurGreater(Iq, Io, 3600, I, OnTime).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% AtANCHOR %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

holdsForSDFluentInc2(atAnchor_1(Index)=nearPort, Ip, _, Iq, OnTime) :-
	holdsForProcessedSimpleFluentInc(Index, anchor(Index)=nearPort, Iq1, Id1, Io1, Ir1),
	holdsForProcessedSDFluentInc(Index, aground(Index)=nearPort, Iq2, Id2, Io2, Ir2),
	incremental_relative_complement_all([Iq1,Iq2], [Id1,Id2], [Io1,Io2], [Ir1,Ir2], Ip, Iq, OnTime).
	

holdsForSDFluentInc2(atAnchor(Index)=nearPort, _, Dp, I, OnTime) :-
	getIntervals2(atAnchor_1(Index)=nearPort, Dp, Iq, _, Io, _),
	intDurGreater(Iq, Io, 1800, I, OnTime).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% rendezVous %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

holdsForSDFluentInc2(rendezVous_1(Index,Index2)=true, Ip, _, Iq, OnTime) :-
	holdsForProcessedSimpleFluentInc(Index, lowSpeed(Index)=true, Iq1, Id1, Io1, Ir1),
	holdsForProcessedSimpleFluentInc(Index, stopped(Index)=farFromPorts, Iq2, Id2, Io2, Ir2),
	(recorded(vesselPairNew, (Index,Index2), _) -> incremental_union_all_alt([Iq1,Iq2], [Io1,Io2], Iq, OnTime)
	; incremental_union_all([Iq1,Iq2], [Id1,Id2], [Io1,Io2], [Ir1,Ir2], Ip, Iq, OnTime)
	).

holdsForSDFluentInc2(rendezVous_2(Index,Index2)=true, Ip, _, Iq, OnTime) :-
	holdsForProcessedSimpleFluentInc(Index2, lowSpeed(Index2)=true, Iq1, Id1, Io1, Ir1),
	holdsForProcessedSimpleFluentInc(Index2, stopped(Index2)=farFromPorts, Iq2, Id2, Io2, Ir2),
	(recorded(vesselPairNew, (Index,Index2), _) -> incremental_union_all_alt([Iq1,Iq2], [Io1,Io2], Iq, OnTime)
	; incremental_union_all([Iq1,Iq2], [Id1,Id2], [Io1,Io2], [Ir1,Ir2], Ip, Iq, OnTime)
	).

holdsForSDFluentInc2(rendezVous_3(Index,Index2)=true, Ip, Dp, Iq, OnTime) :-
	getIntervals2(rendezVous_1(Index,Index2)=true, Dp, Iq1, Id1, Io1, Ir1),
	getIntervals2(rendezVous_2(Index, Index2)=true, Dp, Iq2, Id2, Io2, Ir2),
	(recorded(vesselPairNew, (Index,Index2), _) ->
		incremental_intersect_all_alt([Iq1,Iq2], [Io1,Io2], Iq, OnTime)
		;
		incremental_intersect_all([Iq1,Iq2], [Id1,Id2], [Io1,Io2], [Ir1,Ir2], Ip, Iq, OnTime)
	).

holdsForSDFluentInc2(rendezVous_4(Index,Index2)=true, Ip, Dp, Iq, OnTime) :-
	holdsForProcessedIEInc(Index, proximity(Index,Index2)=true, Iq1, Id1, Io1, Ir1),
	getIntervals2(rendezVous_3(Index,Index2)=true, Dp, Iq2, Id2, Io2, Ir2),
	(recorded(vesselPairNew, (Index,Index2), _) ->
		incremental_intersect_all_alt([Iq1,Iq2], [Io1,Io2], Iq, OnTime)
		;
		incremental_intersect_all([Iq1,Iq2], [Id1,Id2], [Io1,Io2], [Ir1,Ir2], Ip, Iq, OnTime)
	).

holdsForSDFluentInc2(rendezVous(Index,Index2)=true, _, Dp, I, OnTime) :-
	getIntervals2(rendezVous_4(Index,Index2)=true, Dp, Iq, _, Io, _),
	intDurGreater(Iq, Io, 240, I, OnTime).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRAWLING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

holdsForSDFluentInc2(trawling_1(Index)=true, Ip, _, Iq, OnTime) :-
	holdsForProcessedSimpleFluentInc(Index, trawlSpeed(Index)=true, Iq1, Id1, Io1, Ir1),
	vesselStaticInfo(Index,fishing,_),
	holdsForProcessedSimpleFluentInc(Index, withinArea(Index,fishing)=true, Iq2, Id2, Io2, Ir2),
	incremental_intersect_all([Iq1,Iq2], [Id1,Id2], [Io1,Io2], [Ir1,Ir2], Ip, Iq, OnTime).

holdsForSDFluentInc2(trawling(Index)=true, _, Dp, I, OnTime) :-
	getIntervals2(trawling_1(Index)=true, Dp, Iq, _, Io, _),
	intDurGreater(Iq, Io, 3600, I, OnTime).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% UNUSUALSPEED %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

holdsForSDFluentInc2(unusualSpeed_1(Index)=true, Ip, _, Iq, OnTime) :-
	holdsForProcessedSimpleFluentInc(Index, underWay(Index)=true, Iq1, Id1, Io1, Ir1),
	holdsForProcessedSimpleFluentInc(Index, lowSpeed(Index)=true, Iq2, Id2, Io2, Ir2),
	incremental_union_all([Iq1,Iq2], [Id1,Id2], [Io1,Io2], [Ir1,Ir2], Ip, Iq, OnTime).
     
holdsForSDFluentInc2(unusualSpeed(Index)=true, Ip, Dp, Iq, OnTime) :-
	getIntervals2(unusualSpeed_1(Index)=true, Dp, Iq1, Id1, Io1, Ir1),
	holdsForProcessedSimpleFluentInc(Index, travelSpeed(Index)=true, Iq2, Id2, Io2, Ir2),
	incremental_relative_complement_all([Iq1,Iq2], [Id1,Id2], [Io1,Io2], [Ir1,Ir2], Ip, Iq, OnTime).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cachingOrder2(Index, withinArea(Index,AreaType)=true) :-
     recorded(vessel, Index, _), areaType(AreaType).

cachingOrder2(Index, gap(Index)=nearPort) :-
     recorded(vessel, Index, _).

cachingOrder2(Index, gap(Index)=farFromPorts) :-
     recorded(vessel, Index, _).

cachingOrder2(Index, stopped(Index)=nearPort) :-
     recorded(vessel, Index, _).

cachingOrder2(Index, stopped(Index)=farFromPorts) :-
     recorded(vessel, Index, _).

cachingOrder2(Index, lowSpeed(Index)=true) :-
     recorded(vessel, Index, _).

cachingOrder2(Index, underWay(Index)=true) :-
     recorded(vessel, Index, _).

cachingOrder2(Index, adrift(Index)=true) :-
     recorded(vessel, Index, _).

cachingOrder2(Index, beforeaground(Index)=nearPort) :-
     recorded(vessel, Index, _).

cachingOrder2(Index, beforeaground(Index)=farFromPorts) :-
     recorded(vessel, Index, _).

cachingOrder2(Index, aground(Index)=nearPort) :-
     recorded(vessel, Index, _).

cachingOrder2(Index, aground(Index)=farFromPorts) :-
     recorded(vessel, Index, _).

cachingOrder2(Index, anchor(Index)=nearPort) :-
     recorded(vessel, Index, _).

cachingOrder2(Index, anchor(Index)=farFromPorts) :-
     recorded(vessel, Index, _).

cachingOrder2(Index, atAnchor(Index)=nearPort) :-
     recorded(vessel, Index, _).

cachingOrder2(Index, atAnchor(Index)=farFromPorts) :-
     recorded(vessel, Index, _).

cachingOrder2(Index, trawlSpeed(Index)=true) :-
     recorded(vessel, Index, _).

cachingOrder2(Index, travelSpeed(Index)=true) :-
     recorded(vessel, Index, _).

cachingOrder2(Index, speedLessThanMin(Index)=true) :-
     recorded(vessel, Index, _).

cachingOrder2(Index, speedGrThanMax(Index)=true) :-
     recorded(vessel, Index, _).

cachingOrder2(Index, rendezVous(Index,Index2)=true) :-
     recorded(vesselPair, (Index,Index2), _).

cachingOrder2(Index, trawling(Index)=true) :-
     recorded(vessel, Index, _).

cachingOrder2(Index, unusualSpeed(Index)=true) :-
     recorded(vessel, Index, _).

collectIntervals2(Index, proximity(Index,Index2)=true) :-
     recorded(vesselPair, (Index,Index2), _).
     %term_hash(proximity(Index,Index2), Pindex).

areaType(anchorage).
areaType(fishing).
areaType(natura).
areaType(nearCoast).
areaType(nearCoast5k).
areaType(nearPorts).
