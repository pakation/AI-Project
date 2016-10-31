
% TODO include proyecto1.pl

% Diagnóstico
%
% Game plan
%	1. For each observed object, generate a (mover, colocar) tuple that would
%		explain how the object got there. For example, if o1 is in
%		loc1 then generate "mover(loc1,loc2),colocar(o1)". If a tuple with
%		destination loc2 was already generated then it should become
%		"mover(...,loc2),colocar(o1),colocar(o2)"
%	2. For any objects unaccounted for, note that if the robot moves to loc1
%		the robot will see all objects at loc1. For that reason, the
%		unaccounted objects must either be at the location(s) that the worker
%		told the robot that they would be. If this information can be proven 
%		to be inaccurrate then o1 must be in one of the locations that the 
%		robot has or is out of stock. By default assume the worker is correct,
%		that is, if it is not mentioned then it is out of stock and we don't
%		need to represent any worker actions in the diagnosis for this item.
%	3. Given the above, we should now have a list of actions for all items
%		explaining how any item that has been accounted for reached that
%		destination. For items that have not been accounted for, there may
%		be multiple posibilities. We now have to generate a list of all
%		posible permutations that may explain where that item may be. 
%
%		For example if the worker says that o1 is in l1 and we visit l1 and o1 
%		is not there, we must generate all of the following
%			[...,"mover(locx,loc2),colocar(o1)"],
%			[...,"mover(locx,loc3),colocar(o1)"],
%							...
%			[...,"mover(locx,locn),colocar(o1)"]
%	4. Forard this set of possible scenarios on to the decision taking module

% Diagnóstico
% Planeación
