
% TODO include proyecto1.pl

% [ Diagnóstico ]
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
%	4. Choose one of these scenarios
%	5. Update the knowledge base to correspond with this scenario
%
% [ Toma de decición ]
%
% Game plan
%	1. For each object o1 that the customer requested, generate "entregar(o1)".
%	2. For every object o1 that is at the robot's current position and 
%		shouldn't be, generate "reacomodar(o1)".
%	3. Forward these decisions to the planning module
%
% [ Planeación ]
%
% The planning phase is greatly affected if the robot has one hand vs two
% hands - which is correct? If one, the robot can simply fulfill objects
% one by one. If two or more, the robot has to combine tasks (for example
% if the robot arrives at a shelf and o1 the customer wants and o2 is out of
% place the robot needs to grab both o1 and o2, deliver o1 to customer and
% o2 to the correct place. If the robot only has one hand, he simply grabs
% o1 and delivers to customer, then goes back for o2.
%
% Game plan
%	1. Accept decisions as input
%	2. If the current location is in front of the customer, for every object
%		that the customer requested move to the corresponding shelf, generate
%		"mover(li,lj),buscar(o1),agarrar(o1)". If buscar(o1) fails interrupt
%		and go to diagnosis.
%	3. If the current location is in front of a shelf, we were previously
%		interrupted. For every object o2 on this shelf that the customer wants, 
%		"buscar(o1),agarrar(o1)". For every object o1 on this shelf that is 
%		out of place, generate "buscar(o1),agarrar(o1)". If hands are full
%		at any time generate "mover(li,lj),colocar(o1)" (lj should be customer 
%		if the customer wants this object or the correct shelf otherwise). 
%		Continue until all of the customer's requests are satisfied.
%	4. Note that for any items out of place, the robot should generate
%		"buscar(o1),agarrar(o1),mover(li,lj),colocar(o1)".
