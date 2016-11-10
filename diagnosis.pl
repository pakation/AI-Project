% :- ensure_loaded(proyecto1).

% TODO 

:- op(800,xfx,'=>').

% TODO initial diagnosis
% take all objects that the shopkeeper mentioned
% for each object, pair it with where it's supposed to be
% then generate mover,colocar pairs for each
% then merge colocars
%
% TODO normal diagnosis
% > take initial diagnosis and observations
% < return new diagnosis
%
% for each item in the diagnosis, look in observations for any contradictions
% if one exists, remove the offending entry to construct a diagnosis free of errors.
% take this entry and, checking against observations, generate all possible alternatives
%
% if no more contradictions exist, take all offending entries and generate all
% posible permutations. for each, append the "clean" diagnosis, then compare the 
% number of items per shelf to the expected.
%
% choose the permutation that is the closest to expected
% generate move,colocar pairs for this permutation

% return false if the item is observed on a different shelf, true otherwise
is_consistent(_, _, []).
is_consistent(Item, Shelf, [Item => Shelf|_]).
is_consistent(Item, _, [Item => _|_]) :- !, false.
is_consistent(Item, Shelf, [_|T]) :- is_consistent(Item, Shelf, T).

% Creencias, creencias nuevos, observaciones, acciones
diagnosis(Creencias, Observaciones, Creencias_New, []) :- 
	remove_inconsistencies(Creencias, Observaciones, [], Creencias_Malos, [], Creencias_Limpios),
	% TODO [] below needs to be a list of shelves
	generar_explicaciones(Creencias_Malos, Observaciones, [s1,s2,s3], [], Explicaciones),
	nl,
	write(Explicaciones).

% Strip out bad creencias

% Unit tests: 
%	remove_inconsistencies([a => b], [], [], A, [], B), A=[], B=[a=>b]
%	remove_inconsistencies([a => b], [a => c], [], A, [], B), A=[a], B=[]
remove_inconsistencies([], _, Creencias_Malos, Creencias_Malos, Creencias_New, Creencias_New).
remove_inconsistencies([Item => Shelf|T], Obvs, Creencias_Malos_A, Creencias_Malos, Creencias_A, Creencias_New) :-
	is_consistent(Item, Shelf, Obvs),
	append(Creencias_A, [Item => Shelf], Creencias_B),
	remove_inconsistencies(T, Obvs, Creencias_Malos_A, Creencias_Malos, Creencias_B, Creencias_New)
	; 
	append(Creencias_Malos_A, [Item], Creencias_Malos_B),
	remove_inconsistencies(T, Obvs, Creencias_Malos_B, Creencias_Malos, Creencias_A, Creencias_New)
	.

% true if the robot has seen the given shelf, false otherwise
has_seen_shelf(Shelf, [_ => Shelf|_]).
has_seen_shelf(Shelf, [_|T]) :- has_seen_shelf(Shelf, T).

% stricter than is_consistent
is_on_shelf(Item, Shelf, [Item => Shelf|_]).
is_on_shelf(Item, Shelf, [_|T]) :- is_on_shelf(Item, Shelf, T).

% Generate explanations by first checking to see if the robot has sighted
% the object, then by ruling out shelves previously seen that don't have the
% item
generar_explicaciones([], _, _, Expl_New, Expl_New).
generar_explicaciones([Item|T], Obvs, Shelves, Expl_A, Expl_New) :-
	is_on_shelf(Item, Shelf, Obvs), % if it's on the shelf we have a perfect correction
	append(Expl_A, [Item => Shelf], Expl_B),
	generar_explicaciones(T, Obvs, Shelves, Expl_B, Expl_New)
	; expandir_explicaciones(Item, Shelves, Obvs, [], Expl_New), % if not, generate all possible corrections
	generar_explicaciones(T, Obvs, Shelves, Expl_A, Expl_New).

% Expand explanations, if they do not conflict with observations
% Item, [Shelves], [Observations], [], >Explanations)
expandir_explicaciones(_, [], _, Expl_New, Expl_New).
expandir_explicaciones(Item, [Shelf|T], Obvs, Expl_A, Expl_New) :-
	has_seen_shelf(Shelf, Obvs),
	% if the robot has already seen this shelf then if the item is on it,
	% is_on_shelf called by parent should be true. otherwise, the item can't
	% possibly be on this shelf so do not add.
	expandir_explicaciones(Item, T, Obvs, Expl_A, Expl_New)
	;
	% if the robot has not seen this shelf the item may possibly be on it.
	append(Expl_A, [Item => Shelf], Expl_B),
	expandir_explicaciones(Item, T, Obvs, Expl_B, Expl_New)
	.
