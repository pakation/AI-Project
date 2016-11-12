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
	eligir_explicacion(Creencias, Explicaciones, Eligido),
	nl,
	write(Eligido).

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

% test([], Choice_New, Choice_New, Min_New, Min_New).
% test([Key => Number|T], Choice, Choice_New, Min, Min_New) :-
% 	(Number @< Min),
% 	test(T, Key, Choice_New, Number, Min_New)
% 	;
% 	test(T, Choice, Choice_New, Min, Min_New).

%
eligir_explicacion(Creencias, Explicaciones, Eligido) :-
	do_eligir_explicacion(Creencias, Explicaciones, _, Eligido, 999999, _).

do_eligir_explicacion(Creencias, [], Eligido_New, Eligido_New, Min_New, Min_New).
do_eligir_explicacion(Creencias, [Explicacion|T], Eligido, Eligido_New, Min, Min_New) :-
	% Look for shelf count discrepancies
	delta_count(Creencias, Explicacion, Result),
	(Result @< Min),
	% If this solution has less of a discrepancy
	do_eligir_explicacion(Creencias, T, Explicacion, Eligido_New, Result, Min_New)
	;
	% If this solution has more of a discrepancy
	do_eligir_explicacion(Creencias, T, Eligido, Eligido_New, Min, Min_New).

% Delta count
%
% TODO prove that this is is OK for all cases
%
% Take creencias. For every item in the explanation
update_shelf(Shelf, Amount, [], [Shelf => Amount]).
update_shelf(Shelf, Amount, [Shelf => Count|T], [Shelf => Count_New|T]) :- Count_New is Count + Amount.
update_shelf(Shelf, Amount, [H|T], [H|T_New]) :- update_shelf(Shelf, Amount, T, T_New).

delta_count(Creencias, Explicaciones, Total) :- 
	do_delta_count(Creencias, Explicaciones, [], Shelves),
	do_delta_total(Shelves, 0, Total).

do_delta_total([], Total_New, Total_New).
do_delta_total([Shelf => Count|T], Total_A, Total_New) :-
	Total_B is Total_A + abs(Count),
	do_delta_total(T, Total_B, Total_New).

% Creencias, explicaciones, temp, temp, Output
do_delta_count(Creencias, [], Shelves_New, Shelves_New).
do_delta_count(Creencias, [Item => Shelf|T], Shelves_A, Shelves_New) :-
	% -1 for the shelf that we thought the item was on
	is_on_shelf(Item, Shelf_Old, Creencias),
	update_shelf(Shelf_Old, -1, Shelves_A, Shelves_B),
	% +1 for the shelf that the item is really on
	update_shelf(Shelf, 1, Shelves_B, Shelves_C),
	do_delta_count(Creencias, T, Shelves_C, Shelves_New).

