% :- ensure_loaded(proyecto1).

% TODO deal with observations where shelf is totally empty
% TODO handle cases where there are no contradicions

:- op(800,xfx,'=>').

% -- initial diagnosis
% take all objects that the shopkeeper mentioned
% for each object, pair it with where it's supposed to be
% then generate mover,colocar pairs for each
% then merge colocars
%
% -- normal diagnosis
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

% returns true if the item has been seen on a different shelf, or the item
% has never been seen but the shelf that the item should be on has been,
% false otherwise.
contradicts_obs(Item, Shelf, Obs) :- do_contradicts_obs(Item, Shelf, Obs, false, _).
do_contradicts_obs(_, _, [], true, _). % could't find item, but has seen shelf
do_contradicts_obs(Item, Shelf, [Item => Shelf|_], _, _) :- !, false. % stop
do_contradicts_obs(Item, _, [Item => _|_], _, _). % true, stop
do_contradicts_obs(Item, Shelf, [_ => Shelf|T], _, Seen_Shelf_New) :- do_contradicts_obs(Item, Shelf, T, true, Seen_Shelf_New).
do_contradicts_obs(Item, Shelf, [_|T], Seen_Shelf, Seen_Shelf_New) :- do_contradicts_obs(Item, Shelf, T, Seen_Shelf, Seen_Shelf_New).

% Creencias, creencias nuevos, observaciones, acciones
diagnosis(Creencias, Observaciones, Creencias_New, []) :- 
	remove_inconsistencies(Creencias, Observaciones, [], Creencias_Malos, [], Creencias_Limpios),
	generar_posibilidades(Creencias_Malos, Observaciones, [s1,s2,s3], Posibilidades),
	generar_explicaciones(Posibilidades, [], [], Explicaciones),
	eligir_explicacion(Creencias, Explicaciones, Eligido),
	nl,
	write(Eligido).

% Strip out bad creencias

% Unit tests: 
%	remove_inconsistencies([a => b], [], [], A, [], B), A=[], B=[a=>b]
%	remove_inconsistencies([a => b], [a => c], [], A, [], B), A=[a], B=[]
remove_inconsistencies([], _, Creencias_Malos, Creencias_Malos, Creencias_New, Creencias_New).
% for each item
remove_inconsistencies([Item => Shelf|T], Obvs, Creencias_Malos_A, Creencias_Malos, Creencias_A, Creencias_New) :-
	% if the observations do not disagree with where the item is supposed to be
	contradicts_obs(Item, Shelf, Obvs),
	% a contradiction exists
	append(Creencias_Malos_A, [Item], Creencias_Malos_B),
	% recurse without adding to the good list
	remove_inconsistencies(T, Obvs, Creencias_Malos_B, Creencias_Malos, Creencias_A, Creencias_New)
	; 
	% add to the ok/vetted list
	append(Creencias_A, [Item => Shelf], Creencias_B),
	% recurse
	remove_inconsistencies(T, Obvs, Creencias_Malos_A, Creencias_Malos, Creencias_B, Creencias_New)
	.

% true if the robot has seen the given shelf, false otherwise
has_seen_shelf(Shelf, [_ => Shelf|_]).
has_seen_shelf(Shelf, [_|T]) :- has_seen_shelf(Shelf, T).

% stricter than is_consistent
is_on_shelf(Item, Shelf, [Item => Shelf|_]).
is_on_shelf(Item, Shelf, [_|T]) :- is_on_shelf(Item, Shelf, T).

% For each item, generate a set of possible shelves that the item could 
% possibly be
generar_posibilidades([], _, _, []).
generar_posibilidades([Item|T], Obvs, Shelves, [Set|Set_T]) :-
	is_on_shelf(Item, Shelf, Obvs), % if it's on the shelf we have a perfect correction
	Set = [Item => Shelf],
	generar_posibilidades(T, Obvs, Shelves, Set_T)
	; 
	do_generar_posibilidades(Item, Shelves, Obvs, [], Set), % if not, generate all possible corrections
	generar_posibilidades(T, Obvs, Shelves, Set_T).

% Expand explanations, if they do not conflict with observations
% Item, [Shelves], [Observations], [], >Explanations)
do_generar_posibilidades(_, [], _, Expl_New, Expl_New).
do_generar_posibilidades(Item, [Shelf|T], Obvs, Expl_A, Expl_New) :-
	has_seen_shelf(Shelf, Obvs),
	% if the robot has already seen this shelf then if the item is on it,
	% is_on_shelf called by parent should be true. otherwise, the item can't
	% possibly be on this shelf so do not add.
	do_generar_posibilidades(Item, T, Obvs, Expl_A, Expl_New)
	;
	% if the robot has not seen this shelf the item may possibly be on it.
	append(Expl_A, [Item => Shelf], Expl_B),
	do_generar_posibilidades(Item, T, Obvs, Expl_B, Expl_New)
	.

generar_explicaciones([[]], Cadena, Results_New, Results_New).
generar_explicaciones([], Cadena, Results_A, Results_New) :-
	append(Results_A, [Cadena], Results_New).
generar_explicaciones([[]|T], Cadena, Results_New, Results_New).
generar_explicaciones([[Item => Shelf|T]|T_2], Cadena_A, Results_A, Results_New) :-
	append(Cadena_A, [Item => Shelf], Cadena_B),
	generar_explicaciones(T_2, Cadena_B, Results_A, Results_B),
	generar_explicaciones([T|T_2], Cadena_A, Results_B, Results_New).

% test([], Choice_New, Choice_New, Min_New, Min_New).
% test([Key => Number|T], Choice, Choice_New, Min, Min_New) :-
% 	(Number @< Min),
% 	test(T, Key, Choice_New, Number, Min_New)
% 	;
% 	test(T, Choice, Choice_New, Min, Min_New).

%
eligir_explicacion(_, [Item => Shelf], [Item => Shelf]). % Explanation with only one option, always choose that option
eligir_explicacion(Creencias, Explicaciones, Eligido) :-
	do_eligir_explicacion(Creencias, Explicaciones, _, Eligido, 999999, _).

do_eligir_explicacion(_, [], Eligido_New, Eligido_New, Min_New, Min_New).
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
do_delta_total([_ => Count|T], Total_A, Total_New) :-
	Total_B is Total_A + abs(Count),
	do_delta_total(T, Total_B, Total_New).

% Creencias, explicaciones, temp, temp, Output
do_delta_count(_, [], Shelves_New, Shelves_New).
do_delta_count(Creencias, [Item => Shelf|T], Shelves_A, Shelves_New) :-
	% -1 for the shelf that we thought the item was on
	is_on_shelf(Item, Shelf_Old, Creencias),
	update_shelf(Shelf_Old, -1, Shelves_A, Shelves_B),
	% +1 for the shelf that the item is really on
	update_shelf(Shelf, 1, Shelves_B, Shelves_C),
	do_delta_count(Creencias, T, Shelves_C, Shelves_New).

