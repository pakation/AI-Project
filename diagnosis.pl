% verdad si el objecto está en un estante específico
%is_on_shelf(Item, Shelf, [Item => Shelf|_]).
%is_on_shelf(Item, Shelf, [_|T]) :- is_on_shelf(Item, Shelf, T).

%
% Punto de entrada
%
%****************************************************************
% Diagnóstico
%
% Usage: diagnosis(KB, Creencias_New, Acciones)
%****************************************************************
diagnosis(KB, Creencias_New, Acciones) :-
	rels_inst(report, Creencias, KB), % cargar creencias desde el KB
	rels_inst(observations, Observaciones, KB), % cargar observaciones desde el KB
	extension_class(shelves, Estantes, KB), % cargar lista de estantes desde el KB
	do_diagnosis(Creencias, Observaciones, Estantes, Creencias_New, Acciones).

do_diagnosis(Creencias, Observaciones, Estantes, Creencias_New, Acciones) :- 
	% sacar objectos que generan contraditiones
	remove_inconsistencies(Creencias, Observaciones, [], Creencias_Malos, [], Creencias_Limpios),
	% para esos objectos, generar listas de estantes donde cada objecto podría estar
	generar_posibilidades(Creencias_Malos, Observaciones, Estantes, [], Posibilidades),
	% choose an explanation
	eligir_explicacion2(Creencias, Posibilidades, Eligido),
	% % combinar las posibilidades para generar explicaciones
	% generar_explicaciones(Posibilidades, [], [], Explicaciones),
	% % evaluar las explicaciones para eligir uno con menos error
	% eligir_explicacion(Creencias, Explicaciones, Eligido),
	% combinar las creencias limpiados con la explicación eligido
	append(Creencias_Limpios, Eligido, Creencias_B),
	% si observe objectos que no apareció antes en creencias, agregarlos ahora
	new_obsv_to_creencias(Creencias_B, Observaciones, Creencias_C),
	% borrar el objecto especial 'empty'
	remove_special_item_empty(Creencias_C, [], Creencias_New),
	% generar los acciones que el asistente hizo para hacerlo real la nueva creencia
	generate_shopkeeper_actions(Creencias_New, Estantes, almacen, [], Acciones).

% true if we have a match with empty
is_special_item_empty(empty).

% remove 'empty' from beliefs
remove_special_item_empty([], Creencias_New, Creencias_New).
remove_special_item_empty([Item => Shelf|T], Creencias_A, Creencias_New) :-
	is_special_item_empty(Item),
	remove_special_item_empty(T, Creencias_A, Creencias_New)
	;
	append(Creencias_A, [Item => Shelf], Creencias_B),
	remove_special_item_empty(T, Creencias_B, Creencias_New).

% verdad si el objecto aparece en creencias
is_item_expected(Item, [Item => _|_]).
is_item_expected(Item, [_|T]) :- is_item_expected(Item, T).

% convierte un objecto nunca visto antes a una creencia
new_obsv_to_creencias(Creencias_New, [], Creencias_New).
new_obsv_to_creencias(Creencias, [Item => Shelf|T], Creencias_New):-
	not(is_item_expected(Item, Creencias)),
	append(Creencias, [Item => Shelf], Creencias_B),
	new_obsv_to_creencias(Creencias_B, T, Creencias_New)
	;
	new_obsv_to_creencias(Creencias, T, Creencias_New).

% returns true if the item has been seen on a different shelf, or the item
% has never been seen but the shelf that the item should be on has been,
% false otherwise.
contradicts_obs(Item, Shelf, Obs) :- do_contradicts_obs(Item, Shelf, Obs, false, _).
do_contradicts_obs(_, _, [], true, _). % could't find item, but has seen shelf
do_contradicts_obs(Item, Shelf, [Item => Shelf|_], _, _) :- !, false. % stop
do_contradicts_obs(Item, _, [Item => _|_], _, _). % true, stop
do_contradicts_obs(Item, Shelf, [_ => Shelf|T], _, Seen_Shelf_New) :- do_contradicts_obs(Item, Shelf, T, true, Seen_Shelf_New).
do_contradicts_obs(Item, Shelf, [_|T], Seen_Shelf, Seen_Shelf_New) :- do_contradicts_obs(Item, Shelf, T, Seen_Shelf, Seen_Shelf_New).

% Strip out bad creencias
remove_inconsistencies([], _, Creencias_Malos, Creencias_Malos, Creencias_New, Creencias_New).
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

not_empty([_]).
not_empty([_|_]).

% For each item, generate a set of possible shelves that the item could 
% possibly be
generar_posibilidades([], _, _, Set_New, Set_New).
generar_posibilidades([Item|T], Obvs, Shelves, Set_A, Set_New) :-
	is_on_shelf(Item, Shelf, Obvs), 
	% if it's on the shelf we have a perfect correction
	append(Set_A, [[Item => Shelf]], Set_B),
	generar_posibilidades(T, Obvs, Shelves, Set_B, Set_New)
	;
	% if not, generate all possible corrections 
	do_generar_posibilidades(Item, Shelves, Obvs, [], Set), 
	(
		% if we generated at least one posibility
		not_empty(Set),
		% add it
		append(Set_A, [Set], Set_B),
		generar_posibilidades(T, Obvs, Shelves, Set_B, Set_New)
		;
		% otherwise it's not possible for this item to be on any shelf, so skip it
		generar_posibilidades(T, Obvs, Shelves, Set_A, Set_New)
	).

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

% Plan
%	Select the first item
%	See if another object can swap places with it
%	If yes, then choose this swap and recurse
%	If recursion says no, keep checking options
%	If all options run out, just grab the last available best, fix it in
%	place, and keep going
eligir_explicacion2(Creencias, Posibilidades, Explicaciones) :-
	find_swap2(Creencias, Posibilidades, [], Explicaciones).

remove_from_others(_, [], Others_New, Others_New).
remove_from_others(Item, [[]|T_2], Others, Others_New) :-
	remove_from_others(Item, T_2, Others, Others_New).
remove_from_others(Item, [[Item => _|_]|T_2], Others, Others_New) :-
	remove_from_others(Item, T_2, Others, Others_New).
remove_from_others(Item, [Set|T_2], Others, Others_New) :-
	% not found
	append(Others, [Set], Others_B),
	remove_from_others(Item, T_2, Others_B, Others_New).

find_swap2(_, [], Cadena_New, Cadena_New).
find_swap2(Creencias, [[Item => Shelf|T]|T_2], Cadena_A, Cadena_New) :-
	% load original shelf
	is_on_shelf(Item, Shelf_Original, Creencias),
	% find another item that can swap with this item
	swap_exists(Creencias, Shelf_Original, Shelf, T_2, Item_Other),
	% lock in this item
	append(Cadena_A, [Item => Shelf], Cadena_B),
	% lock in the other item
	append(Cadena_B, [Item_Other => Shelf_Original], Cadena_C),
	% remove other item from contention
	remove_from_others(Item_Other, T_2, [], T_2_New),
	% proceed to others
	find_swap2(Creencias, T_2_New, Cadena_C, Cadena_New)
	;
	(
		% look for swap in other position
		find_swap2(Creencias, [T|T_2], Cadena_A, Cadena_New)
		;
		% lock in this item
		append(Cadena_A, [Item => Shelf], Cadena_B),
		% proceed to others
		find_swap2(Creencias, T_2, Cadena_B, Cadena_New)
	).

% find_swap(Creencias, [Item => Shelf|T], Others, Others_New, Cadena_A, Cadena_New) :-
% 	% load original shelf
% 	is_on_shelf(Item, Shelf_Original, Creencias),
% 	% find another item that can swap with this item
% 	swap_exists(Creencias, Shelf_Original, Shelf, Others, Item_Other),
% 	% lock in this item
% 	append(Cadena_A, [Item => Shelf], Cadena_B),
% 	% lock in the other item
% 	append(Cadena_B, [Item_Other => Shelf_Original], Cadena_New),
% 	% remove other item from contention
% 	remove_from_others(Item_Other, Others, [], Others_New)
% 	;
% 	(
% 		% look for swap in other position
% 		find_swap(Creencias, T, Others, Others_New, Cadena_A, Cadena_New)
% 		;
% 		% lock in this item
% 		append(Cadena_A, [Item => Shelf], Cadena_New),
% 		Others_New = Others
% 	).

% if there exists an item Shelf that used to be on Shelf_Original
swap_exists(Creencias, Shelf, Shelf_Original, [[]|T_2], Item) :- swap_exists(Creencias, Shelf, Shelf_Original, T_2, Item).
swap_exists(Creencias, Shelf, Shelf_Original, [[Item => Shelf|_]|T_2], Item) :-
	is_on_shelf(Item, Shelf_Original, Creencias)
	; swap_exists(Creencias, Shelf, Shelf_Original, T_2, Item).
swap_exists(Creencias, Shelf, Shelf_Original, [[_|T]|T_2], Item) :- swap_exists(Creencias, Shelf, Shelf_Original, [T|T_2], Item).


% given a 2d array of posibilities, expand it into a flat 1d array of
% all permutations. So [[1,2],[3,4]] should turn into [[1,3],[1,4],[2,3],2,4]]
% generar_explicaciones([], Cadena, Results_A, Results_New) :-
% 	append(Results_A, [Cadena], Results_New).
% % case where there are no posibilities for an item
% generar_explicaciones([[]], _, Results_New, Results_New).
% generar_explicaciones([[]|_], _, Results_New, Results_New).
% generar_explicaciones([[Item => Shelf|T]|T_2], Cadena_A, Results_A, Results_New) :-
% 	append(Cadena_A, [Item => Shelf], Cadena_B),
% 	generar_explicaciones(T_2, Cadena_B, Results_A, Results_B),
% 	generar_explicaciones([T|T_2], Cadena_A, Results_B, Results_New).

% choose an explanation given a set of explanations
% eligir_explicacion(_, [Item => Shelf], [Item => Shelf]). % Explanation with only one option, always choose that option
% eligir_explicacion(Creencias, Explicaciones, Eligido) :-
% 	do_eligir_explicacion(Creencias, Explicaciones, _, Eligido, 999999, _).
% 
% % compute the delta between this explanation and the ideal or expected
% % and select the option with minimum delta
% do_eligir_explicacion(_, [], Eligido_New, Eligido_New, Min_New, Min_New).
% do_eligir_explicacion(Creencias, [Explicacion|T], Eligido, Eligido_New, Min, Min_New) :-
% 	% Look for shelf count discrepancies
% 	delta_count(Creencias, Explicacion, Result),
% 	(Result @< Min),
% 	% If this solution has less of a discrepancy
% 	do_eligir_explicacion(Creencias, T, Explicacion, Eligido_New, Result, Min_New)
% 	;
% 	% If this solution has more of a discrepancy
% 	do_eligir_explicacion(Creencias, T, Eligido, Eligido_New, Min, Min_New).

%*************
% Delta count
%
% Returns the difference, in number of objects, of the given diagnosis to
% what the robot belives is true in terms of number of objects per shelf
%************

update_shelf(Shelf, Amount, [], [Shelf => Amount]).
update_shelf(Shelf, Amount, [Shelf => Count|T], [Shelf => Count_New|T]) :- 
	Count_New is Count + Amount.
update_shelf(Shelf, Amount, [H|T], [H|T_New]) :- 
	update_shelf(Shelf, Amount, T, T_New).

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

% apply an explanation to a set of beliefs. The explanation is an incomplete
% set [item1 => shelf1], that is some items are not defined. The belief is a 
% complete set.
aplicar_explicacion(Creencias_New, [], Creencias_New).
aplicar_explicacion(Creencias_A, [Item => Shelf|T], Creencias_New) :-
	do_aplicar_explicacion(Item, Shelf, Creencias_A, Creencias_B),
	aplicar_explicacion(Creencias_B, T, Creencias_New).

do_aplicar_explicacion(Item, Shelf, [], [Item => Shelf]).
do_aplicar_explicacion(Item, Shelf, [Item => _|T], [Item => Shelf_Old|T]) :-
	Shelf_Old = Shelf.
do_aplicar_explicacion(Item, Shelf, [H|T], [H|T_New]) :-
	do_aplicar_explicacion(Item, Shelf, T, T_New).

%******************************
% Generate shopkeeper actions
%******************************

% generates actions that the shopkeeper takes to fill the scenario in creencias
generate_shopkeeper_actions(_, [], _, Actions_New, Actions_New).
generate_shopkeeper_actions(Creencias, [Shelf|T], Location, Actions_A, Actions_New) :-
	% for each shelf
	will_visit_shelf(Creencias, Shelf),
	% append a movement action
	append(Actions_A, [mover(Location, Shelf)], Actions_B),
	% append all posible placement actions
	do_generate_actions(Creencias, Shelf, Actions_B, Actions_C),
	generate_shopkeeper_actions(Creencias, T, Shelf, Actions_C, Actions_New)
	;
	generate_shopkeeper_actions(Creencias, T, Location, Actions_A, Actions_New).

% returns true if the shopkeeper will visit the shelf during his routine,
% that is, if the shopkeeper will place an item on that shelf
%will_visit_shelf([Item => Shelf|T], Shelf) :- 
	%not(is_special_item_empty(Item))
	%; will_visit_shelf(T, Shelf).
will_visit_shelf([_ => Shelf|_], Shelf).
will_visit_shelf([_|T], Shelf) :- will_visit_shelf(T, Shelf).

do_generate_actions([], _, Actions_New, Actions_New).
do_generate_actions([Item => Shelf|T], Shelf, Actions_A, Actions_New) :-
	% for each shelf
	not(is_special_item_empty(Item)),
	% append a placement action
	append(Actions_A, [colocar(Item)], Actions_B),
	do_generate_actions(T, Shelf, Actions_B, Actions_New)
	;
	% do not append "empty" to the actions
	do_generate_actions(T, Shelf, Actions_A, Actions_New).
do_generate_actions([_|T], Shelf, Actions_A, Actions_New) :-
	do_generate_actions(T, Shelf, Actions_A, Actions_New).

