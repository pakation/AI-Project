:- ensure_loaded(proyecto1).

% Usage: open_kb("KB_Store_Ejemplo_Arturo.txt", KB), eligir_proximo_objecto(KB, [coke=>shelf2,heineken=>shelf3], shelf1, [coke,heineken], Item, Shelf, Cost).

eligir_proximo_objecto(KB, Creencias, Location, Items, Item_Best, Shelf_Best, Score_Best) :-
	props_class(mover_r, M_C, KB),
	props_class(buscar_r, B_C, KB),
	props_class(agarrar_r, A_C, KB),
	props_class(colocar_r, C_C, KB),

	props_class(buscar_r, B_R, KB),
	props_class(agarrar_r, A_R, KB),
	props_class(colocar_r, C_R, KB),

	props_class(buscar_p, B_P, KB),
	props_class(agarrar_p, A_P, KB),
	props_class(colocar_p, C_P, KB),

	do_eligir_proximo_objecto(Creencias, Location, [M_C, B_C, A_C, C_C], [B_R, A_R, C_R], [B_P, A_P, C_P], Items, [-9999999,_], [Score_Best,Item_Best=>Shelf_Best]).

is_on_shelf(Item, Shelf, [Item => Shelf|_]).
is_on_shelf(Item, Shelf, [_|T]) :- is_on_shelf(Item, Shelf, T).

get_movement_cost(_, Shelf_O, Shelf_O, 0).
get_movement_cost([], _, _, 999999).
get_movement_cost([[[Shelf_O,Shelf_D]=>Cost]|_], Shelf_O, Shelf_D, Cost).
get_movement_cost([_|T], Shelf_O, Shelf_D, Cost) :- 
	get_movement_cost(T, Shelf_O, Shelf_D, Cost).

get_recompensas_item(Item, [B_R, A_R, C_R], Recompensa) :-
	get_value_single(Item, B_R, R_B),
	get_value_single(Item, A_R, R_A),
	get_value_single(Item, C_R, R_C),
	Recompensa is R_B + R_A + R_C.

get_costs_item(Item, Location, [M_C, B_C, A_C, C_C], Cost) :-
	get_value_single(Item, B_C, Cost_B),
	get_value_single(Item, A_C, Cost_A),
	get_value_single(Item, C_C, Cost_C),
	get_movement_cost(M_C, Location, Shelf, Cost_M),
	Cost is Cost_B + Cost_A + Cost_C + Cost_M.

get_probability_item(Item, [B_P, A_P, C_P], Probability) :-
	get_value_single(Item, B_P, P_B),
	get_value_single(Item, A_P, P_A),
	get_value_single(Item, C_P, P_C),
	Probability is P_B * P_A * P_C.

get_value_single(Item, [Item=>Value|_], Value).
get_value_single(Item, [_|T], Value) :- get_value_single(Item, T, Value).

do_eligir_proximo_objecto(Creencias, Location, Costs, Recomps, Probs, [], Set_New, Set_New).
do_eligir_proximo_objecto(Creencias, Location, Costs, Recomps, Probs, [Item|T], [Score_A,Item_A=>Shelf_A], Set_New) :-
	% fetch the shelf the item is on
	is_on_shelf(Item, Shelf, Creencias),
	% recompensas
	get_recompensas_item(Item, Recomps, Recompensa),
	% costos
	get_costs_item(Item, Location, Costs, Cost),
	% probability
	get_probability_item(Item, Probs, Probability),
	% puntación
	Score is (100 + Recompensa - Cost) * Probability,
 	(Score @> Score_A),
 	do_eligir_proximo_objecto(Creencias, Location, Costs, Recomps, Probs, T, [Score,Item=>Shelf], Set_New)
 	;
 	do_eligir_proximo_objecto(Creencias, Location, Costs, Recomps, Probs, T, [Score_A,Item_A=>Shelf_A], Set_New).
