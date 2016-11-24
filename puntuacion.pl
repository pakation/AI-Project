%:- ensure_loaded(proyecto1).

% Usage: open_kb("KB_Store_Ejemplo_Arturo.txt", KB), eligir_proximo_objecto(KB, [coke=>shelf2,heineken=>shelf3], shelf1, [coke,heineken], Item, Shelf, Cost).

eligir_proximo_objecto(KB, Creencias, Location, Items, Item_Best, Shelf_Best, Score_Best) :-
	props_class(mover_c, M_C, KB),
	props_class(buscar_c, B_C, KB),
	props_class(agarrar_c, A_C, KB),
	props_class(colocar_c, C_C, KB),

	props_class(buscar_r, B_R, KB),
	props_class(agarrar_r, A_R, KB),
	props_class(colocar_r, C_R, KB),

	props_class(buscar_p, B_P, KB),
	props_class(agarrar_p, A_P, KB),
	props_class(colocar_p, C_P, KB),

	do_eligir_proximo_objecto(Creencias, Location, [M_C, B_C, A_C, C_C], [B_R, A_R, C_R], [B_P, A_P, C_P], Items, [-9999999,_], [Score_Best,Item_Best=>Shelf_Best]).

calcular(C, R, P, Resultado) :-
	Temp is R - C,
	(
		(Temp == 0),
		Resultado is P
		; Resultado is (Temp * P)
	).

evaluar(KB, Location, Acciones, Puntos_Nuevo) :-
	props_class(mover_c, M_C, KB),
	props_class(buscar_c, B_C, KB),
	props_class(agarrar_c, A_C, KB),
	props_class(colocar_c, C_C, KB),

	props_class(mover_r, M_R, KB),
	props_class(buscar_r, B_R, KB),
	props_class(agarrar_r, A_R, KB),
	props_class(colocar_r, C_R, KB),

	props_class(mover_p, M_P, KB),
	props_class(buscar_p, B_P, KB),
	props_class(agarrar_p, A_P, KB),
	props_class(colocar_p, C_P, KB),

	do_evaluar(KB, Location, Acciones, [M_C, B_C, A_C, C_C], [M_R, B_R, A_R, C_R], [M_P, B_P, A_P, C_P], 0, Puntos_Nuevo).

do_evaluar(_, _, [], [_, _, _, _], [_, _, _, _], [_, _, _, _], Puntos_Nuevo, Puntos_Nuevo).
do_evaluar(KB, Location, [mover(Origen,Destino)|T], [M_C, B_C, A_C, C_C], [M_R, B_R, A_R, C_R], [M_P, B_P, A_P, C_P], Puntos_A, Puntos_Nuevo) :-
	get_movement_cost(M_C, Origen, Destino, C_M),
	get_movement_cost(M_R, Origen, Destino, R_M),
	get_movement_cost(M_P, Origen, Destino, P_M),
	calcular(C_M, R_M, P_M, Puntos),
	Puntos_B is Puntos_A + Puntos,
	do_evaluar(KB, Location, T, [M_C, B_C, A_C, C_C], [M_R, B_R, A_R, C_R], [M_P, B_P, A_P, C_P], Puntos_B, Puntos_Nuevo).
do_evaluar(KB, Location, [agarrar(Item)|T], [M_C, B_C, A_C, C_C], [M_R, B_R, A_R, C_R], [M_P, B_P, A_P, C_P], Puntos_A, Puntos_Nuevo) :-
	get_value_single(Item, A_C, C_A), % costo
	get_value_single(Item, A_R, R_A), % recompensa
	get_value_single(Item, A_P, P_A), % probabilidad
	calcular(C_A, R_A, P_A, Puntos),
	Puntos_B is Puntos_A + Puntos,
	do_evaluar(KB, Location, T, [M_C, B_C, A_C, C_C], [M_R, B_R, A_R, C_R], [M_P, B_P, A_P, C_P], Puntos_B, Puntos_Nuevo).
do_evaluar(KB, Location, [colocar(Item)|T], [M_C, B_C, A_C, C_C], [M_R, B_R, A_R, C_R], [M_P, B_P, A_P, C_P], Puntos_A, Puntos_Nuevo) :-
	get_value_single(Item, C_C, C_O), % costo
	get_value_single(Item, C_R, R_C), % recompensa
	get_value_single(Item, C_P, P_C), % probabilidad
	calcular(C_O, R_C, P_C, Puntos),
	Puntos_B is Puntos_A + Puntos,
	do_evaluar(KB, Location, T, [M_C, B_C, A_C, C_C], [M_R, B_R, A_R, C_R], [M_P, B_P, A_P, C_P], Puntos_B, Puntos_Nuevo).
do_evaluar(KB, Location, [buscar(Item)|T], [M_C, B_C, A_C, C_C], [M_R, B_R, A_R, C_R], [M_P, B_P, A_P, C_P], Puntos_A, Puntos_Nuevo) :-
	get_value_single(Item, B_C, C_B), % costo
	get_value_single(Item, B_R, R_B), % recompensa
	get_value_single(Item, B_P, P_B), % probabilidad
	calcular(C_B, R_B, P_B, Puntos),
	Puntos_B is Puntos_A + Puntos,
	do_evaluar(KB, Location, T, [M_C, B_C, A_C, C_C], [M_R, B_R, A_R, C_R], [M_P, B_P, A_P, C_P], Puntos_B, Puntos_Nuevo).
do_evaluar(KB, Location, [_|T], [M_C, B_C, A_C, C_C], [M_R, B_R, A_R, C_R], [M_P, B_P, A_P, C_P], Puntos_A, Puntos_Nuevo) :-
	do_evaluar(KB, Location, T, [M_C, B_C, A_C, C_C], [M_R, B_R, A_R, C_R], [M_P, B_P, A_P, C_P], Puntos_A, Puntos_Nuevo).

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

get_costs_item(Creencias, Item, Location, [M_C, B_C, A_C, C_C], Cost) :-
	get_value_single(Item, B_C, Cost_B),
	get_value_single(Item, A_C, Cost_A),
	get_value_single(Item, C_C, Cost_C),
	is_on_shelf(Item, Shelf, Creencias),
	get_movement_cost(M_C, Location, Shelf, Cost_M),
	Cost is Cost_B + Cost_A + Cost_C + Cost_M.

get_probability_item(Item, [B_P, A_P, C_P], Probability) :-
	get_value_single(Item, B_P, P_B),
	get_value_single(Item, A_P, P_A),
	get_value_single(Item, C_P, P_C),
	Probability is P_B * P_A * P_C.

get_value_single(Item, [Item=>Value|_], Value).
get_value_single(Item, [_|T], Value) :- get_value_single(Item, T, Value).

do_eligir_proximo_objecto(_, _, _, _, _, [], Set_New, Set_New).
do_eligir_proximo_objecto(Creencias, Location, Costs, Recomps, Probs, [reacomodar(Item)|T], [Score_A,Item_A=>Shelf_A], Set_New) :-
	% fetch the shelf the item is on
	is_on_shelf(Item, Shelf, Creencias),
	% recompensas
	get_recompensas_item(Item, Recomps, Recompensa),
	% costos
	get_costs_item(Creencias, Item, Location, Costs, Cost),
	% probability
	get_probability_item(Item, Probs, Probability),
	% puntación
	calcular(Cost, Recompensa, Probability, Score),
 	(Score @> Score_A),
 	do_eligir_proximo_objecto(Creencias, Location, Costs, Recomps, Probs, T, [Score,Item=>Shelf], Set_New)
 	;
 	do_eligir_proximo_objecto(Creencias, Location, Costs, Recomps, Probs, T, [Score_A,Item_A=>Shelf_A], Set_New).
do_eligir_proximo_objecto(Creencias, Location, Costs, Recomps, Probs, [_|T], [Score_A,Item_A=>Shelf_A], Set_New) :-
	do_eligir_proximo_objecto(Creencias, Location, Costs, Recomps, Probs, T, [Score_A,Item_A=>Shelf_A], Set_New).
% do_eligir_proximo_objecto(Creencias, Location, Costs, Recomps, Probs, [_|T], [Score_A,Item_A=>Shelf_A], Set_New) :-
%  	do_eligir_proximo_objecto(Creencias, Location, Costs, Recomps, Probs, T, [Score_A,Item_A=>Shelf_A], Set_New).
