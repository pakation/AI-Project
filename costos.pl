:- ensure_loaded(proyecto1).

% Usage: open_kb("KB_Store_Ejemplo_Arturo.txt", KB), eligir_proximo_objecto(KB, [coke=>shelf2,heineken=>shelf3], shelf1, [coke,heineken], Item, Shelf, Cost).

eligir_proximo_objecto(KB, Creencias, Location, Items, Item_Best, Shelf_Best, Cost_Best) :-
	props_class(mover, M_Costs, KB),
	props_class(buscar, B_Costs, KB),
	props_class(agarrar, A_Costs, KB),
	props_class(colocar, C_Costs, KB),
	do_eligir_proximo_objecto(Creencias, M_Costs, B_Costs, A_Costs, C_Costs, Location, Items, [9999999999,_], [Cost_Best,Item_Best=>Shelf_Best]).

is_on_shelf(Item, Shelf, [Item => Shelf|_]).
is_on_shelf(Item, Shelf, [_|T]) :- is_on_shelf(Item, Shelf, T).

get_movement_cost([], _, _, 999999).
get_movement_cost([[[Shelf_O,Shelf_D]=>Cost]|_], Shelf_O, Shelf_D, Cost).
get_movement_cost([_|T], Shelf_O, Shelf_D, Cost) :- 
	get_movement_cost(T, Shelf_O, Shelf_D, Cost).

get_costs_item(Item, Buscar, Agarrar, Colocar, Cost) :-
	get_cost_single(Item, Buscar, Cost_B),
	get_cost_single(Item, Agarrar, Cost_A),
	get_cost_single(Item, Colocar, Cost_C),
	Cost is Cost_A + Cost_B + Cost_C.

get_cost_single(Item, [[Item=>Cost]|_], Cost).
get_cost_single(Item, [_|T], Cost) :- get_cost_single(Item, T, Cost).

do_eligir_proximo_objecto(Creencias, M_Costs, B_Costs, A_Costs, C_Costs, Location, [], Set_New, Set_New).
do_eligir_proximo_objecto(Creencias, M_Costs, B_Costs, A_Costs, C_Costs, Location, [Item|T], [Cost_A,Item_A=>Shelf_A], Set_New) :-
	% fetch the shelf the item is on
	is_on_shelf(Item, Shelf, Creencias),
	get_movement_cost(M_Costs, Location, Shelf, M_Cost),
	get_costs_item(Item, B_Costs, A_Costs, C_Costs, I_Cost),
	Cost is M_Cost + I_Cost,
 	(Cost @< Cost_A),
 	do_eligir_proximo_objecto(Creencias, M_Costs, B_Costs, A_Costs, C_Costs, Location, T, [Cost,Item=>Shelf], Set_New)
 	;
 	do_eligir_proximo_objecto(Creencias, M_Costs, B_Costs, A_Costs, C_Costs, Location, T, [Cost_A,Item_A=>Shelf_A], Set_New).
