%Funciones auxiliares para should_be_in_shelf 
find_shelf_inner(Clase,[class(Clase,_,_,[Clase=>ShelfOriginal_Objeto],_)|_], ShelfOriginal_Objeto).
find_shelf_inner(Clase, [_|T], ShelfOriginal_Objeto):-
	find_shelf_inner(Clase, T, ShelfOriginal_Objeto).
find_shelf_inner(_, [], _).

find_shelf([Clase|R], KB_Original, ShelfOriginal_Objeto):- 
    find_shelf_inner(Clase,KB_Original, ShelfOriginal_Objeto),
	find_shelf(R, KB_Original, ShelfOriginal_Objeto).
find_shelf([],_,_).

%Encuentra el shelf en el que debería estar el objeto
should_be_in_shelf(Objeto, ShelfOriginal_Objeto, KB_Original):-
	class_inst(Objeto, Clases_Inst, KB_Original),
	find_shelf(Clases_Inst, KB_Original, ShelfOriginal_Objeto).

top_down(_, [], _, PosRobot_New, PosRobot_New, Set_New, Set_New).
top_down(KB, Decisiones, Diagnostico, PosRobot, PosRobot_New, Set, Set_New) :-
	eligir_proximo_objecto(KB, Diagnostico, PosRobot, Decisiones, Mejor_Obj, Mejor_Shelf, _),
	remove_generic(reacomodar(Item), Decisiones, [], Decisiones_B),
	should_be_in_shelf(Mejor_Obj, Target_Shelf, KB),
	(
		(Mejor_Shelf == PosRobot), 
		append(Set, [buscar(Item),agarrar(Item),mover(PosRobot,Target_Shelf),colocar(Item)], Set_A)
		;
		append(Set, [mover(PosRobot,Mejor_Shelf),buscar(Item),agarrar(Item),mover(Mejor_Shelf,Target_Shelf),colocar(Item)], Set_A)
	),
	top_down(KB, Decisiones_B, Diagnostico, Target_Shelf, PosRobot_New, Set_A, Set_New).

remove_entregas([], Set_New, Set_New, Output_New, Output_New).
remove_entregas([entregar(Item)|T], Set_A, Set_New, Output, Output_New):- 
	append(Output, [Item], Output_B),
	remove_entregas(T, Set_A, Set_New, Output_B, Output_New).
remove_entregas([H|T], Set_A, Set_New, Output, Output_New):-
	append(Set_A, [H], Set_B),
	remove_entregas(T, Set_B, Set_New, Output, Output_New).

append_entregas([], _, _, Set_New, Set_New).
append_entregas([Item|T], Diagnostico, PosRobot, Set, Set_New) :-
	is_on_shelf(Item, Shelf, Diagnostico),
	(
		(PosRobot == Shelf),
		append(Set, [buscar(Item),agarrar(Item),mover(Shelf,origen),colocar(Item)], Set_A)
		;
		append(Set, [mover(PosRobot,Shelf),buscar(Item),agarrar(Item),mover(Shelf,origen),colocar(Item)], Set_A)
	),
	append_entregas(T, Diagnostico, cliente, Set_A, Set_New).

planeacion(KB, Decisiones, Diagnostico, PosRobot, Set_New) :-
	remove_entregas(Decisiones, [], Decisiones_Clean, [], Entregas),
	top_down(KB, Decisiones_Clean, Diagnostico, PosRobot, PosRobot_New, [], Set_A),
	append_entregas(Entregas, Diagnostico, PosRobot_New, Set_A, Set_New).
