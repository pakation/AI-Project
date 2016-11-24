%****************************************************************
%*PROYECTO    : PROYECTO DE BÚSQUEDA                            *
%*INTEGRANTES : Cheung Derek                                    *
%*              González Rico Diana Virginia                    *
%*              Neri González José Francisco                    *
%*FECHA       : 24/Noviembre/2016                               *
%*DESCRIPCIÓN : Un robot de servicio opera como asistente en un *
%supermercado, utilizando módulo de inferencia oportunista, que *
%implica realizar un diagnóstico, una toma de decisión y una    *
%planeación.                                                    *
%****************************************************************

%****************************************************************
% MÓDULO DE DIAGNÓSTICO
%****************************************************************
% cases handled
%	new item
%	location contradiction
%	location contradiction such that item cannot possibly be anywhere
%	no contradictions
%
% TODO if the item that appeared in Observations is not known in the kb,
% do not add it to creencias
%
% TODO deal with observations where shelf is totally empty 
% (have caller inject a special "empty => s1" item without it being 
% considered as product?)
:- op(800,xfx,'=>').
proyecto_busqueda(Diagnostico,Act_asistente,Decision_robot):-open_kb('C:/IA/KB_Store.txt',KB),
															 diagnosis(KB,Diagnostico,Act_asistente),
															 decision(KB,Diagnostico,Decision_robot).
diagnosis(KB,Creencias_New, Acciones) :-
	rels_inst(report, Creencias, KB),
	rels_inst(observations, Observaciones, KB),
	extension_class(shelves, Estantes, KB),
	do_diagnosis(Creencias, Observaciones, Estantes, Creencias_New, Acciones).

% Creencias, creencias nuevos, observaciones, acciones
do_diagnosis(Creencias, Observaciones, Estantes, Creencias_New, Acciones) :- 
	remove_inconsistencies(Creencias, Observaciones, [], Creencias_Malos, [], Creencias_Limpios),
	generar_posibilidades(Creencias_Malos, Observaciones, Estantes, [], Posibilidades),
	generar_explicaciones(Posibilidades, [], [], Explicaciones),
	eligir_explicacion(Creencias, Explicaciones, Eligido),
	append(Creencias_Limpios, Eligido, Creencias_B),
	new_obsv_to_creencias(Creencias_B, Observaciones, Creencias_New),
	generate_shopkeeper_actions(Creencias_New, Estantes, l0, [], Acciones).

is_item_expected(Item, [Item => _|_]).
is_item_expected(Item, [_|T]) :- is_item_expected(Item, T).

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

% true if every shelf has been seen, false otherwise
% has_seen_all_shelves([], Obvs).
% has_seen_all_shelves([Shelf|T], Obvs) :-
% 	has_seen_shelf(Shelf, Obvs),
% 	has_seen_all_shelves(T, Obvs)
% 	;
% 	false.

% true if the robot has seen the given shelf, false otherwise
has_seen_shelf(Shelf, [_ => Shelf|_]).
has_seen_shelf(Shelf, [_|T]) :- has_seen_shelf(Shelf, T).

is_on_shelf(Item, Shelf, [Item => Shelf|_]).
is_on_shelf(Item, Shelf, [_|T]) :- is_on_shelf(Item, Shelf, T).

% For each item, generate a set of possible shelves that the item could 
% possibly be
generar_posibilidades([], _, _, Set_New, Set_New).
generar_posibilidades([Item|T], Obvs, Shelves, Set_A, Set_New) :-
	is_on_shelf(Item, Shelf, Obvs), % if it's on the shelf we have a perfect correction
	append(Set_A, [[Item => Shelf]], Set_B),
	generar_posibilidades(T, Obvs, Shelves, Set_B, Set_New)
	; 
	do_generar_posibilidades(Item, Shelves, Obvs, [], Set), % if not, generate all possible corrections
	(
		not_empty(Set),
		append(Set_A, [Set], Set_B),
		generar_posibilidades(T, Obvs, Shelves, Set_B, Set_New)
		;
		generar_posibilidades(T, Obvs, Shelves, Set_A, Set_New)
	).

not_empty([_]).
not_empty([_|_]).

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

% given a 2d array of posibilities, expand it into a flat 1d array of
% all permutations. So [[1,2],[3,4]] should turn into [[1,3],[1,4],[2,3],2,4]]
generar_explicaciones([], Cadena, Results_A, Results_New) :-
	append(Results_A, [Cadena], Results_New).
% case where there are no posibilities for an item
generar_explicaciones([[]], _, Results_New, Results_New).
generar_explicaciones([[]|_], _, Results_New, Results_New).
generar_explicaciones([[Item => Shelf|T]|T_2], Cadena_A, Results_A, Results_New) :-
	append(Cadena_A, [Item => Shelf], Cadena_B),
	generar_explicaciones(T_2, Cadena_B, Results_A, Results_B),
	generar_explicaciones([T|T_2], Cadena_A, Results_B, Results_New).

% choose an explanation given a set of explanations
eligir_explicacion(_, [Item => Shelf], [Item => Shelf]). % Explanation with only one option, always choose that option
eligir_explicacion(Creencias, Explicaciones, Eligido) :-
	do_eligir_explicacion(Creencias, Explicaciones, _, Eligido, 999999, _).

% compute the delta between this explanation and the ideal or expected
% and select the option with minimum delta
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
% TODO prove that this is is OK for all cases
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

% generates actions that the shopkeeper takes to fill the scenario in creencias
generate_shopkeeper_actions(_, [], _, Actions_New, Actions_New).
generate_shopkeeper_actions(Creencias, [Shelf|T], Location, Actions_A, Actions_New) :-
	will_visit_shelf(Creencias, Shelf),
	append(Actions_A, [mover(Location, Shelf)], Actions_B),
	do_generate_actions(Creencias, Shelf, Actions_B, Actions_C),
	generate_shopkeeper_actions(Creencias, T, Shelf, Actions_C, Actions_New)
	;
	generate_shopkeeper_actions(Creencias, T, Location, Actions_A, Actions_New).

% returns true if the shopkeeper will visit the shelf during his routine,
% that is, if the shopkeeper will place an item on that shelf
will_visit_shelf([Item => Shelf|T], Shelf) :- 
	not(is_special_item_empty(Item))
	; will_visit_shelf(T, Shelf).
will_visit_shelf([_|T], Shelf) :- will_visit_shelf(T, Shelf).

% true if we have a match with empty
is_special_item_empty(empty).

do_generate_actions([], _, Actions_New, Actions_New).
do_generate_actions([Item => Shelf|T], Shelf, Actions_A, Actions_New) :-
	not(is_special_item_empty(Item)),
	% do not append "empty" to the actions
	append(Actions_A, [colocar(Item)], Actions_B),
	do_generate_actions(T, Shelf, Actions_B, Actions_New)
	;
	do_generate_actions(T, Shelf, Actions_A, Actions_New).
do_generate_actions([_|T], Shelf, Actions_A, Actions_New) :-
do_generate_actions(T, Shelf, Actions_A, Actions_New).

%****************************************************************
% MÓDULO DE TOMA DE DECISIÓN
%****************************************************************
%Leer el diagnostico generado en el Módulo de diagnostico
%Por ejemplo:
%[maruchan=>shelf1,heineken=>shelf1,coke=>shelf2,marias=>shelf3]

%Leer actividades pendientes
%Por ejemplo:
%entregar(robot,maruchan,client)
%reacomodar(robot,marias,shelf3)

%Comparar cada elemento del diagnostico con cada elemento del mundo ideal. En caso, de que haya inconsistencias
%se debe generar por cada elemento un reacomodo por parte del robot. Evaluar que acciones son prioritarias
%para generar una toma de decisión y generar posteriormente un plan.

%Toma de decision
%Diagnostico: Proviene del módulo de Dianóstico.
%Entregas_pendientes: Solicitudes que realiza el cliente al robot.
%Actividades: Actvidades que decide el robot por ejecutar.
decision([],[]).
decision(KB,Diagnostico,Actividades):-extension_class(pending_activities,Entregas_pendientes,KB),
									  productos_a_reacomodar(KB, Diagnostico,Reacomodar),
									  eligir_conjunto(5000, Entregas_pendientes, Reacomodar, Actividades).
									  % lista_de_actividades(Reacomodar,Act),
									  % verifica_actividades(Act,Entregas_pendientes,Lista_act),
									  % concat_actividades(Entregas_pendientes,Lista_act,Actividades),!.

%%%%%%
% New
%%%%%%

remove_generic(_, [], Set_New, Set_New).
remove_generic(Target, [Target|T], Set_A, Set_New):- 
	remove_generic(Objeto, T, Set_A, Set_New).
remove_generic(Objeto, [H|T], Set_A, Set_New):-
	append(Set_A, [H], Set_B),
	remove_generic(Objeto, T, Set_B, Set_New).

eligir_conjunto(_, Set_New, [], Set_New).
eligir_conjunto(Umbral, Set, Reacomodas, Set_New) :-
	eligir_reacomoda(Set, Reacomodas, 0, High_New, _, [Item => Shelf]),
	(
		(High_New @> Umbral),
		% force stop
		eligir_conjunto(Umbral, Set, [], Set_New)
		;
		append(Set, [reacomodar(Item)], Set_B),
		remove_generic(Item => Shelf, Reacomodas, [], Reacomodas_B),
		eligir_conjunto(Umbral, Set_B, Reacomodas_B, Set_New)
	).

eligir_reacomoda(Set, [], High_New, High_New, Choice_New, Choice_New).
eligir_reacomoda(Set, [Item => Shelf|T], High, High_New, Choice, Choice_New) :-
	append(Set, [reacomodar(Item)], Set_A),
	verifica_puntuacion(Set_A, Puntos),
	(Puntos @> High),
	eligir_reacomoda(Set, T, Puntos, High_New, [Item => Shelf], Choice_New)
	; eligir_reacomoda(Set, T, High, High_New, Choice, Choice_New).

%%%%%%
% End New
%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
elimina_elemento_r([], Lista, Lista).
elimina_elemento_r(Elemento, [Elemento|T], T).
elimina_elemento_r(Elemento, [H|T], [H|R]) :- elimina_elemento_r(Elemento, T,R).

%Elimina una lista de reacomodos de la lista original (Reacomodos Padre,Reacomodo_Original,Lista)
elimina_reacomodos([],Lista,Lista).
elimina_reacomodos([H|T],Lista_reacomodos,Lista_nueva):- elimina_elemento_r(H,Lista_reacomodos,Lista_A),write(T),write(Lista_A),
														 elimina_reacomodos(T,Lista_A,Lista_nueva),!.
														 
%Obtener los reacomodos de una lista de tareas combinadas
verifica_si_es_reacomodo([],[]).
verifica_si_es_reacomodo(entregar(_),Lista):- append([],[],Lista).
verifica_si_es_reacomodo(reacomodar(Producto),Lista):- append([],[reacomodar(Producto)],Lista).
obtiene_reacomodos_comb([],[]).
obtiene_reacomodos_comb([H|T],Lista):-verifica_si_es_reacomodo(H,Lista_A),write(T),obtiene_reacomodos_comb(T,Lista_B),
									  append(Lista_A,Lista_B,Lista).


%Genera una lista de reacomodos
genera_combinacion_reacomodos(Reacomodos,Lista_reacomodos):- combinaciones(Reacomodos,1,Lista_reacomodos).

concatena_entrega_reacomodos([_|_],[],[]).
concatena_entrega_reacomodos(Entrega,[H|T],Lista):-append(Entrega,H,Lista_A),concatena_entrega_reacomodos(Entrega,T,Lista_B),
												   append([Lista_A],Lista_B,Lista),!.

genera_combinacion_entrega_reacomodos(Entrega,List_reacomodos,Nivel):-genera_combinacion_reacomodos(List_reacomodos,Lista),
																	  concatena_entrega_reacomodos(Entrega,Lista,Nivel).

%Valida la primera entrega 
verifica_puntuacion([entregar(refresco)],10).
verifica_puntuacion([entregar(cerveza)],20).
verifica_puntuacion([entregar(sopa)],40).
verifica_puntuacion([entregar(galletas)],15).
verifica_puntuacion([entregar(refresco),reacomodar(sopa)],32).
verifica_puntuacion([entregar(refresco),reacomodar(cerveza)],45).
verifica_puntuacion([entregar(refresco),reacomodar(galletas)],36).
verifica_puntuacion([entregar(refresco),reacomodar(cerveza),reacomodar(sopa)],80).

obtiene_puntuacion_nivel(Entrega,Puntuacion):- verifica_puntuacion(Entrega,Puntuacion).

%Verifica si una lista de actividades está dentro del umbral. Si es correcto, regresa la lista con su puntuacion, de lo contrario, regresa lista vacia.
comprueba_umbral(Umbral,Puntuacion):- Puntuacion < Umbral.
verifica_actividades_en_umbral(Umbral,Actividades,Lista):- (obtiene_puntuacion_nivel(Actividades,Puntuacion),
														     comprueba_umbral(Umbral,Puntuacion), append([Actividades => Puntuacion],[],Lista)) ; 
															 append([],[],Lista).
															 
%verifica_actividades_u(Umbral,Actividad,Lista):- verifica_actividades_en_umbral(Umbral,Actividad,Lista).

generar_nivel(Padre,List_reacomodos,Nivel):- genera_combinacion_entrega_reacomodos(Padre,List_reacomodos,Nivel).


obtiene_lista_actividades_y_puntuaciones(_,[],[]).
obtiene_lista_actividades_y_puntuaciones(Umbral,[H|T],Lista):- verifica_actividades_en_umbral(Umbral,H,Lista_A),
															   obtiene_lista_actividades_y_puntuaciones(Umbral,T,Lista_B),
															   append(Lista_A,Lista_B,Lista),!.
%%%%%Aqui mando la entrega y recomodos
%%%%
%ejecutar  --> obtiene_mejor_solucion(50,[entregar(refresco)],[reacomodar(sopa),reacomodar(cerveza),reacomodar(galletas)],Lista).
obtiene_mejor_solucion(Umbral,Entregas_pendientes,Reacomodos,Lista_salida):-verifica_actividades_en_umbral(Umbral,Entregas_pendientes,Lista_Puntu_Raiz),write(Lista_Puntu_Raiz),
					generar_nivel(Entregas_pendientes,Reacomodos,Nivel),obtiene_lista_actividades_y_puntuaciones(Umbral,Nivel,Lista_Puntu_Nivel),
					append(Lista_Puntu_Raiz,Lista_Puntu_Nivel,Lista_salida).



%*********
partes(L1,L2) :-
findall(Y,subconjunto(Y,L1),L2).

subconjunto([],[]).
subconjunto([X|L1],[X|L2]) :- subconjunto(L1,L2).
subconjunto(L1,[_|L2]) :-
subconjunto(L1,L2).

combinacion(L1,N,L2) :-
combinacion_2(L1,N,L2).
combinacion_1(L1,N,L2) :-
subconjunto(L2,L1),
length(L2,N).
combinacion_2(L1,N,L2) :-
length(L2,N),
subconjunto(L2,L1).


combinaciones(L1,N,L2) :-
combinaciones_2(L1,N,L2).

combinaciones_1(L1,N,L2) :-
findall(L,combinacion_1(L1,N,L),L2).

combinaciones_2(L1,N,L2) :-
findall(L,combinacion_2(L1,N,L),L2).

permuta([],[]).
permuta([X|Xs],Ys):- permuta(Xs,Zs), inserta(X,Zs,Ys).
inserta(X,L,[X|L]).
inserta(X,[Y|Ys],[Y|Ys1]):- inserta(X,Ys,Ys1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Si existe la funcion reacomodar y entregar al cliente de un mismo producto,
%se elimina la actividad reacomodar.														  
verifica_actividad([],[],[]).
verifica_actividad(reacomodar(Producto),[entregar(Producto)|_],Lista):- append([],[],Lista).
verifica_actividad(reacomodar(Producto),[],Lista):- append([reacomodar(Producto)],[],Lista).
verifica_actividad(reacomodar(Producto),[_|T],Lista) :- verifica_actividad(reacomodar(Producto),T,Lista).

verifica_actividades([],_,[]).
verifica_actividades([H|T],L,Lista_Nueva):- verifica_actividad(H,L,Lista_A),verifica_actividades(T,L,Lista_B),append(Lista_A,Lista_B,Lista_Nueva),!.


%Concatena 2 listas de elementos
concat_actividades([], Elemento, Elemento).
concat_actividades([H|T],Lista,[H|R]):-concat_actividades(T,Lista,R).

%my_append([], Cs, Cs).
%my_append([A|As],Bs,[A|Cs]):-my_append(As, Bs, Cs).

%Genera lista de actividades de acuerdo a la lista de reacomodos.
lista_de_actividades([],[]).
lista_de_actividades([H|T],Actividades):-genera_actividad(H,Actividad),lista_de_actividades(T,Acts),append(Actividad,Acts,Actividades).

%Genera una actividad de un producto por reacomodar.
genera_actividad([],[]).
genera_actividad(Producto => _,Actividad):-append([reacomodar(Producto)],[],Actividad).


%Genera una lista de productos que se deben reacomodar con base en el Diagnostico.
productos_a_reacomodar(_, [],[]).
productos_a_reacomodar(KB,Diagnostico,Reacomodar):- obtiene_productos(KB,Productos),obtiene_ubicacion_producto(KB, Productos,Mundo_real),
												 verif_lista_shelfs(Mundo_real,Diagnostico,Reacomodar). 

%Obtiene del Mundo Real la lista de relaciones(A nivel clase) para verificar en que shelf se encuentra cada producto. Y cambia el valor del producto por la clase.
obtiene_ubicacion_producto(KB, [],[]).	
obtiene_ubicacion_producto(KB, [H|T],Lista_ubicacion):- obtiene_ubicacion_producto(KB, H,[Clase => Shelf]),obtiene_ubicacion_producto(KB,T,Ubi),
													append([Clase => Shelf],Ubi,Lista_ubicacion),!.
obtiene_ubicacion_producto(KB, Producto,Ubicacion):- rels_inst(Producto,[Clase=>Shelf],KB),
												 cambia_valor(Producto,Clase,[Clase=>Shelf],Ubicacion),!.
									  

%Obtiene lista de productos que deben existir en la base de conocimiento.
obtiene_productos(KB, Productos):-extension_class(comestible,Productos,KB),!.

%Cambia el nombre de la clase por el producto para verificar en que shelf se encuentra.
cambia_valor(Producto, Clase, [Clase => Shelf|T], [Producto => Shelf|T]).
cambia_valor(Producto, Clase, [H|T], [H|R]) :- actualiza_valor(Producto, Clase, T,R).

%Verifica que un producto esté en el shelf correcto. Si está bien acomodado regresa Lista vacia, de lo contrario, regresa el shelf donde debe estar
%acomodado.
verif_shelf(_ => _,[],Lista):- append([],[],Lista). %El producto no está aún acomododado en los shelfs.
verif_shelf(Producto => Shelf,[Producto => Shelf|_],Lista):- append([],[],Lista). %Está el producto en el shelf correcto.
verif_shelf(Producto => Shelf,[Producto => _|_],Lista):- append([Producto => Shelf],[],Lista).%Esta el producto en shelf incorrecto
verif_shelf(Producto => Shelf,[_|T],Lista) :- verif_shelf(Producto => Shelf,T,Lista).

%Verifica que los productos del diagnostico estén en el shelf del mundo real(KB).
%verif_lista_shelfs(Shelfs_Mundo_Real,Diagnostico,Productos_por_acomodar)
verif_lista_shelfs([],_,[]).
verif_lista_shelfs([H|T],L,Lista_Nueva):- verif_shelf(H,L,Lista_A),verif_lista_shelfs(T,L,Lista_B),append(Lista_A,Lista_B,Lista_Nueva),!.

%****************************************************************
% MÓDULO DE PLANEACIÓN
%****************************************************************

%**********************************************************************************************************************
%****************************************************************
%*PROYECTO    : REPRESENTACIÓN DEL CONOCIMIENTO                 *
%*INTEGRANTES : Cheung Derek                                    *
%*              González Rico Diana Virginia                    *
%*              Neri González José Francisco                    *
%*FECHA       : 11/Octubre/2016                                 *
%*DESCRIPCIÓN : Generar una Base de conocimiento(KB) a partir   *
%*de la jerarquía conceptual con defaults y excepciones         * 
%*explicada en class y de la especificación de la base de       *
%*conocimiento presentada en el artículo "A Light Non-Monotonic *
%*Knowledge-Base for Service Robots"                            *
%****************************************************************

%****************************************************************
% Carga y lectura de la base de conocimiento
%****************************************************************

%KB open and save

open_kb(Route,KB):-
	open(Route,read,Stream),
	readclauses(Stream,X),
	close(Stream),
	atom_to_term(X,KB).

save_kb(Route,KB):-
	open(Route,write,Stream),
	writeq(Stream,KB),
	close(Stream).

readclauses(InStream,W) :-
        get0(InStream,Char),
        checkCharAndReadRest(Char,Chars,InStream),
	atom_chars(W,Chars).

checkCharAndReadRest(-1,[],_) :- !.  % End of Stream	
checkCharAndReadRest(end_of_file,[],_) :- !.

checkCharAndReadRest(Char,[Char|Chars],InStream) :-
        get0(InStream,NextChar),
        checkCharAndReadRest(NextChar,Chars,InStream).

%compile an atom string of characters as a prolog term
atom_to_term(ATOM, TERM) :-
	atom(ATOM),
	atom_to_chars(ATOM,STR),
	atom_to_chars('.',PTO),
	append(STR,PTO,STR_PTO),
	read_from_chars(STR_PTO,TERM).

%:- op(800,xfx,'=>').

%****************************************************************
% 1a. Extensiones de una class
%
% Usage: extension_class(NomClase, ExtensionesPorId, KnowledgeBase)
%****************************************************************

% Buscar classs que tiene este nombre como su madre
ec_madre(NomClaseMadre, Exts, KB_Original, [class(NomClase,NomClaseMadre,_,_,Insts_A)|T]) :-
	% Seguir buscando en T porque es posible que hay múltiples que tiene
	%  NomClaseMadre como su madre. Pero ignore() el resultado porque
	%  queremos ejecutar lo siguiente si esa busqueda falla o no.
	ignore(ec_madre(NomClaseMadre, Insts_B, KB_Original, T)),
	% Aggregar los instancias de este class con los instancias que la
	%  busqueda encontró
	append(Insts_A, Insts_B, Insts_C),
	% Empezar una nueva busqueda por classs que tiene este class como
	%  madre. Otra vez, si ningún tal class existe no nos importamos si
	%  el resultado es verdadero o falso, queremos seguir con append
	ignore(ec_madre(NomClase, Insts_D, KB_Original, KB_Original)),
	% Append estas nuevas instancias, si hay, y devolver el resultado en Exts
	append(Insts_C, Insts_D, Exts).
% Recursión. Si NomClase no corresponde con el valor de Madre de este class, seguir con el resto de la lista
ec_madre(NomClaseMadre, Exts, KB_Original, [_|T]) :- ec_madre(NomClaseMadre, Exts, KB_Original, T).
% Caso base. Cuando ya no tenemos elementos para probar, devolver una lista vacia
ec_madre(_, [], _, []).

% Buscar class que tiene este nombre como su nombre
ec(NomClase, Insts, KB_Original, [class(NomClase,_,_,_,Insts_A)|_]) :-
	% Empezar una nueva busqueda por classs que tiene este class como
	%  su madre. Pero ignore() el resultado porque queremos ejecutar lo 
	% siguiente si esa busqueda falla o no.
	ignore(ec_madre(NomClase, Insts_B, KB_Original, KB_Original)),
	% Append estas nuevas instancias, si hay, y devolver el resultado en Exts
	append(Insts_A, Insts_B, Insts).
	% No buscamos otras classs dado que no puede existir dos classs
	% del mismo nombre
% Recursión. Si NomClase no corresponde con el valor de Madre de este class, seguir con el resto de la lista
ec(NomClase, Exts, KB_Original, [_|T]) :- ec(NomClase, Exts, KB_Original, T).
% Caso base. Cuando ya no tenemos elementos para probar, devolver una lista vacia
%ec(_, [], _, []).

% Convertir una lista de objectos a una lista de sus ids
insts_ids([[id => Name,_,_]| T], Ids, Ids_New) :-
	append(Ids, [Name], Ids_Tmp),
	insts_ids(T, Ids_Tmp, Ids_New).
insts_ids([], Ids, Ids).

extension_class(NomClase, Ids, KB_Original) :-
	ec(NomClase, Insts_Raw, KB_Original, KB_Original),
	insts_ids(Insts_Raw, [], Ids)
	; Ids = unknown.

%****************************************************************
% 1b. Extensiones de una propiedad
%
% Usage: extension_propiedad(Attr, ExtensionesConValorDeProp, KnowledgeBase)
%****************************************************************

% Recuperar un valor de una lista de propiedades, dado un attributo.
%  Evalua a falso si el valor no existe.
get_value(not(Attr) => yes, [not(Attr)|_]). % not(...) corresponde exclusivamente con not(...) cuando buscando
get_value(not(Attr) => no, [Attr =>_|_]).
get_value(not(Attr) => no, [Attr|_]).

get_value(Attr => Value, [Attr => Value|_]).
get_value(Attr => yes, [Attr|_]). % traducir attr a attr => yes
get_value(Attr => no, [not(Attr)|_]). % traducir not(attr) a attr => no
get_value(Attr => not(Value), [not(Attr => Value)|_]).
get_value(Elemento, [_|T]) :- get_value(Elemento, T).

% Empacar una lista de objectos a la forma "id:valor"
%  Attr			Attributo del valor de buscar
%  Value		Valor defaulto
%  ...			Objecto al head
%  Insts		Lista de objectos ya en la nueva forma, pasa [] al inicio
%  Insts_new	Lista de objectos de devolver
package(Attr, Value, [[id => Name,props => Props,_]| T], Insts, Insts_New) :-
	package(Attr, Value, [[id => Name,Props,_]| T], Insts, Insts_New).
package(Attr, Value, [[id => Name,Props,_]| T], Insts, Insts_New) :-
	get_value(Attr => ValueNew, Props), % buscar para propiedad
	append(Insts, [Name:ValueNew], Insts_Tmp), % existe, sobreescribir default
	package(Attr, Value, T, Insts_Tmp, Insts_New) % empacar
	; 
	append(Insts, [Name:Value], Insts_Tmp), % no existe, el default aplica
	package(Attr, Value, T, Insts_Tmp, Insts_New). % empacar
package(_, _, [], Insts, Insts).

% Propagar un attributo y un valor a todos los instancias en el árbol bajo
% una clase especifica.
%
%  Attr				Attributo del valor de buscar
%  Value			Valor defaulto
%  NomClaseMadre	Buscar clases con Madre con este nombre
%  Results			Resultados de devolver
%  KB_Original		KB
%  ...				Clase al head
ep_madre(Attr, Value, NomClaseMadre, Results, KB_Original, [class(NomClase,NomClaseMadre,Props,_,Insts_A)|T]) :-
	get_value(Attr => ValueNew, Props), % buscar para propiedad
	package(Attr, ValueNew, Insts_A, [], Results_A), % existe, actualizar valor
	% seguir escaneando lateralmente, pero con el valor original
	ignore(ep_madre(Attr, Value, NomClaseMadre, Results_B, KB_Original, T)),
	append(Results_A, Results_B, Results_C), % agregar resultos
	% seguir escaneando en profundidad, pero ya con el valor nuevo
	ignore(ep_madre(Attr, ValueNew, NomClase, Results_D, KB_Original, KB_Original)), % depth scan with new value
	append(Results_C, Results_D, Results) % agregar resultos
	;
	% este clase no tiene ninguna valor por ese attributo definido
	package(Attr, Value, Insts_A, [], Results_A),
	% seguir escaneando lateralmente con el valor original
	ignore(ep_madre(Attr, Value, NomClaseMadre, Results_B, KB_Original, T)),
	append(Results_A, Results_B, Results_C), % agregar resultos
	% seguir escaneando en profundidad con el valor original
	ignore(ep_madre(Attr, Value, NomClase, Results_D, KB_Original, KB_Original)), % depth scan with same value
	append(Results_C, Results_D, Results). % agregar resultos
ep_madre(Attr, Value, NomClaseMadre, Exts, KB_Original, [_|T]) :- ep_madre(Attr, Value, NomClaseMadre, Exts, KB_Original, T).
ep_madre(_, _, _, [], _, []).

ep_objecto(Attr, [[id=>Name,props => Props,_]|T], Results, Results_New) :-
	ep_objecto(Attr, [[id=>Name,Props,_]|T], Results, Results_New).
ep_objecto(Attr, [[id=>Name,Props,_]|T], Results, Results_New) :-
	get_value(Attr => Value, Props), % buscar para propiedad
	append(Results, [Name:Value], Results_A),
	ep_objecto(Attr, T, Results_A, Results_New) % seguir buscando por más que tiene la relación
	; 
	ep_objecto(Attr, T, Results, Results_New).
ep_objecto(_, [], Results, Results).

% Buscar desde el inicio del árbol para las clases más arribas que tienen
%  el attributo dado especificado
%
%  NomClaseMadre	Buscar clases con Madre con este nombre
%  Attr				Attributo del valor de buscar
%  Results			Resultados de devolver
%  KB_Original		KB
%  ...				Clase al head
ep_find_root(NomClaseMadre, Attr, Results, KB_Original, [class(NomClase,NomClaseMadre,Props,_,Insts_A)|T]) :-
	get_value(Attr => Value, Props), % buscar para propiedad
	package(Attr, Value, Insts_A, [], Results_A), % existe, applicarlo a instancias
	% seguir escaneando lateralmente
	ignore(ep_find_root(NomClaseMadre, Attr, Results_B, KB_Original, T)),
	append(Results_A, Results_B, Results_C),
	% propagar ese valor encontrado a todos los hijos de este clase también
	ignore(ep_madre(Attr, Value, NomClase, Results_D, KB_Original, KB_Original)), % propogate in depth scan
	append(Results_C, Results_D, Results)
	; 
	% uno de los objectos puede tener la propiedad
	ep_objecto(Attr, Insts_A, [], Results_A),
	% si la propiedad no fue encontrado, seguir buscando lateralmente
	ep_find_root(NomClaseMadre, Attr, Results_B, KB_Original, T),
	append(Results_A, Results_B, Results_C),
	% y seguir buscando en profundidad también. nota que cuando se encuentra
	% una clase con la propiedad, ya convierte a propagando en profundidad
	% en vez de escaneando
	ep_find_root(NomClase, Attr, Results_D, KB_Original, KB_Original),
	append(Results_C, Results_D, Results).
ep_find_root(NomClaseMadre, Attr, Results, KB_Original, [_|T]) :- ep_find_root(NomClaseMadre, Attr, Results, KB_Original, T).
ep_find_root(_, _, [], _, []).

filter_not([_:not(_)|T], R) :- filter_not(T, R).
filter_not([H|T], [H|R]) :- filter_not(T, R).
filter_not([], []).

select_yes([Attr:yes|T], [Attr:yes|R]) :- select_yes(T, R).
select_yes([_|T], R) :- select_yes(T,R).
select_yes([], []).

% sobreescribir todos los valores para que sean yes
%overwrite([Attr : _|T], [Attr : yes|R]) :- overwrite(T, R).
%overwrite([], []).

extension_propiedad(not(Attr), Results, KB_Original) :- 
	ep_find_root(top, not(Attr), Results_A, KB_Original, KB_Original),
	select_yes(Results_A, Results_B),
	filter_not(Results_B, Results).
extension_propiedad(Attr, Results, KB_Original) :- 
	ep_find_root(top, Attr, Results_A, KB_Original, KB_Original),
	filter_not(Results_A, Results).

%****************************************************************
% 1c. Extensiones de una relación
%
% Usage: extension_relación(Typ, Extensiones, KnowledgeBase)
%****************************************************************

get_value_strict(not(Attr) => Value, [not(Attr => Value)|_]).
get_value_strict(Attr => Value, [Attr => Value|_]).
get_value_strict(Elemento, [_|T]) :- get_value_strict(Elemento, T).

% Buscar por la extension de una clase. Si ningun objectos existen,
%  considerar la clase una objecto.
%extension_or_object(not(Algo), not(Insts), KB_Original) :-
%	extension_or_object(Algo, Insts, KB_Original).
extension_or_object(NomClase, Insts, KB_Original) :-
	extension_class(NomClase, Subjs, KB_Original),
	not_empty(Subjs),
	append(Subjs, [], Insts)
	; append([], [NomClase], Insts).

% Presentar unos objectos con sus relaciones resolvados. Si el objecto
%  tiene algo más especifico, utilizar eso en vez.
%
%  Attr			Nombre de la relación
%  ...			Lista de objectos quienes tienen esa relación
%  Subjs		Lista de sujectos de la relación
%  Results_New	Lista formatado como [ name :[ ... ] ]
%  KB_Original	Knowledge base
package_relation(Attr, [[id=>Name,_,rels => Rels]|T], Subjs, Results, Results_New, KB_Original) :-
	package_relation(Attr, [[id=>Name,_,Rels]|T], Subjs, Results, Results_New, KB_Original).
package_relation(Attr, [[id=>Name,_,Rels]|T], Subjs, Results, Results_New, KB_Original) :-
	get_value_strict(Attr => Value, Rels), % buscar si objecto especifica una relación
	extension_or_object(Value, SubjsNew, KB_Original),
	append(Results, [Name:SubjsNew], Results_A),
	package_relation(Attr, T, Subjs, Results_A, Results_New, KB_Original)
	;
	append(Results, [Name:Subjs], Results_A), % ningún más especifica existe
	package_relation(Attr, T, Subjs, Results_A, Results_New, KB_Original).
package_relation(_, [], _, Results, Results, _).

% Buscar clases que tienen como madre una otra clase. Si esa nueva clase 
%  tiene definido la relación que estamos buscando, devolver los instancias
%  junto con el valor de esa relación. O si no, devolver los instancias con
%  el valor que tiene la madre.
%
%  Attr				Nombre de la relación
%  Valor			Valor que tiene la madre por este relación
%  NomClaseMadre	Nombre de la clase madre
%  Results			Instancias con sus relaciones
%  KB_Original		Knowledge base
%  ...				Lista de objectos quienes tienen esa relación
er_madre(Attr, Value, NomClaseMadre, Results, KB_Original, [class(NomClase,NomClaseMadre,_,Rels,Insts)|T]) :-
	get_value_strict(Attr => ValueNew, Rels), % buscar para propiedad
	extension_or_object(ValueNew, Subjs, KB_Original),
	% seleccionar la nueva relación
	package_relation(Attr, Insts, Subjs, [], Results_A, KB_Original),
	% buscar en longitud primero
	ignore(er_madre(Attr, Value, NomClaseMadre, Results_B, KB_Original, T)),
	append(Results_A, Results_B, Results_C), % agregar resultos
	% luego en profundidad
	ignore(er_madre(Attr, ValueNew, NomClase, Results_D, KB_Original, KB_Original)), % depth scan with new value
	append(Results_C, Results_D, Results) % agregar resultos
	;
	package_relation(Attr, Insts, Value, [], Results_A, KB_Original),
	% buscar en longitud primero
	ignore(er_madre(Attr, Value, NomClaseMadre, Results_B, KB_Original, T)),
	append(Results_A, Results_B, Results_C), % agregar resultos
	% luego en profundidad
	ignore(er_madre(Attr, Value, NomClase, Results_D, KB_Original, KB_Original)), % depth scan with same value
	append(Results_C, Results_D, Results). % agregar resultos
er_madre(Attr, Value, NomClaseMadre, Exts, KB_Original, [_|T]) :- er_madre(Attr, Value, NomClaseMadre, Exts, KB_Original, T).
er_madre(_, _, _, [], _, []).

% verdad syss no es vacío
%not_empty([_|_]).

% Buscar un objecto o instancia solito que tiene definido la relación
%
%  Attr				Nombre de la relación
%  ...				Lista de objectos para buscar
%  ...				Formato para regresar
%  KB_Original		Knowledge base
er_objecto(Attr, [[id=>Name,_,rels => Rels]|T], Results, Results_New, KB_Original) :-
	er_objecto(Attr, [[id=>Name,_,Rels]|T], Results, Results_New, KB_Original).
er_objecto(Attr, [[id=>Name,_,Rels]|T], Results, Results_New, KB_Original) :-
	get_value_strict(Attr => Value, Rels), % buscar para relacion
	extension_or_object(Value, Subjs, KB_Original), % tiene la relación
	append(Results, [Name:Subjs], Results_A),
	er_objecto(Attr, T, Results_A, Results_New, KB_Original) % seguir buscando por más que tiene la relación
	; 
	er_objecto(Attr, T, Results, Results_New, KB_Original). % no tiene la relación
er_objecto(_, [], Results, Results, _).

% Buscar desde el inicio del árbol para las clases más arribas que tienen
%  la relación dado especificado
%
%  NomClaseMadre	Buscar clases con Madre con este nombre
%  Attr				Attributo del valor de buscar
%  Results			Resultados de devolver
%  KB_Original		KB
%  ...				Clase al head
er_find_root(NomClaseMadre, Attr, Results, KB_Original, [class(NomClase,NomClaseMadre,_,Rels,Insts)|T]) :-
	get_value_strict(Attr => Value, Rels), % buscar para propiedad
	extension_or_object(Value, Subjs, KB_Original),
	% formatar la relación
	package_relation(Attr, Insts, Subjs, [], Results_A, KB_Original),
	% seguir buscando en longitud primero
	ignore(er_find_root(NomClaseMadre, Attr, Results_B, KB_Original, T)),
	append(Results_A, Results_B, Results_C),
	% luego en profundidad
	ignore(er_madre(Attr, Subjs, NomClase, Results_D, KB_Original, KB_Original)), % propogate in depth scan
	append(Results_C, Results_D, Results)
	; 
	% si no tiene la propiedad definido, todavía una de sus objectos puede
	% tenerlo. así que busar entre sus objectos
	ignore(er_objecto(Attr, Insts, [], Results_A, KB_Original)),
	% seguir buscando en longitud primero
	ignore(er_find_root(NomClaseMadre, Attr, Results_B, KB_Original, T)),
	append(Results_A, Results_B, Results_C),
	% luego en profundidad
	er_find_root(NomClase, Attr, Results_D, KB_Original, KB_Original),
	append(Results_C, Results_D, Results).
er_find_root(NomClaseMadre, Attr, Results, KB_Original, [_|T]) :- er_find_root(NomClaseMadre, Attr, Results, KB_Original, T).
er_find_root(_, _, [], _, []).

% empezar desde top
extension_relacion(Attr, Results, KB_Original) :- er_find_root(top, Attr, Results, KB_Original, KB_Original).

%****************************************************************
% 1d. Todas las clases a las que pertenece un objeto/instancia.
%
% Usage: class_inst(Id, Clases_Inst, KnowledgeBase)
%****************************************************************

%Buscar objeto/instancia que coincide con el id

%Encontró objeto/instancia que coincide con el id.
eo(Id, NomClase, [class(NomClase,_,_,_,[[_=>Id,_,_]|_])|_]).

%Buscar en el siguiente objeto/instancia de la misma clase.
eo(Id, NomClase, [class(NomClase,NomClaseMadre,_,_,[_|T])|T1]):-
	eo(Id, NomClase, [class(NomClase,NomClaseMadre,_,_,T)|T1]).

%Seguir buscando en las demás clases.
eo(Id, NomClase, [_|T]):- 
	eo(Id, NomClase, T).

%Encontró la clase madre y la mete a la lista.
ec_objeto(NomClase, KB_Original, [class(NomClase,NomClaseMadre,_,_,_)|_], Clases_Base, Clases_Insts):-
	append(Clases_Base,[NomClase],Clases_Raw),
	ec_objeto(NomClaseMadre, KB_Original, KB_Original, Clases_Raw, Clases_Insts).

%Seguir buscando la clase.
ec_objeto(NomClase, KB_Original, [_|T], Clases_Base, Clases_Insts):- 
	ec_objeto(NomClase, KB_Original, T, Clases_Base, Clases_Insts).

%Llegó a la clase top, caso base. 
ec_objeto(top,_,_,Clases_Raw,Clases_Insts):-
	append(Clases_Raw,[top],Clases_Insts).

class_inst(Id, Clases_Inst, KB_Original) :- 
	(eo(Id, NomClase, KB_Original), ec_objeto(NomClase, KB_Original, KB_Original, [], Clases_Inst)) ; Clases_Inst = unknown.

%****************************************************************
% 1e1. Todas las propiedades de un objeto/instancia.
%
% Usage: props_inst(Id, Props_Inst, KnowledgeBase)
%****************************************************************

attr_match(not(Attr), Lista) :- attr_match(Attr, Lista).
attr_match(Attr => _, Lista) :- attr_match(Attr, Lista).
attr_match(Attr, [Attr => _|_]).
attr_match(Attr, [not(Attr)|_]).
attr_match(Attr, [Attr|_]).
attr_match(Attr, [_|T]) :- attr_match(Attr, T).

merge_safe([Attr|T], Lista, Results) :- 
	attr_match(Attr, Lista),
	merge_safe(T, Lista, Results)
	;
	append(Lista, [Attr], Results_A),
	merge_safe(T, Results_A, Results).
merge_safe([], Results, Results).

%Buscar objeto/instancia que coincida con el id
eop(Id, Props_Inst, NomClase, [class(NomClase,_,_,_,[[id=>Id,Props_Inst,_]|_])|_]).

eop(Id, Props_Inst, NomClase, [class(NomClase,_,_,_,[_|T])|T1]):-
	eop(Id, Props_Inst, NomClase, [class(NomClase,_,_,_,T)|T1]).

%Seguir buscando si no coincide
eop(Id, Props_Inst, NomClase, [_|T]):-
	eop(Id, Props_Inst, NomClase, T).

ec_objetoP(NomClase, _, [class(NomClase,top,Props,_,_)|_], Props_Base, Props_Insts):-
	merge_safe(Props,Props_Base,Props_Insts).

%Encontró la clase madre y mete sus propiedades a la lista.
ec_objetoP(NomClase, KB_Original, [class(NomClase,NomClaseMadre,Props,_,_)|_], Props_Base, Props_Insts):-
	merge_safe(Props,Props_Base,Props_Raw),
	ec_objetoP(NomClaseMadre, KB_Original, KB_Original, Props_Raw, Props_Insts).

%Seguir buscando la clase madre.
ec_objetoP(NomClase, KB_Original, [_|T], Props_Base, Props_Insts):- 
	ec_objetoP(NomClase, KB_Original, T, Props_Base, Props_Insts).

props_inst(Id, Props_Inst, KB_Original) :- 	
	(eop(Id, Props_Base, NomClase, KB_Original), ec_objetoP(NomClase, KB_Original, KB_Original, Props_Base, Props_Inst)) ; Props_Inst = unknown.

%****************************************************************
% 1e2. Todas las propiedades de una clase.
%
% Usage: props_class(NomClase, Props_Class, KnowledgeBase)
%****************************************************************

epc(NomClase, _, [class(NomClase,top,Props,_,_)|_], Props_Base, Props_Insts):-
	merge_safe(Props,Props_Base,Props_Insts).

%Encontró la clase madre y mete sus propiedades a la lista.
epc(NomClase, KB_Original, [class(NomClase,NomClaseMadre,Props,_,_)|_], Props_Base, Props_Insts):-
	merge_safe(Props,Props_Base,Props_Raw), 
	epc(NomClaseMadre, KB_Original, KB_Original, Props_Raw, Props_Insts).

%Seguir buscando la clase madre.
epc(NomClase, KB_Original, [_|T], Props_Base, Props_Insts):- 
	epc(NomClase, KB_Original, T, Props_Base, Props_Insts).

props_class(NomClase, Props_Class, KB_Original) :- 	
	(epc(NomClase, KB_Original, KB_Original, [], Props_Class)) ; Props_Class = unknown.

%****************************************************************
% 1f1. Todas las relaciones de un objeto/instancia.
%
% Usage: rels_inst(Id, Rels_Inst, KnowledgeBase)
%****************************************************************

%Buscar objeto/instancia que coincida con el id
eor(Id, Rels_Inst, NomClase, [class(NomClase,_,_,_,[[id=>Id,_,Rels_Inst]|_])|_]).

eor(Id, Rels_Inst, NomClase, [class(NomClase,_,_,_,[_|T])|T1]):-
	eor(Id, Rels_Inst, NomClase, [class(NomClase,_,_,_,T)|T1]).

%Seguir buscando si no coincide
eor(Id, Rels_Inst, NomClase, [_|T]):- 
	eor(Id, Rels_Inst, NomClase, T).

ec_objetoR(NomClase, _, [class(NomClase,top,_,Rels,_)|_], Rels_Base, Rels_Insts):-
	merge_safe(Rels,Rels_Base,Rels_Insts).

%Encontró la clase madre y mete sus relaciones a la lista.
ec_objetoR(NomClase, KB_Original, [class(NomClase,NomClaseMadre,_,Rels,_)|_], Rels_Base, Rels_Insts):-
	merge_safe(Rels,Rels_Base,Rels_Raw), 
	ec_objetoR(NomClaseMadre, KB_Original, KB_Original, Rels_Raw, Rels_Insts).

%Seguir buscando la clase madre.
ec_objetoR(NomClase, KB_Original, [_|T], Rels_Base, Rels_Insts):- 
	ec_objetoR(NomClase, KB_Original, T, Rels_Base, Rels_Insts).

rels_inst(Id, Rels_Inst, KB_Original) :- 	
	(eor(Id, Rels_Base, NomClase, KB_Original), ec_objetoR(NomClase, KB_Original, KB_Original, Rels_Base, Rels_Inst)) ; Rels_Inst = unknown.

%****************************************************************
% 1f2. Todas las relaciones de una clase.
%
% Usage: rels_class(NomClase, Rels_Class, KnowledgeBase)
%****************************************************************

erc(NomClase, _, [class(NomClase,top,_,Rels,_)|_], Rels_Base, Rels_Class):-
	merge_safe(Rels,Rels_Base,Rels_Class).

%Encontró la clase madre y mete sus relaciones a la lista.
erc(NomClase, KB_Original, [class(NomClase,NomClaseMadre,_,Rels,_)|_], Rels_Base, Rels_Class):-
	merge_safe(Rels,Rels_Base,Rels_Raw), 
	erc(NomClaseMadre, KB_Original, KB_Original, Rels_Raw, Rels_Class).

%Seguir buscando la clase madre.
erc(NomClase, KB_Original, [_|T], Rels_Base, Rels_Class):- 
	erc(NomClase, KB_Original, T, Rels_Base, Rels_Class).

rels_class(NomClase, Rels_Class, KB_Original) :- 	
	(erc(NomClase, KB_Original, KB_Original, [], Rels_Class)) ; Rels_Class = unknown.

%****************************************************************
% 2a. Agrega una nueva clase vacia.
%****************************************************************

agrega_clase(NomClase,Madre,KB_Original,KB_Nuevo) :- append(KB_Original,[class(NomClase,Madre,[],[],[])],KB_Nuevo).

%****************************************************************
% 2a. Agrega un nuevo objeto a una clase
%****************************************************************

agrega_objeto_clase(NomClase,NomObjeto,[class(NomClase,Madre,Props,Rels,Insts)|T],[class(NomClase,Madre,Props,Rels,Insts_New)|T]) :- 
	append(Insts, [[id=>NomObjeto,[],[]]], Insts_New).
agrega_objeto_clase(NomClase,NomObjeto,[H|T],[H|R]) :- 
	agrega_objeto_clase(NomClase, NomObjeto, T, R).

%****************************************************************
% 2b. Agrega una nueva propiedad a una clase
%****************************************************************

% Usage: open_kb("KB_Original.txt", KB_Original), agrega_propiedad_class(animal, alfa, beta, KB_Original, KB_Nuevo).

% modificar elemento concordante, este forma la basis del abajo
% foo([alfa|T], [beta|T]).
% foo([H|T], [H|R]) :- foo(T,R).

% Propiedad debe estar en formato de atomo, atributo => valor, o not(atributo => valor)

agrega_propiedad_clase(NomClase,Propiedad,[class(NomClase,Madre,Props,Rels,Insts)|T],[class(NomClase,Madre,Props_New,Rels,Insts)|T]) :- 
	append(Props, [Propiedad], Props_New).
agrega_propiedad_clase(NomClase,Propiedad,[H|T],[H|R]) :- 
	agrega_propiedad_clase(NomClase, Propiedad, T, R).

%****************************************************************
% 2b. Agrega una nueva propiedad a un objeto 
%****************************************************************

agrega_propiedad_objeto(NomObjeto,Propiedad,KB_Original,KB_Nuevo) :-
	reemplaza_elemento(class(NomClase,Madre,Props,Rels,Insts),class(NomClase,Madre,Props,Rels,Insts_New),KB_Original,KB_Nuevo),
	verifica_elemento([id=>NomObjeto,PropsObjeto,RelsObjeto],Insts),
	reemplaza_elemento([id=>NomObjeto,PropsObjeto,RelsObjeto],[id=>NomObjeto,PropsObjeto_New,RelsObjeto],Insts,Insts_New),
	append(PropsObjeto,[Propiedad],PropsObjeto_New).

%****************************************************************
% 2c. Agrega una nueva relacion a una clase
%****************************************************************

% Relacion debe estar en formato de atom, atributo => valor, o not(atributo => valor)

agrega_relacion_clase(NomClase,Relacion,[class(NomClase,Madre,Props,Rels,Insts)|T],[class(NomClase,Madre,Props,Rels_New,Insts)|T]) :- 
	append(Rels, [Relacion], Rels_New).
agrega_relacion_clase(NomClase,Relacion,[H|T],[H|R]) :- 
	agrega_relacion_clase(NomClase, Relacion, T, R).

%****************************************************************
% 2c. Agrega una nueva relacion a un objeto
%****************************************************************

agrega_relacion_objeto(NomObjeto,Relacion,KB_Original,KB_Nuevo) :-
	reemplaza_elemento(class(NomClase,Madre,Props,Rels,Insts),class(NomClase,Madre,Props,Rels,Insts_New),KB_Original,KB_Nuevo),
	verifica_elemento([id=>NomObjeto,PropsObjeto,RelsObjeto],Insts),
	reemplaza_elemento([id=>NomObjeto,PropsObjeto,RelsObjeto],[id=>NomObjeto,PropsObjeto,RelsObjeto_New],Insts,Insts_New),
	append(RelsObjeto,[Relacion],RelsObjeto_New).

%****************************************************************
% 3a. Eliminar una clase
%****************************************************************

elimina_clase(NomClase,KB_Original,KB_Nuevo) :- elimina_elemento(class(NomClase,Madre,_,_,_),KB_Original,KB_Aux),
	actualiza_clase_madre(NomClase,Madre,KB_Aux,KB_Aux1),
	elimina_toda_relacion(NomClase,KB_Aux1,KB_Aux2),
	lista_de_objetos(NomClase,KB_Original,Lista),
	elimina_lista_objetos(Lista,KB_Aux2,KB_Nuevo).

%****************************************************************
% 3a. Eliminar un objeto
%****************************************************************

elimina_objeto(NomObjeto,KB_Original,KB_Nuevo) :-
	reemplaza_elemento(class(NomClase,Madre,Props,Rels,Insts),class(NomClase,Madre,Props,Rels,Insts_New),KB_Original,KB_Aux1),
	verifica_elemento([id=>NomObjeto,_,_],Insts),
	elimina_elemento([id=>NomObjeto,_,_],Insts,Insts_New),
	elimina_toda_relacion(NomObjeto,KB_Aux1,KB_Nuevo).
	
%****************************************************************
% 3b. Eliminar una propiedad de una clase
%****************************************************************

elimina_propiedad_clase(NomClase,Propiedad,[class(NomClase,Madre,Props,Rels,Insts)|T],[class(NomClase,Madre,Props_New,Rels,Insts)|T]) :- 
	elimina_elemento(Propiedad, Props, Props_New).
elimina_propiedad_clase(NomClase,Propiedad,[H|T],[H|R]) :- 
	elimina_propiedad_clase(NomClase, Propiedad, T, R).

%****************************************************************
% 3b. Eliminar una propiedad de un objeto
%****************************************************************

elimina_propiedad_objeto(NomObjeto,Propiedad,KB_Original,KB_Nuevo) :-
	reemplaza_elemento(class(NomClase,Madre,Props,Rels,Insts),class(NomClase,Madre,Props,Rels,Insts_New),KB_Original,KB_Nuevo),
	verifica_elemento([id=>NomObjeto,PropsObjeto,RelsObjeto],Insts),
	reemplaza_elemento([id=>NomObjeto,PropsObjeto,RelsObjeto],[id=>NomObjeto,PropsObjeto_New,RelsObjeto],Insts,Insts_New),
	elimina_elemento(Propiedad,PropsObjeto,PropsObjeto_New).

%****************************************************************
% 3c. Eliminar una relacion de una clase
%****************************************************************

elimina_relacion_clase(NomClase,Relacion,[class(NomClase,Madre,Props,Rels,Insts)|T],[class(NomClase,Madre,Props,Rels_New,Insts)|T]) :- 
	elimina_elemento(Relacion, Rels, Rels_New).
elimina_relacion_clase(NomClase,Relacion,[H|T],[H|R]) :- 
	elimina_relacion_clase(NomClase, Relacion, T, R).

%****************************************************************
% 3c. Eliminar una relación de un objeto
%****************************************************************

elimina_relacion_objeto(NomObjeto,Relacion,KB_Original,KB_Nuevo) :-
	reemplaza_elemento(class(NomClase,Madre,Props,Rels,Insts),class(NomClase,Madre,Props,Rels,Insts_New),KB_Original,KB_Nuevo),
	verifica_elemento([id=>NomObjeto,PropsObjeto,RelsObjeto],Insts),
	reemplaza_elemento([id=>NomObjeto,PropsObjeto,RelsObjeto],[id=>NomObjeto,PropsObjeto,RelsObjeto_New],Insts,Insts_New),
	elimina_elemento(Relacion,RelsObjeto,RelsObjeto_New).

%****************************************************************
% 4a. Modificar nombre de una clase
%****************************************************************

modifica_nombre_clase(NomClase,NomClase_New,KB_Original,KB_Nuevo):-
	reemplaza_elemento(class(NomClase,Madre,Props,Rels,Insts),class(NomClase_New,Madre,Props,Rels,Insts),KB_Original,KB_Aux),
	actualiza_clase_madre(NomClase,NomClase_New,KB_Aux,KB_Aux1),
	actualiza_toda_relacion(NomClase,NomClase_New,KB_Aux1,KB_Nuevo).

%****************************************************************
% 4a. Modificar nombre de un objeto
%****************************************************************

modifica_nombre_objeto(NomObjeto,NomObjeto_New,KB_Original,KB_Nuevo) :-
	reemplaza_elemento(class(NomClase,Madre,Props,Rels,Insts),class(NomClase,Madre,Props,Rels,Insts_New),KB_Original,KB_Aux),
	verifica_elemento([id=>NomObjeto|T],Insts),
	reemplaza_elemento([id=>NomObjeto|T],[id=>NomObjeto_New|T],Insts,Insts_New),
	actualiza_toda_relacion(NomObjeto,NomObjeto_New,KB_Aux,KB_Nuevo).

%****************************************************************
% 4b. Modificar una propiedad de una clase
%****************************************************************

modifica_propiedad_clase(NomClase,Propiedad,Propiedad_New,KB_Original,KB_Nuevo) :-
	elimina_propiedad_clase(NomClase,Propiedad,KB_Original,KB_Aux),
	agrega_propiedad_clase(NomClase,Propiedad_New,KB_Aux,KB_Nuevo).

%****************************************************************
% 4b. Modificar la propiedad de un objeto
%****************************************************************

modifica_propiedad_objeto(NomObjeto,Propiedad,Propiedad_New,KB_Original,KB_Nuevo) :-
	elimina_propiedad_objeto(NomObjeto,Propiedad,KB_Original,KB_Aux),
	agrega_propiedad_objeto(NomObjeto,Propiedad_New,KB_Aux,KB_Nuevo).

%****************************************************************
% 4c. Modificar una relación de una clase
%****************************************************************

modifica_relacion_clase(NomClase,Relacion,Relacion_New,KB_Original,KB_Nuevo) :-
		elimina_relacion_clase(NomClase,Relacion,KB_Original,KB_Aux),
		agrega_relacion_clase(NomClase,Relacion_New,KB_Aux,KB_Nuevo).
	
%****************************************************************
% 4c. Modificar la relación de un objeto
%****************************************************************

modifica_relacion_objeto(NomObjeto,Relacion,Relacion_New,KB_Original,KB_Nuevo) :-
	elimina_relacion_objeto(NomObjeto,Relacion,KB_Original,KB_Aux),
	agrega_relacion_objeto(NomObjeto,Relacion_New,KB_Aux,KB_Nuevo).
	
%****************************************************************
% Verifica que un elemento sea parte de una lista
%****************************************************************

verifica_elemento(Elemento,[Elemento|_]).
verifica_elemento(Elemento,[_|T]):-
	verifica_elemento(Elemento,T).

%****************************************************************
% Cambia un elemento A por otro elemento B en una lista
%****************************************************************

reemplaza_elemento(_,_,[],[]).
reemplaza_elemento(Elemento_A,Elemento_B,[Elemento_A|T],[Elemento_B|R]):-
	reemplaza_elemento(Elemento_A,Elemento_B,T,R).
reemplaza_elemento(Elemento_A,Elemento_B,[H|T],[H|R]):-
	reemplaza_elemento(Elemento_A,Elemento_B,T,R).

%****************************************************************
% Actualizar valor de un elemento en una lista
%****************************************************************

actualiza_valor(Prop, Valor, [Prop => _|T], [Prop => Valor|T]).
actualiza_valor(Prop, Valor, [H|T], [H|R]) :- actualiza_valor(Prop, Valor, T,R).

%****************************************************************
% Eliminar un elemento de una lista
%****************************************************************

elimina_elemento(Elemento, [Elemento|T], T).
elimina_elemento(Elemento, [H|T], [H|R]) :- elimina_elemento(Elemento, T,R).

%****************************************************************
% Actualiza la madre de de las clases hijas
%****************************************************************

actualiza_clase_madre(_,_,[],[]).
actualiza_clase_madre(NomClaseMadre_Actual,NomClaseMadre_New,[class(NomClase,NomClaseMadre_Actual,Props,Rels,Insts)|T],[class(NomClase,NomClaseMadre_New,Props,Rels,Insts)|R]):-
	actualiza_clase_madre(NomClaseMadre_Actual,NomClaseMadre_New,T,R).
actualiza_clase_madre(NomClaseMadre_Actual,NomClaseMadre_New,[H|T],[H|R]):-
	actualiza_clase_madre(NomClaseMadre_Actual,NomClaseMadre_New,T,R).

%****************************************************************
% Elimina todo tipo de relacion entre clases y objetos
%****************************************************************

elimina_toda_relacion(_,[],[]).
elimina_toda_relacion(Elemento,[class(NomClase,Madre,Props,Rels,Insts)|T],[class(NomClase,Madre,Props,Rels_New,Insts_New)|R]):-
	elimina_relacion(Elemento,Rels,Rels_New),
	elimina_relaciones_de_objetos(Elemento,Insts,Insts_New),
	elimina_toda_relacion(Elemento,T,R).

elimina_relaciones_de_objetos(_,[],[]).
elimina_relaciones_de_objetos(Elemento,[[id=>Objeto,PropsObjeto,RelsObjeto]|T],[[id=>Objeto,PropsObjeto,RelsObjeto_New]|R]):-
	elimina_relacion(Elemento,RelsObjeto,RelsObjeto_New),
	elimina_relaciones_de_objetos(Elemento,T,R).

elimina_relacion(_,[],[]).
elimina_relacion(Elemento,[_=>Elemento|T],R):-
	elimina_relacion(Elemento,T,R).
elimina_relacion(Elemento,[not(_=>Elemento)|T],R):-
	elimina_relacion(Elemento,T,R).
elimina_relacion(Elemento,[H|T],[H|R]):-
	elimina_relacion(Elemento,T,R).
	
%****************************************************************
% Actualiza todo tipo de relacion entre clases y objetos
%****************************************************************

actualiza_toda_relacion(_,_,[],[]).
actualiza_toda_relacion(Elemento,Elemento_Nuevo,[class(NomClase,Madre,Props,Rels,Insts)|T],[class(NomClase,Madre,Props,Rels_New,Insts_New)|R]):-
	actualiza_relacion(Elemento,Elemento_Nuevo,Rels,Rels_New),
	actualiza_relaciones_de_objetos(Elemento,Elemento_Nuevo,Insts,Insts_New),
	actualiza_toda_relacion(Elemento,Elemento_Nuevo,T,R).
	
actualiza_relaciones_de_objetos(_,_,[],[]).
actualiza_relaciones_de_objetos(Elemento,Elemento_Nuevo,[[id=>NomObjeto,PropsObjeto,RelsObjeto]|T],[[id=>NomObjeto,PropsObjeto,RelsObjeto_New]|R]):-
	actualiza_relacion(Elemento,Elemento_Nuevo,RelsObjeto,RelsObjeto_New),
	actualiza_relaciones_de_objetos(Elemento,Elemento_Nuevo,T,R).

actualiza_relacion(_,_,[],[]).
actualiza_relacion(Valor,Valor_Nuevo,[Atributo=>Valor|T],[Atributo=>Valor_Nuevo|R]):-
	actualiza_relacion(Valor,Valor_Nuevo,T,R).
actualiza_relacion(Valor,Valor_Nuevo,[not(Atributo=>Valor)|T],[not(Atributo=>Valor_Nuevo)|R]):-
	actualiza_relacion(Valor,Valor_Nuevo,T,R).
actualiza_relacion(Valor,Valor_Nuevo,[H|T],[H|R]):-
	actualiza_relacion(Valor,Valor_Nuevo,T,R).

%****************************************************************
% Elimina las relaciones de una lista de objetos
%****************************************************************

lista_de_objetos(_,[],[]).
lista_de_objetos(NomClase,[class(NomClase,_,_,_,Insts)|_],Insts_Lista):-
	nombre_de_objetos(Insts,Insts_Lista).
lista_de_objetos(NomClase,[_|T],Insts_Lista):-
	lista_de_objetos(NomClase,T,Insts_Lista).
	
nombre_de_objetos([],[]).
nombre_de_objetos([[id=>NomObjeto,_,_]|T],Insts_Lista):-
	nombre_de_objetos(T,Lista),append([NomObjeto],Lista,Insts_Lista).

elimina_lista_objetos([],KB_Original,KB_Original).
elimina_lista_objetos([NomObjeto|C],KB_Original,KB_Nuevo) :-
	elimina_lista_objetos(C,KB_Original,KB_Aux),elimina_toda_relacion(NomObjeto,KB_Aux,KB_Nuevo).
elimina_lista_objetos([NomObjeto|_],KB_Original,KB_Nuevo) :-
elimina_toda_relacion(NomObjeto,KB_Original,KB_Nuevo).

