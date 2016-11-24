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
decision(KB,PosRobot,Diagnostico,Actividades):-extension_class(pending_activities,Entregas_pendientes,KB),
									  productos_a_reacomodar(KB, Diagnostico,Reacomodar),
									  eligir_conjunto(KB, Diagnostico, PosRobot, 15, Entregas_pendientes, Reacomodar, Actividades).
									  % lista_de_actividades(Reacomodar,Act),
									  % verifica_actividades(Act,Entregas_pendientes,Lista_act),
									  % concat_actividades(Entregas_pendientes,Lista_act,Actividades),!.

%%%%%%
% New
%%%%%%

eligir_conjunto(_, _, _, _, Set_New, [], Set_New).
eligir_conjunto(KB, Diagnostico, PosRobot, Umbral, Set, Reacomodas, Set_New) :-
	eligir_reacomoda(KB, Diagnostico, PosRobot, Set, Reacomodas, 0, High_New, _, [Item => Shelf]),
	(
		(High_New @> Umbral),
		% force stop
		eligir_conjunto(KB, Diagnostico, PosRobot, Umbral, Set, [], Set_New)
		;
		append(Set, [reacomodar(Item)], Set_B),
		remove_generic(Item => Shelf, Reacomodas, [], Reacomodas_B),
		eligir_conjunto(KB, Diagnostico, PosRobot, Umbral, Set_B, Reacomodas_B, Set_New)
	).

eligir_reacomoda(_, _, _, _, [], High_New, High_New, Choice_New, Choice_New).
eligir_reacomoda(KB, Diagnostico, PosRobot, Set, [Item => Shelf|T], High, High_New, Choice, Choice_New) :-
	append(Set, [reacomodar(Item)], Set_A),

	planeacion2(KB, Set_A, Diagnostico, PosRobot, Acciones),
	evaluar(KB, PosRobot, Acciones, Puntos),

	(
		(Puntos @> High),
		eligir_reacomoda(KB, Diagnostico, PosRobot, Set, T, Puntos, High_New, [Item => Shelf], Choice_New)
		; eligir_reacomoda(KB, Diagnostico, PosRobot, Set, T, High, High_New, Choice, Choice_New)
	).

%%%%%%
% End New
%%%%%%

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
obtiene_ubicacion_producto(_, [],[]).	
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

