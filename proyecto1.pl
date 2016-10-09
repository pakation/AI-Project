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
ec(_, [], _, []).

% Convertir una lista de objectos a una lista de sus ids
insts_ids([[id => Name,_,_]| T], Ids, Ids_New) :-
	append(Ids, [Name], Ids_Tmp),
	insts_ids(T, Ids_Tmp, Ids_New).
insts_ids([], Ids, Ids).

extension_class(NomClase, Ids, KB_Original) :-
	ec(NomClase, Insts_Raw, KB_Original, KB_Original),
	insts_ids(Insts_Raw, [], Ids).

%****************************************************************
% 1b. Extensiones de una propiedad
%
% Usage: extension_propiedad(Attr, ExtensionesConValorDeProp, KnowledgeBase)
%****************************************************************

% Recuperar un valor de una lista de propiedades, dado un attributo.
%  Evalua a falso si el valor no existe.
get_value(Attr => Value, [Attr => Value|_]).
get_value(Elemento, [_|T]) :- get_value(Elemento, T).

% Empacar una lista de objectos a la forma "id:valor"
%  Attr			Attributo del valor de buscar
%  Value		Valor defaulto
%  ...			Objecto al head
%  Insts		Lista de objectos ya en la nueva forma, pasa [] al inicio
%  Insts_new	Lista de objectos de devolver
package(Attr, Value, [[id => Name,props => Props,_]| T], Insts, Insts_New) :-
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

ep_objecto(Attr, [[id=>Name,props => Props,_]|T], [Name:Value]) :-
	get_value(Attr => Value, Props) % buscar para propiedad
	; ep_objecto(Attr, T, [Name:Value]).
ep_objecto(_, [], []).

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
	ep_objecto(Attr, Insts_A, Results_A),
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

extension_propiedad(Attr, Results, KB_Original) :- ep_find_root(top, Attr, Results, KB_Original, KB_Original).

%****************************************************************
% 1c. Extensiones de una relación
%
% Usage: extension_relación(Typ, Extensiones, KnowledgeBase)
%****************************************************************

% Buscar por la extension de una clase. Si ningun objectos existen,
%  considerar la clase una objecto.
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
package_relation(Attr, [[id=>Name,_,rels=>Rels]|T], Subjs, Results, Results_New, KB_Original) :-
	get_value(Attr => Value, Rels), % buscar para relación más especifica
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
	get_value(Attr => ValueNew, Rels), % buscar para propiedad
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
not_empty([_|_]).

% Buscar un objecto o instancia solito que tiene definido la relación
%
%  Attr				Nombre de la relación
%  ...				Lista de objectos para buscar
%  ...				Formato para regresar
%  KB_Original		Knowledge base
er_objecto(Attr, [[id=>Name,_,rels => Rels]|T], [Name:Subjs], KB_Original) :-
	get_value(Attr => Value, Rels), % buscar para relacion
	extension_or_object(Value, Subjs, KB_Original) % tiene la relación
	; er_objecto(Attr, T, [Name:Subjs], KB_Original). % no tiene la relación
er_objecto(_, [], [],_).

% Buscar desde el inicio del árbol para las clases más arribas que tienen
%  la relación dado especificado
%
%  NomClaseMadre	Buscar clases con Madre con este nombre
%  Attr				Attributo del valor de buscar
%  Results			Resultados de devolver
%  KB_Original		KB
%  ...				Clase al head
er_find_root(NomClaseMadre, Attr, Results, KB_Original, [class(NomClase,NomClaseMadre,_,Rels,Insts)|T]) :-
	get_value(Attr => Value, Rels), % buscar para propiedad
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
	er_objecto(Attr, Insts, Results_A, KB_Original),
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
	elimina_toda_relacion(NomClase,KB_Aux1,KB_Nuevo).

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
% 4b. Modificar valor de una propiedad de una clase
%****************************************************************

modifica_propiedad_clase(NomClase,Propiedad,Valor,[class(NomClase,Madre,Props,Rels,Insts)|T],[class(NomClase,Madre,Props_New,Rels,Insts)|T]) :- 
	actualiza_valor(Propiedad, Valor, Props, Props_New).
modifica_propiedad_clase(NomClase,Propiedad,Valor,[H|T],[H|R]) :- 
	modifica_propiedad_clase(NomClase, Propiedad, Valor, T, R).

%****************************************************************
% 4b. Modificar la propiedad de un objeto
%****************************************************************

modifica_propiedad_objeto(NomObjeto,Propiedad,Propiedad_New,KB_Original,KB_Nuevo) :-
	elimina_propiedad_objeto(NomObjeto,Propiedad,KB_Original,KB_Aux),
	agrega_propiedad_objeto(NomObjeto,Propiedad_New,KB_Aux,KB_Nuevo).

%****************************************************************
% 4c. Modificar el valor de una relacion para una clase
%****************************************************************

modifica_relacion_clase(NomClase,Propiedad,Valor,[class(NomClase,Madre,Props,Rels,Insts)|T],[class(NomClase,Madre,Props,Rels_New,Insts)|T]) :- 
	actualiza_valor(Propiedad, Valor, Rels, Rels_New).
modifica_relacion_clase(NomClase,Propiedad,Valor,[H|T],[H|R]) :- 
	modifica_relacion_clase(NomClase, Propiedad, Valor, T, R).
	
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

:- op(800,xfx,'=>').

