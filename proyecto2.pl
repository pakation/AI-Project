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

:- ensure_loaded(puntuacion).

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
not_empty([_|_]).

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

%****************************************************************
% MÓDULO PLANEACIÓN
%****************************************************************

equal(A,A).

%Función para regresar la reversa de una lista
reverse([],[]).
reverse([H|T],Z) :-
    reverse(T,Z1),
    append(Z1, [H], Z).

%Encuentra el objeto a entregar
deliver_obj([entregar(Objeto)|_], Objeto, PlanRaw, Plan):- 
	append(PlanRaw, [colocar(Objeto)], PlanRaw1),
	append(PlanRaw1, [mover(cliente)], PlanRaw2),
	append(PlanRaw2, [agarrar(Objeto)], PlanRaw3),
	append(PlanRaw3, [buscar(Objeto)], Plan).
deliver_obj([_|T], Objeto, PlanRaw, Plan):-
	deliver_obj(T, Objeto, PlanRaw, Plan).

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

%Encuentra el shelf en el que está el objeto
exists_in_shelf(Objeto, [Objeto=>Shelf_Objeto|_], Shelf_Objeto).
exists_in_shelf(Objeto, [_|T], Shelf_Objeto):-
	exists_in_shelf(Objeto, T, Shelf_Objeto).

take_decision(KB, Shelf_Objeto, [reacomodar(Objeto_Reacomodar)|T], Objeto_Reacomodar):- 
	should_be_in_shelf(Objeto_Reacomodar, Shelf_Objeto, KB)
	; 
	take_decision(KB, Shelf_Objeto, T, Objeto_Reacomodar).
take_decision(KB, Shelf_Objeto, [_|T], Objeto_Reacomodar):-
	take_decision(KB, Shelf_Objeto, T, Objeto_Reacomodar).

%Compara shelf actual con los demás reacomodos
%compare_shelves(Shelf_Objeto,  ShelfOriginal_Objeto, Objeto_Reacomodar, Decisiones, Diagnostico, KB_Original):- 
%	take_decision(Decisiones, Objeto_Reacomodar),

bottom_up(Shelf_Objeto, [], Diagnostico, Plan, Plan, KB_Original).
bottom_up(Shelf_Objeto, Decisiones, Diagnostico, PlanRaw, Plan, KB_Original):- 
	take_decision(KB_Original, Shelf_Objeto, Decisiones, Objeto_Reacomodar),
	append(PlanRaw, [colocar(Objeto_Reacomodar)], PlanRaw1), 
	exists_in_shelf(Objeto_Reacomodar, Diagnostico, ShelfOriginal_Objeto),
	append(PlanRaw1, [mover(Shelf_Objeto)], PlanRaw2),
	remove(Objeto_Reacomodar, Decisiones, [], Decisiones_New),
	bottom_up(ShelfOriginal_Objeto, Decisiones_New, Diagnostico, PlanRaw2, Plan, KB_Original)
	; 
	(
		eligir_proximo_objecto(KB_Original, Diagnostico, Shelf_Objeto, Decisiones, Mejor_Obj, Mejor_Shelf, Mejor_Punt)
		;
		% TODO wrap up the final tasks
		bottom_up(ShelfOriginal_Objeto, [], Diagnostico, PlanRaw, Plan, KB_Original)
	).

remove(_, [], Decisiones_New, Decisiones_New).
remove(Objeto, [reacomodar(Objeto)|T], Decisiones_A, Decisiones_New):- 
	remove(Objeto, T, Decisiones_A, Decisiones_New).
remove(Objeto, [H|T], Decisiones_A, Decisiones_New):-
	append(Decisiones_A, H, Decisiones_B),
	remove(Objeto, T, Decisiones_B, Decisiones_New).

planeacion(Decisiones, Diagnostico, PlanRaw, Plan, PosRobot, LeftArm, RightArm, KB_Original):-
	deliver_obj(Decisiones, Objeto, PlanRaw, Plan1), 
	exists_in_shelf(Objeto, Diagnostico, Shelf_Objeto),
	bottom_up(Shelf_Objeto, Decisiones, Diagnostico, Plan1, Plan2, KB_Original),
	reverse(Plan2,Plan),
	% TODO manda a calificar puntuacion
	evaluar(KB_Original, PosRobot, Plan, Puntos_Nuevo).
