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

:- op(800,xfx,'=>').

%****************************************************************
% 1a.
%****************************************************************

%****************************************************************
% 2a. Agrega una nueva class vacia.
%****************************************************************
agrega_class(NomClase,Madre,KB_Original,KB_Nuevo) :- append(KB_Original,[class(NomClase,Madre,[],[],[])],KB_Nuevo).

%****************************************************************
% 2a. Agrega un nuevo objeto a una class
%****************************************************************
agrega_objeto_class(NomClase,NomObjeto,[class(NomClase,Madre,Props,Rels,Insts)|T],[class(NomClase,Madre,Props,Rels,Insts_New)|T]) :- 
	append(Insts, [[id=>NomObjeto,[],[]]], Insts_New).
agrega_objeto_class(NomClase,NomObjeto,[H|T],[H|R]) :- 
	agrega_objeto_class(NomClase, NomObjeto, T, R).

%****************************************************************
% 2b. Agrega una nueva propiedad a una class
%****************************************************************

% Usage: open_kb("KB_Original.txt", KB_Original), agrega_propiedad_class(animal, alfa, beta, KB_Original, KB_Nuevo).

% modificar elemento concordante, este forma la basis del abajo
% foo([alfa|T], [beta|T]).
% foo([H|T], [H|R]) :- foo(T,R).

% Propiedad debe estar en formato de atomo, atributo => valor, o not(atributo => valor)

agrega_propiedad_class(NomClase,Propiedad,[class(NomClase,Madre,Props,Rels,Insts)|T],[class(NomClase,Madre,Props_New,Rels,Insts)|T]) :- 
	append(Props, [Propiedad], Props_New).
agrega_propiedad_class(NomClase,Propiedad,[H|T],[H|R]) :- 
	agrega_propiedad_class(NomClase, Propiedad, T, R).

%****************************************************************
% 2b. Agrega una nueva propiedad a un objeto 
%****************************************************************

agrega_propiedad_objeto(NomObjeto,Propiedad,KB_Original,KB_Nuevo) :-
	reemplaza_elemento(class(NomClase,Madre,Props,Rels,Insts),class(NomClase,Madre,Props,Rels,Insts_New),KB_Original,KB_Nuevo),
	verifica_elemento([id=>NomObjeto,PropsObjeto,RelsObjeto],Insts),
	reemplaza_elemento([id=>NomObjeto,PropsObjeto,RelsObjeto],[id=>NomObjeto,PropsObjeto_New,RelsObjeto],Insts,Insts_New),
	append(PropsObjeto,[Propiedad],PropsObjeto_New).

%****************************************************************
% 2c. Agrega una nueva relacion a una class
%****************************************************************

% Relacion debe estar en formato de atom, atributo => valor, o not(atributo => valor)

agrega_relacion_class(NomClase,Relacion,[class(NomClase,Madre,Props,Rels,Insts)|T],[class(NomClase,Madre,Props,Rels_New,Insts)|T]) :- 
	append(Rels, [Relacion], Rels_New).
agrega_relacion_class(NomClase,Relacion,[H|T],[H|R]) :- 
	agrega_relacion_class(NomClase, Relacion, T, R).

%****************************************************************
% 2c. Agrega una nueva relacion a un objeto
%****************************************************************

agrega_relacion_objeto(NomObjeto,Relacion,KB_Original,KB_Nuevo) :-
	reemplaza_elemento(class(NomClase,Madre,Props,Rels,Insts),class(NomClase,Madre,Props,Rels,Insts_New),KB_Original,KB_Nuevo),
	verifica_elemento([id=>NomObjeto,PropsObjeto,RelsObjeto],Insts),
	reemplaza_elemento([id=>NomObjeto,PropsObjeto,RelsObjeto],[id=>NomObjeto,PropsObjeto,RelsObjeto_New],Insts,Insts_New),
	append(RelsObjeto,[Relacion],RelsObjeto_New).

%****************************************************************
% Eliminar un elemento de una lista
%****************************************************************

elimina_elemento(Elemento, [Elemento|T], T).
elimina_elemento(Elemento, [H|T], [H|R]) :- elimina_elemento(Elemento, T,R).

%****************************************************************
% 3a. Eliminar una class
%****************************************************************

elimina_class(NomClase, [class(NomClase,_,_,_,_)|T], T).
elimina_class(NomClase, [H|T], [H|R]) :- elimina_class(NomClase, T,R).

%****************************************************************
% 3a. Eliminar un objeto
%****************************************************************

elimina_objeto(NomObjeto,KB_Original,KB_Nuevo) :-
	reemplaza_elemento(class(NomClase,Madre,Props,Rels,Insts),class(NomClase,Madre,Props,Rels,Insts_New),KB_Original,KB_Nuevo),
	verifica_elemento([id=>NomObjeto,_,_],Insts),
	elimina_elemento([id=>NomObjeto,_,_],Insts,Insts_New).

%****************************************************************
% 3b. Eliminar una propiedad de una class
%****************************************************************

elimina_propiedad_class(NomClase,Propiedad,[class(NomClase,Madre,Props,Rels,Insts)|T],[class(NomClase,Madre,Props_New,Rels,Insts)|T]) :- 
	elimina_elemento(Propiedad, Props, Props_New).
elimina_propiedad_class(NomClase,Propiedad,[H|T],[H|R]) :- 
	elimina_propiedad_class(NomClase, Propiedad, T, R).

%****************************************************************
% 3b. Eliminar una propiedad de un objeto
%****************************************************************

elimina_propiedad_objeto(NomObjeto,Propiedad,KB_Original,KB_Nuevo) :-
	reemplaza_elemento(class(NomClase,Madre,Props,Rels,Insts),class(NomClase,Madre,Props,Rels,Insts_New),KB_Original,KB_Nuevo),
	verifica_elemento([id=>NomObjeto,PropsObjeto,RelsObjeto],Insts),
	reemplaza_elemento([id=>NomObjeto,PropsObjeto,RelsObjeto],[id=>NomObjeto,PropsObjeto_New,RelsObjeto],Insts,Insts_New),
	elimina_elemento(Propiedad,PropsObjeto,PropsObjeto_New).

%****************************************************************
% 3c. Eliminar una relacion de una class
%****************************************************************

elimina_relacion_class(NomClase,Relacion,[class(NomClase,Madre,Props,Rels,Insts)|T],[class(NomClase,Madre,Props,Rels_New,Insts)|T]) :- 
	elimina_elemento(Relacion, Rels, Rels_New).
elimina_relacion_class(NomClase,Relacion,[H|T],[H|R]) :- 
	elimina_relacion_class(NomClase, Relacion, T, R).

%****************************************************************
% 3b. Eliminar una relación de un objeto
%****************************************************************

elimina_relacion_objeto(NomObjeto,Relacion,KB_Original,KB_Nuevo) :-
	reemplaza_elemento(class(NomClase,Madre,Props,Rels,Insts),class(NomClase,Madre,Props,Rels,Insts_New),KB_Original,KB_Nuevo),
	verifica_elemento([id=>NomObjeto,PropsObjeto,RelsObjeto],Insts),
	reemplaza_elemento([id=>NomObjeto,PropsObjeto,RelsObjeto],[id=>NomObjeto,PropsObjeto,RelsObjeto_New],Insts,Insts_New),
	elimina_elemento(Relacion,RelsObjeto,RelsObjeto_New).

%****************************************************************
% 4a. Modificar nombre de una class
%****************************************************************

modifica_nombre_class(NomClase,NomClase_New,[class(NomClase,Madre,Props,Rels,Insts)|T],[class(NomClase_New,Madre,Props,Rels,Insts)|T]).
modifica_nombre_class(NomClase,NomClase_New,[H|T],[H|R]) :- 
	modifica_nombre_class(NomClase, NomClase_New, T, R).

%****************************************************************
% 4a. Modificar nombre de un objeto
%****************************************************************

modifica_nombre_objeto(NomObjeto,NomObjeto_New,KB_Original,KB_Nuevo) :-
	reemplaza_elemento(class(NomClase,Madre,Props,Rels,Insts),class(NomClase,Madre,Props,Rels,Insts_New),KB_Original,KB_Nuevo),
	verifica_elemento([id=>NomObjeto|T],Insts),
	reemplaza_elemento([id=>NomObjeto|T],[id=>NomObjeto_New|T],Insts,Insts_New).
	
%****************************************************************
% Actualizar valor de un elemento en una lista
%****************************************************************

actualiza_valor(Prop, Valor, [Prop => _|T], [Prop => Valor|T]).
actualiza_valor(Prop, Valor, [H|T], [H|R]) :- actualiza_valor(Prop, Valor, T,R).

%****************************************************************
% 4b. Modificar valor de una propiedad de una class
%****************************************************************

modifica_propiedad_class(NomClase,Propiedad,Valor,[class(NomClase,Madre,Props,Rels,Insts)|T],[class(NomClase,Madre,Props_New,Rels,Insts)|T]) :- 
	actualiza_valor(Propiedad, Valor, Props, Props_New).
modifica_propiedad_class(NomClase,Propiedad,Valor,[H|T],[H|R]) :- 
	modifica_propiedad_class(NomClase, Propiedad, Valor, T, R).

%****************************************************************
% 4b. Modificar valor de una propiedad de un objeto
%****************************************************************

modifica_propiedad_objeto(NomObjeto,Propiedad,Propiedad_New,KB_Original,KB_Nuevo) :-
	elimina_propiedad_objeto(NomObjeto,Propiedad,KB_Original,KB_Aux),
	agrega_propiedad_objeto(NomObjeto,Propiedad_New,KB_Aux,KB_Nuevo).

%****************************************************************
% 4c. Modificar con quién mantiene una relación una class
%****************************************************************

modifica_relacion_class(NomClase,Propiedad,Valor,[class(NomClase,Madre,Props,Rels,Insts)|T],[class(NomClase,Madre,Props,Rels_New,Insts)|T]) :- 
	actualiza_valor(Propiedad, Valor, Rels, Rels_New).
modifica_relacion_class(NomClase,Propiedad,Valor,[H|T],[H|R]) :- 
	modifica_relacion_class(NomClase, Propiedad, Valor, T, R).
	
%****************************************************************
% 4c. Modificar con quién mantiene una relación un objeto
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
%--------------------------------------------------------------------------------------------------
%--------------------------------------------------------------------------------------------------
%--------------------------------------------------------------------------------------------------

%****************************************************************
% Verifica que una class exista en la base de conocimiento
%****************************************************************

%usage open_kb('ruta',KB),verifica_class(class,Resp,KB).

% verifica_class(_,'No sé',[]).
% verifica_class(NomClase,'No',[class(not(NomClase),_,_,_,_)|_]).
% verifica_class(NomClase,'Sí',[class(NomClase,_,_,_,_)|_]).
% verifica_class(NomClase,Resp,[_|T]):-
% 	verifica_class(NomClase,Resp,T).
% 
% extension_class_madre(NomClaseMadre, Exts, Exts_New, KB_Original, [class(NomClase,NomClaseMadre, _, _, Insts)|T]) :- 
% 	write(NomClase),
% 	extension_class(NomClase, Exts_A, Exts_B, KB_Original, KB_Original),
% 	extension_class_madre(NomClaseMadre, Exts_B, Exts_New, KB_Original, T).
% 
% extension_class_madre(NomClase, Exts, Exts_New, KB_Original, [_|T]) :- extension_class_madre(NomClase, Exts, Exts_New, KB_Original, T) ; append(Exts, [], Exts_New). 
% 
% extension_class(NomClase, Exts, Exts_New, KB_Original, [class(NomClase,_, _, _, Insts)|T]) :- 
% 	write(NomClase),
% 	append(Exts, Insts, Exts_A),
% 	extension_class_madre(NomClase, Exts_A, Exts_New, KB_Original, KB_Original).
% 
% extension_class(NomClase, Exts, Exts_New, KB_Original, [_|T]) :- extension_class(NomClase, Exts, Exts_New, KB_Original, T).

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
%****************************************************************

% Recuperar un valor dado un attributo y una lista de propiedades
get_value(Attr => Value, [Attr => Value|_]).
get_value(Elemento, [_|T]) :- get_value(Elemento, T).

% Dado un Valor, convertir instancias de la forma [id=>A,[],[], id=>B,[],[]]
% a [A:Valor, B:Valor]
%
% TODO si la instancia ya tiene un valor definido por el atributo dado,
% entonces utiliza eso en vez de Valor
package(Value, [[id => Name,_,_]| T], Insts, Insts_New) :-
	append(Insts, [Name:Value], Insts_Tmp),
	package(Value, T, Insts_Tmp, Insts_New).
package(_, [], Insts, Insts).

% Dado el nombre de un attributo, buscar por todos los clases (!) que tienen
% una propiedad con el mismo attributo. Luego asignar ese valor a todos 
% los instancias (!) de ese clase
%
% TODO por todos que tienen este NomClase como su Madre, package() sus
% instancias con el mismo valor. Si esas hijo-clases tienen una definición
% para este propiedad, cambia a esa valor.
%
% propagate_propiedad(NomClaseMadre, Attr, Valor, Results, KB_Original, [class(NomClase,NomClaseMadre,Props,_,Insts_A)|T]) :-
%	...
%
ec_propiedad(Attr, Results, KB_Original, [class(_,_,Props,_,Insts_A)|T]) :-
	get_value(Attr => Value, Props),
	package(Value, Insts_A, [], Results_A),
	ec_propiedad(Attr, Results_B, KB_Original, T),
	append(Results_A, Results_B, Results)
	; ec_propiedad(Attr, Results, KB_Original, T).
	% TODO
ec_propiedad(_, [], _, []).
	
ec_propiedad(_, Insts, KB_Original, [_|T]) :- ec(_, Insts, KB_Original, T).
% TODO
ec_propiedad(_, [], _, []).
