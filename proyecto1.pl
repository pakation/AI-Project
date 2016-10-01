%****************************************************************
%*PROYECTO    : REPRESENTACIÓN DEL CONOCIMIENTO                 *
%*INTEGRANTES : Cheung Derek                                    *
%*              González Rico Diana Virginia                    *
%*              Neri González José Francisco                    *
%*FECHA       : 11/Octubre/2016                                 *
%*DESCRIPCIÓN : Generar una Base de conocimeinto(KB) a partir   *
%*de la jerarquía conceptual con defaults y excepciones         * 
%*explicada en clase y de la especificación de la base de       *
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
% Agrega una nueva clase vacia.
%****************************************************************
agrega_clase(NomClase,Madre,KB_Original,KB_Nuevo) :- append(KB_Original,[clase(NomClase,Madre,[],[],[])],KB_Nuevo).

%****************************************************************
% Agrega una nueva propiedad a una clase
%****************************************************************

% Usage: open_kb("KB_Original.txt", KB_Original), agrega_propiedad_clase(animal, alfa, beta, KB_Original, KB_Nuevo).

% modificar elemento concordante, este forma la basis del abajo
% foo([alfa|T], [beta|T]).
% foo([H|T], [H|R]) :- foo(T,R).

% Propiedad debe estar en la forma de atom, propiedad => valor, o not(propiedad => valor)

agrega_propiedad_clase(NomClase,Propiedad,[clase(NomClase,Madre,Props,Rels,Insts)|T],[clase(NomClase,Madre,Props_New,Rels,Insts)|T]) :- 
	append(Props, [Propiedad], Props_New).
agrega_propiedad_clase(NomClase,Propiedad,[H|T],[H|R]) :- 
	agrega_propiedad_clase(NomClase, Propiedad, T, R).

%****************************************************************
% Agrega una nueva relacion a una clase
%****************************************************************

% Relacion debe estar en la forma de atom, propiedad => valor, o not(propiedad => valor)

agrega_relacion_clase(NomClase,Relacion,[clase(NomClase,Madre,Props,Rels,Insts)|T],[clase(NomClase,Madre,Props,Rels_New,Insts)|T]) :- 
	append(Rels, [Relacion], Rels_New).
agrega_relacion_clase(NomClase,Relacion,[H|T],[H|R]) :- 
	agrega_relacion_clase(NomClase, Relacion, T, R).

%****************************************************************
% Eliminar un elemento de una lista
%****************************************************************

elimina_elemento(Elemento, [Elemento|T], T).
elimina_elemento(Elemento, [H|T], [H|R]) :- elimina_elemento(Elemento, T,R).

%****************************************************************
% Eliminar una propiedad de una clase
%****************************************************************

elimina_propiedad_clase(NomClase,Propiedad,[clase(NomClase,Madre,Props,Rels,Insts)|T],[clase(NomClase,Madre,Props_New,Rels,Insts)|T]) :- 
	elimina_elemento(Propiedad, Props, Props_New).
elimina_propiedad_clase(NomClase,Propiedad,[H|T],[H|R]) :- 
	elimina_propiedad_clase(NomClase, Propiedad, T, R).

%****************************************************************
% Eliminar una relacion de una clase
%****************************************************************

elimina_relacion_clase(NomClase,Relacion,[clase(NomClase,Madre,Props,Rels,Insts)|T],[clase(NomClase,Madre,Props,Rels_New,Insts)|T]) :- 
	elimina_elemento(Relacion, Rels, Rels_New).
elimina_relacion_clase(NomClase,Relacion,[H|T],[H|R]) :- 
	elimina_relacion_clase(NomClase, Relacion, T, R).
