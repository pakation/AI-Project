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

%agrega_propiedad_clase(NomClase,Propiedad,Valor,KB_Original,KB_Nuevo) :-
%	cambiaElemento(clase(NomClase,Madre,Props,Rels,Objetos),clase(NomClase,Madre,NProps,Rels,Objetos),KB_Original,KB_Nuevo),
%	agregar_propiedad(Props,Propiedad,Valor,NProps).

