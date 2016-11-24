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

:- op(800,xfx,'=>').

remove_generic(_, [], Set_New, Set_New).
remove_generic(Target, [Target|T], Set_A, Set_New):- 
	remove_generic(Target, T, Set_A, Set_New).
remove_generic(Target, [H|T], Set_A, Set_New):-
	append(Set_A, [H], Set_B),
	remove_generic(Target, T, Set_B, Set_New).

% verdad si el objecto está en un estante específico
is_on_shelf(Item, Shelf, [Item => Shelf|_]).
is_on_shelf(Item, Shelf, [_|T]) :- is_on_shelf(Item, Shelf, T).

:- ensure_loaded(proyecto1).

:- ensure_loaded(puntuacion).
:- ensure_loaded(diagnosis).
:- ensure_loaded(decision).
:- ensure_loaded(planeacion).

robot(KB, Diagnostico, AsistenteAcciones, Decision, Plan) :-
	diagnosis(KB, Diagnostico, AsistenteAcciones),
	decision(KB, shelf1, Diagnostico, Decision),
	planeacion2(KB, Decision, Diagnostico, shelf1, Plan).
