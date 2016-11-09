:- ensure_loaded(proyecto1).

% TODO initial diagnosis
% take all objects that the shopkeeper mentioned
% for each object, pair it with where it's supposed to be
% then generate mover,colocar pairs for each
% then merge colocars
%
% TODO normal dianosis
% > take initial diagnosis and observations
% < return new diagnosis
%
% for each item in the diagnosis, look in observations for any contradictions
% if one exists, remove the offending entry to construct a diagnosis free of errors.
% take this entry and, checking against observations, generate all possible alternatives
%
% if no more contradictions exist, take all offending entries and generate all
% posible permutations. for each, append the "clean" diagnosis, then compare the 
% number of items per shelf to the expected.
%
% choose the permutation that is the closest to expected
% generate move,colocar pairs for this permutation
